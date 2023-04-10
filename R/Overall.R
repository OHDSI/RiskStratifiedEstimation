# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of RiskStratifiedEstimation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# @author Observational Health Data Sciences and Informatics
# @author Alexandros Rekkas
# @author Peter Rijnbeek

#' Fit overall outcome model
#'
#' Fits outcome models within risk strata, estimating relative and absolute differences. Designed to be performed within
#' a parellelized analysis.
#'
#' @param outcomeId                  The outcome of interest for which the esitmation is performed. That is the outcome for
#'                                   which risk stratification is performed.
#' @param analysisSettings           An R object of type \code{analysisSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createAnalysisSettings}}.
#' @param getDataSettings            An R object of type \code{getDataSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createGetDataSettings}}.
#' @param runCmSettings              A parameter object of type \code{runCmSettingsArgs} defined using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createRunCmSettingsArgs}}

#' @return                        \code{NULL}. The results are all saved.
#'
fitOutcomeModelsOverall <- function(
  outcomeId,
  analysisSettings,
  getDataSettings,
  runCmSettings
) {
  ParallelLogger::logInfo(
    paste(
      "Calculating main results for outcome:",
      outcomeId
    )
  )

  ps <- readRDS(
    file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "Estimation",
      outcomeId,
      "psFull.rds"
    )
  )

  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = getDataSettings$cohortMethodDataFolder
  )

  if (runCmSettings$psMethod == "matchOnPs")
  {
    matchedPop <-  CohortMethod::matchOnPs(
      ps,
      caliper = runCmSettings$effectEstimationSettings$caliper,
      caliperScale = runCmSettings$effectEstimationSettings$caliperScale,
      maxRatio = runCmSettings$effectEstimationSettings$maxRatio,
      stratificationColumns = runCmSettings$effectEstimationSettings$stratificationColumns
    )

    outcomeModel <- CohortMethod::fitOutcomeModel(
      matchedPop,
      stratified = FALSE,
      modelType = "cox"
    )

    arr <- absoluteRiskReduction(
      matchedPop,
      timePoint = runCmSettings$timePoint,
      psMethod = "matchOnPs"
    )

    rrr <- relativeRiskReduction(
      outcomeModel
    )

  }
  else if (runCmSettings$psMethod == "stratifyByPs") {
    stratifiedPop <- CohortMethod::stratifyByPs(
      ps,
      numberOfStrata = runCmSettings$effectEstimationSettings$numberOfStrata,
      stratificationColumns = runCmSettings$effectEstimationSettings$stratificationColumns,
      baseSelection = runCmSettings$effectEstimationSettings$baseSelection
    )

    outcomeModel <- CohortMethod::fitOutcomeModel(
      stratifiedPop,
      stratified = TRUE,
      modelType = "cox"
    )

    arr <- absoluteRiskReduction(
      stratifiedPop,
      timePoint = runCmSettings$timePoint,
      psMethod = "stratifyByPs"
    )

    rrr <- relativeRiskReduction(
      outcomeModel
    )

  }

  overallResult <- list(
    absoluteRiskReduction = arr,
    relativeRiskReduction = rrr,
    outcomeModel = outcomeModel
  )

  saveDir <- file.path(
    analysisPath,
    "Estimation",
    outcomeId
  )

  saveRDS(
    overallResult,
    file = file.path(
      saveDir,
      'overallResult.rds'
    )
  )

  ParallelLogger::logInfo(
    'Saved the overall  results'
  )

  return(NULL)

}


includeOverallResults <- function(
  analysisSettings,
  getDataSettings,
  runSettings
) {

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  nThreads <- runSettings$runCmSettings$fitOutcomeModelsThreads

  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = file.path(
      getDataSettings$cohortMethodDataFolder
    )
  )

  cluster <- ParallelLogger::makeCluster(
    numberOfThreads = nThreads
  )
  ParallelLogger::clusterRequire(
    cluster,
    "RiskStratifiedEstimation"
  )
  ParallelLogger::clusterRequire(
    cluster,
    "CohortMethod"
  )

  ret <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = seq_along(runSettings$runCmSettings$analyses),
    fun = generateSingleAnalysis,
    analysisSettings = analysisSettings,
    runSettings = runSettings,
    analysisPath = analysisPath,
    cohortMethodData = cohortMethodData
  )

  ParallelLogger::stopCluster(cluster)

  combinedResults <- dplyr::bind_rows(ret) %>%
    dplyr::mutate(analysisId = analysisSettings$analysisId) %>%
    dplyr::relocate(analysisId, runLabel)

  rownames(combinedResults) <- NULL
  fileName <- "mappedOverallResults"
  saveRDS(
    object = combinedResults,
    file = file.path(
      analysisPath,
      "shiny",
      paste0(
        paste(
          fileName,
          analysisSettings$analysisId,
          sep = "_"
        ),
        ".rds"
      )
    )
  )

  ParallelLogger::logInfo("Saved overall results")

    incidenceList <- list()
    covariates <- cohortMethodData$covariates %>%
      dplyr::collect()
    data.table::setDT(covariates)

    covariateRef <- cohortMethodData$covariateRef %>%
      dplyr::collect()
    data.table::setDT(covariateRef)

    cohorts <- cohortMethodData$cohorts %>%
      dplyr::collect()
    data.table::setDT(cohorts)

    for (i in seq_along(runSettings$runCmSettings$analyses)) {
      currentRunCmAnalysis <- runSettings$runCmSettings$analyses[[i]]
      predictOutcome <- currentRunCmAnalysis$stratificationOutcome
      ps <- readRDS(
        file = file.path(
          analysisPath,
          "Estimation",
          currentRunCmAnalysis$label,
          predictOutcome,
          predictOutcome,
          paste0(
            paste(
              "psFull",
              currentRunCmAnalysis$label,
              sep = "_"
            ),
            ".rds"
          )
        )
      )
      incidenceList[[i]] <- computeIncidence(ps) %>%
        dplyr::mutate(
          analysisId = analysisSettings$analysisId,
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          outcomeId = predictOutcome,
          runLabel = currentRunCmAnalysis$label
        ) %>%
        dplyr::relocate(analysisId, runLabel)

      computePsDensity(ps) %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          outcomeId = predictOutcome,
          treatment = ifelse(
            treatment == 1,
            yes = analysisSettings$treatmentCohortId,
            no = analysisSettings$comparatorCohortId
          ),
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          runLabel = currentRunCmAnalysis$label
        ) %>%
        dplyr::relocate(analysisId, runLabel) %>%
        saveRDS(
          file = file.path(
            analysisPath,
            "shiny",
            paste(
              paste(
                "overall",
                "psDensity",
                analysisSettings$analysisId,
                currentRunCmAnalysis$label,
                sep = "_"
              ),
              "rds",
              sep = "."
            )
          )
        )

      ParallelLogger::logInfo(
        paste(
          "saved overall PS density for analysis:",
          currentRunCmAnalysis$label
        )
      )

      computeCovariateBalance(
        population = ps,
        cohorts = cohorts,
        covariates = covariates,
        covariateRef = covariateRef
      ) %>%
        dplyr::rename(c("covariateAnalysisId" = "analysisId")) %>%
        dplyr::select(
          covariateId,
          covariateName,
          beforeMatchingStdDiff,
          afterMatchingStdDiff,
          beforeMatchingMeanTarget,
          afterMatchingMeanTarget,
          beforeMatchingMeanComparator,
          afterMatchingMeanComparator,
          covariateAnalysisId
        ) %>%
        dplyr::mutate(
          analysisId = analysisSettings$analysisId,
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          outcomeId = predictOutcome,
          runLabel = currentRunCmAnalysis$label
        ) %>%
          dplyr::relocate(analysisId, runLabel) %>%
        saveRDS(
          file = file.path(
            analysisPath,
            "shiny",
            paste(
              paste(
                "overall",
                "balance",
                analysisSettings$analysisId,
                currentRunCmAnalysis$label,
                sep = "_"
              ),
              "rds",
              sep = "."
            )
          )
        )
      ParallelLogger::logInfo(
        paste(
          "Saved overall covariate balance for analysis:",
          currentRunCmAnalysis$label
        )
      )
    }

    incidence <- dplyr::bind_rows(incidenceList)
    rownames(incidence) <- NULL
    saveRDS(
      object = incidence,
      file = file.path(
        analysisPath,
        "shiny",
        paste0(
          paste(
            "incidenceOverall",
            analysisSettings$analysisId,
            sep = "_"
          ),
          ".rds"
        )
      )
    )
    ParallelLogger::logInfo("Saved incidence results for all analyses")

  return(NULL)
}



generateSingleAnalysis <- function(
  analysisId,
  analysisPath,
  runSettings,
  cohortMethodData = NULL,
  analysisSettings
) {

  analysis <- runSettings$runCmSettings$analyses[[analysisId]]
  psMethod <- analysis$psAdjustmentMethod$psAdjustmentSettings$psMethod

  outcomeId <- analysis$stratificationOutcome
  psFullLocation <- "Prediction"
  psSaveLocation <- file.path(
    analysisPath,
    "Estimation",
    analysis$label,
    outcomeId,
    outcomeId,
    paste0(
      paste(
        "psFull",
        analysis$label,
        sep = "_"
      ),
      ".rds"
    )
  )

  directoryCheck(dirname(psSaveLocation))

  ps <- readRDS(
    file = file.path(
      analysisPath,
      psFullLocation,
      outcomeId,
      "psFull.rds"
    )
  )

  psAdjustmentSettings <- analysis$psAdjustmentMethod$psAdjustmentSettings
  if (psMethod == "matchOnPs") {
    ps <- CohortMethod::matchOnPs(
      population = ps,
      caliper = psAdjustmentSettings$caliper,
      caliperScale = psAdjustmentSettings$caliperScale,
      maxRatio = psAdjustmentSettings$maxRatio
    )
    if (sum(ps$outcomeCount) < 20) {
      return(NULL)
    }
    model <- CohortMethod::fitOutcomeModel(
      population = ps,
      cohortMethodData = cohortMethodData,
      modelType = "cox",
      stratified = FALSE
    )
  } else if (psMethod == "stratifyByPs") {
    ps <- CohortMethod::stratifyByPs(
      population = ps,
      numberOfStrata = psAdjustmentSettings$numberOfStrata,
      baseSelection = psAdjustmentSettings$baseSelection
    )
    if (sum(ps$outcomeCount) < 20) {
      return(NULL)
    }
    model <- CohortMethod::fitOutcomeModel(
      population = ps,
      cohortMethodData = cohortMethodData,
      modelType = "cox",
      stratified = TRUE
    )
  }
  saveRDS(
    ps,
    file = psSaveLocation
  )
  return(
    data.frame(
      runLabel = analysis$label,
      estimate = exp(model$outcomeModelTreatmentEstimate$logRr),
      lower = exp(model$outcomeModelTreatmentEstimate$logLb95),
      upper = exp(model$outcomeModelTreatmentEstimate$logUb95),
      seLogRr = model$outcomeModelTreatmentEstimate$seLogRr,
      outcomeId = outcomeId,
      database = analysisSettings$databaseName,
      analysisType = psMethod,
      treatmentId = analysisSettings$treatmentCohortId,
      comparatorId = analysisSettings$comparatorCohortId
    )
  )
}


includeOverallResultsNegativeControls <- function(
    analysisSettings,
    getDataSettings,
    runSettings
) {

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  nThreads <- runSettings$runCmSettings$negativeControlThreads
  outcomeIds <- analysisSettings$negativeControlOutcomes

  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = file.path(
      getDataSettings$cohortMethodDataFolder
    )
  )

  cluster <- ParallelLogger::makeCluster(
    numberOfThreads = nThreads
  )
  ParallelLogger::clusterRequire(
    cluster,
    "RiskStratifiedEstimation"
  )
  ParallelLogger::clusterRequire(
    cluster,
    "CohortMethod"
  )

  overallNcEffectsList <- list()
  for (i in seq_along(runSettings$runCmSettings$analyses)) {
    currentRunCmAnalysis <- runSettings$runCmSettings$analyses[[i]]

    result <- ParallelLogger::clusterApply(
      cluster = cluster,
      x = outcomeIds,
      fun = generateSingleAnalysisNegativeControls,
      analysisSettings = analysisSettings,
      runSettings = runSettings,
      analysisPath = analysisPath,
      cohortMethodData = cohortMethodData,
      analysis = currentRunCmAnalysis
    )

    overallNcEffectsList[[i]] <- result %>% dplyr::bind_rows()
  }

  mappedOverallResultsNegativeControls <- overallNcEffectsList %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(analysisId = analysisSettings$analysisId) %>%
    dplyr::relocate(analysisId, runLabel)
  rownames(mappedOverallResultsNegativeControls) <- NULL

  fileName <- "mappedOverallResultsNegativeControls"
  saveRDS(
    mappedOverallResultsNegativeControls,
    file.path(
      analysisPath,
      "shiny",
      paste0(
        paste(fileName, analysisSettings$analysisId, sep = "_"),
        ".rds"
      )
    )
  )

}

generateSingleAnalysisNegativeControls <- function(
    outcomeId,
    analysisSettings,
    runSettings,
    analysisPath,
    cohortMethodData,
    analysis
) {

  predictOutcome <- analysis$stratificationOutcome
  psFullLocation <- "NegativeControls"
  psAdjustmentSettings <- analysis$psAdjustmentMethod$psAdjustmentSettings
  psMethod <- psAdjustmentSettings$psMethod

  psSaveLocation <- file.path(
    analysisPath,
    "Estimation",
    analysis$label,
    predictOutcome,
    outcomeId,
    paste0(
      paste(
        "psFull",
        analysis$label,
        sep = "_"
      ),
      ".rds"
    )
  )

  directoryCheck(dirname(psSaveLocation))

  ps <- readRDS(
    file = file.path(
      analysisPath,
      psFullLocation,
      outcomeId,
      "psFull.rds"
    )
  )

  if (psMethod == "matchOnPs") {
    ps <- CohortMethod::matchOnPs(
      population = ps,
      caliper = psAdjustmentSettings$caliper,
      caliperScale = psAdjustmentSettings$caliperScale,
      maxRatio = psAdjustmentSettings$maxRatio
    )
    if (sum(ps$outcomeCount) < 20) {
      return(NULL)
    }
    model <- CohortMethod::fitOutcomeModel(
      population = ps,
      cohortMethodData = cohortMethodData,
      modelType = "cox",
      stratified = FALSE
    )
  } else if (psMethod == "stratifyByPs") {
    ps <- CohortMethod::stratifyByPs(
      population = ps,
      numberOfStrata = psAdjustmentSettings$numberOfStrata,
      baseSelection = psAdjustmentSettings$baseSelection
    )
    if (sum(ps$outcomeCount) < 20) {
      return(NULL)
    }
    model <- CohortMethod::fitOutcomeModel(
      population = ps,
      cohortMethodData = cohortMethodData,
      modelType = "cox",
      stratified = TRUE
    )
  }
  saveRDS(
    ps,
    file = psSaveLocation
  )
  return(
    data.frame(
      runLabel = analysis$label,
      estimate = exp(model$outcomeModelTreatmentEstimate$logRr),
      lower = exp(model$outcomeModelTreatmentEstimate$logLb95),
      upper = exp(model$outcomeModelTreatmentEstimate$logUb95),
      seLogRr = model$outcomeModelTreatmentEstimate$seLogRr,
      outcomeId = outcomeId,
      database = analysisSettings$databaseName,
      treatmentId  = analysisSettings$treatmentCohortId,
      comparatorId = analysisSettings$comparatorCohortId
    )
  )
}
