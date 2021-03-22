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
#' @export

fitOutcomeModelsOverall <- function(
  outcomeId,
  analysisSettings,
  getDataSettings,
  runCmSettings
)
{
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


#' @export
includeOverallResults <- function(
  analysisSettings,
  getDataSettings,
  runSettings,
  analysis,
  isNegativeControl = FALSE
) {
  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  if (isNegativeControl) {
    outcomeIds <- analysisSettings$negativeControlOutcomes
    nThreads <- runSettings$runCmSettings$negativeControlThreads
  } else {
    outcomeIds <- analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]
    nThreads <- runSettings$runCmSettings$fitOutcomeModelsThreads
  }

  # outcomeIds <- analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]
  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = file.path(
      getDataSettings$cohortMethodDataFolder
    )
  )

  generateSingleAnalysis <- function(
    outcomeId,
    analysisPath,
    cohortMethodData = NULL,
    analysisSettings,
    analysis
  ) {
    ps <- readRDS(
      file = file.path(
        analysisPath,
        "Estimation",
        outcomeId,
        "psFull.rds"
      )
    )

    if (analysis$psMethod == "matchOnPs") {
      ps <- CohortMethod::matchOnPs(
        population = ps,
        caliper = analysis$effectEstimationSettings$caliper,
        caliperScale = analysis$effectEstimationSettings$caliperScale,
        maxRatio = analysis$effectEstimationSettings$maxRatio
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
    } else if (analysis$psMethod == "stratifyByPs") {
      ps <- CohortMethod::stratifyByPs(
        population = ps,
        numberOfStrata = analysis$effectEstimationSettings$numberOfStrata,
        baseSelection = analysis$effectEstimationSettings$baseSelection
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
      file = file.path(
        analysisPath,
        "Estimation",
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
    )
    return(
      data.frame(
        estimate = exp(model$outcomeModelTreatmentEstimate$logRr),
        lower = exp(model$outcomeModelTreatmentEstimate$logLb95),
        upper = exp(model$outcomeModelTreatmentEstimate$logUb95),
        seLogRr = model$outcomeModelTreatmentEstimate$seLogRr,
        outcomeId = outcomeId,
        database = analysisSettings$databaseName,
        analysisType = analysis$label,
        treatmentId = analysisSettings$treatmentCohortId,
        comparatorId = analysisSettings$comparatorCohortId
      )
    )
  }

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
    x = outcomeIds,
    fun = generateSingleAnalysis,
    analysis = analysis,
    analysisSettings = analysisSettings,
    analysisPath = analysisPath
  )

  ParallelLogger::stopCluster(cluster)

  combinedResults <- dplyr::bind_rows(ret)

  rownames(combinedResults) <- NULL
  fileName <- ifelse(
    test = isNegativeControl,
    yes = "mappedOverallResultsNegativeControls",
    no = "mappedOverallResults"
  )
  saveRDS(
    object = combinedResults,
    file = file.path(
      analysisPath,
      "shiny",
      paste0(
        paste(
          "temp",
          fileName,
          analysis$label,
          sep = "_"
        ),
        ".rds"
      )
    )
  )

  if (!isNegativeControl) {

    incidence <- NULL
    covariates <- cohortMethodData$covariates %>%
      dplyr::collect()
    data.table::setDT(covariates)

    covariateRef <- cohortMethodData$covariateRef %>%
      dplyr::collect()
    data.table::setDT(covariateRef)

    cohorts <- cohortMethodData$cohorts %>%
      dplyr::collect()
    data.table::setDT(cohorts)

    for (outcomeId in outcomeIds) {
      ps <- readRDS(
        file = file.path(
          analysisPath,
          "Estimation",
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
      )
      incidence <- computeIncidence(ps) %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          outcomeId = outcomeId,
          analysisType = analysis$label
        ) %>%
        dplyr::bind_rows(incidence)

      psDensity(ps) %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          outcomeId = outcomeId,
          treatment = ifelse(
            treatment == 1,
            yes = analysisSettings$treatmentCohortId,
            no = analysisSettings$comparatorCohortId
          ),
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          analysisType = analysis$label
        ) %>%
        saveRDS(
          file = file.path(
            analysisPath,
            "shiny",
            paste(
              paste(
                "overall",
                "psDensity",
                analysisSettings$analysisId,
                analysis$label,
                analysisSettings$databaseName,
                analysisSettings$treatmentCohortId,
                analysisSettings$comparatorCohortId,
                outcomeId,
                sep = "_"
              ),
              "rds",
              sep = "."
            )
          )
        )

      computeCovariateBalance4(
        population = ps,
        cohorts = cohorts,
        covariates = covariates,
        covariateRef = covariateRef
      ) %>%
        dplyr::select(
          covariateId,
          covariateName,
          beforeMatchingStdDiff,
          afterMatchingStdDiff,
          beforeMatchingMeanTarget,
          afterMatchingMeanTarget,
          beforeMatchingMeanComparator,
          afterMatchingMeanComparator,
          analysisId
        ) %>%
        dplyr::mutate(
          analysisIdRsee = analysisSettings$analysisId,
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          outcomeId = outcomeId
        ) %>%
        saveRDS(
          file = file.path(
            analysisPath,
            "shiny",
            paste(
              paste(
                "overall",
                "balance",
                analysisSettings$analysisId,
                analysis$label,
                analysisSettings$databaseName,
                analysisSettings$treatmentCohortId,
                analysisSettings$comparatorCohortId,
                outcomeId,
                sep = "_"
              ),
              "rds",
              sep = "."
            )
          )
        )
    }
    rownames(incidence) <- NULL
    saveRDS(
      object = incidence,
      file = file.path(
        analysisPath,
        "shiny",
        paste0(
          paste(
            "temp",
            "incidenceOverall",
            analysis$label,
            sep = "_"
          ),
          ".rds"
        )
      )
    )
  }

  return(NULL)
}