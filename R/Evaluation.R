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


#' @importFrom dplyr %>%
#' @export

evaluatePrediction <- function(
  analysisSettings,
  getDataSettings,
  populationSettings,
  predictionId
)
{
  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  plpData <- PatientLevelPrediction::loadPlpData(
    getDataSettings$plpDataFolder
  )

  plpResult <- PatientLevelPrediction::loadPlpResult(
    file.path(
      saveDir,
      "Prediction",
      predictionId,
      analysisSettings$analysisId,
      "plpResult"
    )
  )

  psFull <- readRDS(
    file.path(
      saveDir,
      "Estimation",
      predictionId,
      "psFull.rds"
    )
  )

  startingPopulation <- psFull %>%
    dplyr::select(
      -"daysToEvent"
    )
  attr(startingPopulation, "metaData") <- attr(plpData$cohorts, "metaData")

  psFull <-
    psFull %>%
    dplyr::right_join(
      PatientLevelPrediction::createStudyPopulation(
        plpData = plpData,
        population = startingPopulation,
        outcomeId = predictionId,
        binary = populationSettings$populationPlpSettings$binary,
        includeAllOutcomes = populationSettings$populationPlpSettings$includeAllOutcomes,
        firstExposureOnly = populationSettings$populationPlpSettings$firstExposureOnly,
        washoutPeriod = populationSettings$populationPlpSettings$washoutPeriod,
        removeSubjectsWithPriorOutcome = populationSettings$populationPlpSettings$removeSubjectsWithPriorOutcome,
        priorOutcomeLookback = populationSettings$populationPlpSettings$priorOutcomeLookback,
        requireTimeAtRisk = populationSettings$populationPlpSettings$requireTimeAtRisk,
        minTimeAtRisk = populationSettings$populationPlpSettings$minTimeAtRisk,
        riskWindowStart = populationSettings$populationPlpSettings$riskWindowStart,
        startAnchor = populationSettings$populationPlpSettings$startAnchor,
        riskWindowEnd = populationSettings$populationPlpSettings$riskWindowEnd,
        endAnchor = populationSettings$populationPlpSettings$endAnchor,
        verbosity = populationSettings$populationPlpSettings$verbosity
      )
    ) %>%
    dplyr::mutate(
      outcomeCount = ifelse(
        outcomeCount > 0,
        yes = 1,
        no = 0
      )
    )

  # attr(psFull, "metaData") <- attr(psFull, "metaData")

  # Evaluate on matched set
  populationSubset <- CohortMethod::matchOnPs(
    psFull
  )
  attr(populationSubset, "metaData") <- list(
    cohortId = 1,
    outcomeId = predictionId
  )

  modelEvaluationOnSubset <- PatientLevelPrediction::applyModel(
    population = populationSubset,
    plpData = plpData,
    plpModel = plpResult$model
  )

  # calibrationData <- PatientLevelPrediction::calibrationLine(
  #   prediction = modelEvaluationOnSubset$prediction
  # )

  calibrationData <- modelEvaluationOnSubset$prediction %>%
    dplyr::mutate(
      group = dplyr::ntile(
        value,
        10
      )
    ) %>%
    dplyr::group_by(
      group
    ) %>%
    dplyr::summarise(
      personsWithOutcome = sum(
        outcomeCount
      ),
      PersonCountAtRisk = dplyr::n(),
      observedIncidence = mean(
        outcomeCount
      ),
     averagePredictedProbability = mean(
       value
     )
    )

  # Save evaluation on matched set
  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "Matched"
  )

  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "Matched"
  )

  if (!dir.exists(analysisPath)) {

    dir.create(
      analysisPath,
      recursive = TRUE
    )

  }

  saveRDS(
    calibrationData,
    file = file.path(
      analysisPath,
      "calibrationData.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$performanceEvaluation,
    file = file.path(
      analysisPath,
      "performanceEvaluation.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$prediction,
    file = file.path(
      analysisPath,
      "prediction.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$covariateSummary,
    file = file.path(
      analysisPath,
      "covariateSummary.rds"
    )
  )

  # Load entire target population
  population <- plpData$cohorts %>%
    dplyr::right_join(
      psFull
    )

  attr(population, "metaData") <- list(
    cohortId = 1,
    outcomeId = predictionId
  )

  modelEvaluationOnPopulation <- PatientLevelPrediction::applyModel(
    population = population,
    plpData = plpData,
    plpModel = plpResult$model
  )

  calibrationData <- modelEvaluationOnPopulation$prediction %>%
    dplyr::mutate(
      group = dplyr::ntile(
        value,
        10
      )
    ) %>%
    dplyr::group_by(
      group
    ) %>%
    dplyr::summarise(
      personsWithOutcome = sum(
        outcomeCount
      ),
      PersonCountAtRisk = dplyr::n(),
      observedIncidence = mean(
        outcomeCount
      ),
     averagePredictedProbability = mean(
       value
     )
    )

  # Save evaluation on the entire target population
  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "EntirePopulation"
  )

  if (!dir.exists(analysisPath)) {

    dir.create(
      analysisPath,
      recursive = TRUE
    )

  }

  saveRDS(
    calibrationData,
    file = file.path(
      analysisPath,
      "calibrationData.rds"
    )
  )
  saveRDS(
    modelEvaluationOnPopulation$performanceEvaluation,
    file = file.path(
      analysisPath,
      "performanceEvaluation.rds"
    )
  )

  saveRDS(
    modelEvaluationOnPopulation$prediction,
    file = file.path(
      analysisPath,
      "prediction.rds"
    )
  )

  saveRDS(
    modelEvaluationOnPopulation$covariateSummary,
    file = file.path(
      analysisPath,
      "covariateSummary.rds"
    )
  )


  # Evaluate on treatment arm
  populationSubset <- population %>%
    dplyr::filter(
      treatment == 1
    )

  modelEvaluationOnSubset <- PatientLevelPrediction::applyModel(
    population = populationSubset,
    plpData = plpData,
    plpModel = plpResult$model
  )

  calibrationData <- modelEvaluationOnSubset$prediction %>%
    dplyr::mutate(
      group = dplyr::ntile(
        value,
        10
      )
    ) %>%
    dplyr::group_by(
      group
    ) %>%
    dplyr::summarise(
      personsWithOutcome = sum(
        outcomeCount
      ),
      PersonCountAtRisk = dplyr::n(),
      observedIncidence = mean(
        outcomeCount
      ),
      averagePredictedProbability = mean(
        value
      )
    )

  # Save evaluation on treatment arm
  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "Treatment"
  )

  if (!dir.exists(analysisPath)) {

    dir.create(
      analysisPath,
      recursive = TRUE
    )

  }

  saveRDS(
    calibrationData,
    file = file.path(
      analysisPath,
      "calibrationData.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$performanceEvaluation,
    file = file.path(
      analysisPath,
      "performanceEvaluation.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$prediction,
    file = file.path(
      analysisPath,
      "prediction.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$covariateSummary,
    file = file.path(
      analysisPath,
      "covariateSummary.rds"
    )
  )

  # Evaluate on comparator arm
  populationSubset <- population %>%
    dplyr::filter(
      treatment == 0
    )

  modelEvaluationOnSubset <- PatientLevelPrediction::applyModel(
    population = populationSubset,
    plpData = plpData,
    plpModel = plpResult$model
  )

  calibrationData <- modelEvaluationOnSubset$prediction %>%
    dplyr::mutate(
      group = dplyr::ntile(
        value,
        10
      )
    ) %>%
    dplyr::group_by(
      group
    ) %>%
    dplyr::summarise(
      personsWithOutcome = sum(
        outcomeCount
      ),
      PersonCountAtRisk = dplyr::n(),
      observedIncidence = mean(
        outcomeCount
      ),
      averagePredictedProbability = mean(
        value
      )
    )

  # Save evaluation on comparator arm
  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "Comparator"
  )

  if (!dir.exists(analysisPath)) {

    dir.create(
      analysisPath,
      recursive = TRUE
    )

  }

  saveRDS(
    calibrationData,
    file = file.path(
      analysisPath,
      "calibrationData.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$performanceEvaluation,
    file = file.path(
      analysisPath,
      "performanceEvaluation.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$prediction,
    file = file.path(
      analysisPath,
      "prediction.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$covariateSummary,
    file = file.path(
      analysisPath,
      "covariateSummary.rds"
    )
  )

  return(NULL)

}



#' @importFrom dplyr %>%
#' @export
psDensityOverall <- function(
  ps,
  runCmSettings
) {

  if (runCmSettings$psMethod == "matchOnPs") {
    ps <- lapply(
      ps,
      CohortMethod::matchOnPs,
      caliper      = runCmSettings$effectEstimationSettings$caliper,
      caliperScale = runCmSettings$effectEstimationSettings$caliperScale,
      maxRatio     = runCmSettings$effectEstimationSettings$maxRatio
    )
  } else if (runCmSettings$psMethod == "stratifyByPs") {
    ps <- lapply(
      ps,
      CohortMethod::stratifyByPs,
      numberOfStrata = runCmSettings$effectEstimationSettings$numberOfStrata,
      baseSelection  = runCmSettings$effectEstimationSettings$baseSelection
    )
  }

  lapply(ps,
         psDensity) %>%
    dplyr::bind_rows(
      .id = "riskStratum"
    ) %>%
    dplyr::mutate(
      riskStratum = paste0(
        "Q",
        riskStratum
      ) %>%
        return()
    )

}

#' @importFrom dplyr %>%
#' @export
psDensityCombined <- function(
  outcomeId,
  analysisSettings,
  runCmSettings,
  secondaryOutcomes = FALSE
) {

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  if (!dir.exists(saveDir)) {
    dir.create(
      saveDir,
      recursive = TRUE
    )
  }

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  dir <- file.path(
    analysisPath,
    "Estimation",
    outcomeId
  )

  ps <- tryCatch(
    {
      readRDS(
        file.path(
          dir,
          "ps.rds"
        )
      )
    },
    error = function(e)
    {
      e$message
    }
  )

  if (is.character(ps))
  {
    return(NULL)
  }

  psDensityOverall(
    ps = ps,
    runCmSettings = runCmSettings
  ) %>%
    dplyr::mutate(
      database = analysisSettings$databaseName,
      analysisId = analysisSettings$analysisId,
      stratOutcome = outcomeId,
      estOutcome = outcomeId,
      treatment = ifelse(
        treatment == 1,
        yes = analysisSettings$treatmentCohortId,
        no = analysisSettings$comparatorCohortId
      ),
      treatmentId = analysisSettings$treatmentCohortId,
      comparatorId = analysisSettings$comparatorCohortId,
      analysisType = runCmSettings$label
    ) %>%
    saveRDS(
      file.path(
        saveDir,
        paste0(
          paste(
            "psDensity",
            analysisSettings$analysisId,
            runCmSettings$label,
            analysisSettings$databaseName,
            analysisSettings$treatmentCohortId,
            analysisSettings$comparatorCohortId,
            outcomeId,
            outcomeId,
            sep = "_"
          ),
          ".rds"
        )
      )
    )

  if (secondaryOutcomes) {
    predLoc <- which(analysisSettings$outcomeIds == outcomeId)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- compareOutcomes[compareOutcomes != outcomeId]
    compareOutcomes <- sort(
      compareOutcomes[compareOutcomes != outcomeId]
    )

    for (compareOutcome in compareOutcomes) {
      dir <- file.path(
        analysisPath,
        "Estimation",
        outcomeId,
        compareOutcome
      )

      ps <- tryCatch(
        {
          readRDS(
            file.path(
              dir,
              "ps.rds"
            )
          )
        },
        error = function(e)
        {
          e$message
        }
      )

      if (is.character(ps))
      {
        return(NULL)
      }

      psDensityOverall(
        ps = ps,
        runCmSettings = runCmSettings
      ) %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          stratOutcome = outcomeId,
          estOutcome = compareOutcome,
          treatment = ifelse(
            treatment == 1,
            yes = analysisSettings$treatmentCohortId,
            no = analysisSettings$comparatorCohortId
          ),
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          analysisType = runCmSettings$label
        ) %>%
        saveRDS(
          file.path(
            saveDir,
            paste0(
              paste(
                "psDensity",
                analysisSettings$analysisId,
                runCmSettings$label,
                analysisSettings$databaseName,
                analysisSettings$treatmentCohortId,
                analysisSettings$comparatorCohortId,
                outcomeId,
                compareOutcome,
                sep = "_"
              ),
              ".rds"
            )
          )
        )
    }
  }
  return(NULL)
}

#' @export
psDensityAnalysis <- function(analysisSettings,
                              runCmSettings,
                              secondaryOutcomes = FALSE,
                              threads = 1){
  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  cluster <- ParallelLogger::makeCluster(threads)
  ParallelLogger::clusterRequire(
    cluster,
    "RiskStratifiedEstimation"
  )


  dummy <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = predictOutcomes,
    fun = psDensityCombined,
    analysisSettings = analysisSettings,
    runCmSettings = runCmSettings,
    secondaryOutcomes = secondaryOutcomes
  )

  ParallelLogger::stopCluster(cluster)

  return(NULL)
}




#### Non-exports ####

psDensity <- function(population) {

  treatmentDensity <- density(
    population %>%
      dplyr::filter(
        treatment == 1
      ) %>%
      dplyr::select(
        preferenceScore
      ) %>%
      unlist()
  )
  comparatorDensity <- density(
    population %>%
      dplyr::filter(
        treatment == 0
      ) %>%
      dplyr::select(
        preferenceScore
      ) %>%
      unlist()
  )

  data.frame(
    x = treatmentDensity$x,
    y = treatmentDensity$y,
    treatment = 1
  ) %>%
    dplyr::bind_rows(
      data.frame(
        x = comparatorDensity$x,
        y = comparatorDensity$y,
        treatment = 0
      )
    ) %>%
    return()


}



#' @importFrom dplyr %>%
#' @export

computeIncidence <- function(
  population,
  alpha = .05,
  power = .8,
  twoSided = TRUE,
  modelType = "cox"
) {

  population <- population %>%
    dplyr::mutate(
      outcomeCount = ifelse(
        outcomeCount > 0,
        yes = 1,
        no = 0
      )
    )

  res <- CohortMethod::computeMdrr(
    population = population
  ) %>%
    dplyr::select(
      -c(
        "targetExposures",
        "comparatorExposures",
        "se"
      )
    )  %>%
    dplyr::rename(
      "treatmentPersons" = "targetPersons",
      "treatmentDays"    = "targetDays"
    )

  treatmentArmOutcomes <- population %>%
    dplyr::group_by(
      treatment
    ) %>%
    dplyr::summarise(
      outcomes = sum(
        outcomeCount
      )
    )

  res %>%
    dplyr::mutate(
      treatmentOutcomes = treatmentArmOutcomes %>%
        dplyr::filter(
          treatment == 1
        ) %>%
        dplyr::select(
          outcomes
        ) %>%
        unlist(),
      comparatorOutcomes = treatmentArmOutcomes %>%
        dplyr::filter(
          treatment == 0
        ) %>%
        dplyr::select(
          outcomes
        ) %>%
        unlist()
    ) %>%
    return()

}






#' @importFrom dplyr %>%
#' @export
computeIncidenceOverall <- function(
  path,
  alpha = 0.05,
  power = 0.8,
  twoSided = TRUE,
  modelType = "cox"
) {

  analysisPath <- file.path(
    path,
    "ps_analysis.rds"
  )
  if (!file.exists(analysisPath)) {
    return()
  }
  ps <- readRDS(analysisPath)

  lapply(
    ps,
    computeIncidence,
    alpha = alpha,
    power = power,
    twoSided = twoSided,
    modelType = modelType
  ) %>%
    dplyr::bind_rows(
      .id = "riskStratum"
    ) %>%
    dplyr::mutate(
      riskStratum = paste0(
        "Q",
        riskStratum
      ),
      estOutcome = as.numeric(
        basename(
          path
        )
      )
    ) %>%
    return()

}


computeIncidenceAnalysis <- function(
  label,
  analysisSettings
) {

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "Estimation",
    label
  )

}


computeRseeInicidence <- function(analysisSettings) {
  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "Estimation"
  )

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  labels <- list.dirs(
    path = analysisPath,
    recursive = FALSE
  )

  cluster <- ParallelLogger::makeCluster(
    runSettings$runCmSettings$fitOutcomeModelsThreads
  )

  ParallelLogger::clusterRequire(
    cluster,
    "RiskStratifiedEstimation"
  )
  for (i in seq_along(labels)) {
    predictOutcomeDirs <- list.dirs(
      path       = labels[i],
      recursive  = FALSE,
      full.names = TRUE
    )

    predictOutcomes <- as.numeric(
      basename(
        predictOutcomeDirs
      )
    )

    for (j in seq_along(predictOutcomes)) {
      compareOutcomeDirs <- list.dirs(
        predictOutcomeDirs[j],
        recursive = FALSE
      )
      tmp <- ParallelLogger::clusterApply(
        cluster = cluster,
        x       = compareOutcomeDirs,
        fun     = computeIncidenceOverall
      )

      do.call(
        rbind,
        tmp
      ) %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          stratOutcome = predictOutcomes[j],
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          analysisType = basename(labels[i])
        ) %>%
        dplyr::tibble() %>%
        saveRDS(
          file.path(
            saveDir,
            paste0(
              paste(
                "tmp",
                incidence,
                basename(labels[i]),
                sep = "_"
              ),
              ".rds"
            )
          )
        )
    }
  }
}

#' @importFrom dplyr %>%
#' @export
computeIncidenceCombined <- function(
  outcomeId,
  analysisSettings,
  analysisType,
  alpha = 0.05,
  power = 0.8,
  twoSided = TRUE,
  modelType = "cox",
  secondaryOutcomes = FALSE
)
{

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  if (!dir.exists(saveDir)) {
    dir.create(
      saveDir,
      recursive = TRUE
    )
  }

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  ps <- readRDS(
    file.path(
      analysisPath,
      "Estimation",
      outcomeId,
      paste0(
        paste(
          "ps",
          analysisType,
          sep = "_"
        ),
        ".rds"
      )
    )
  )

  incidence <-
    computeIncidenceOverall(
      ps,
      alpha = alpha,
      power = power,
      twoSided = twoSided,
      modelType = modelType
    ) %>%
    dplyr::mutate(
      database = analysisSettings$databaseName,
      analysisId = analysisSettings$analysisId,
      stratOutcome = outcomeId,
      estOutcome = outcomeId,
      treatmentId = analysisSettings$treatmentCohortId,
      comparatorId = analysisSettings$comparatorCohortId,
      analysisType = analysisType
    )

  if (secondaryOutcomes) {

    predLoc <- which(analysisSettings$outcomeIds == outcomeId)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- sort(
      compareOutcomes[compareOutcomes != outcomeId]
    )

    for (compareOutcome in compareOutcomes) {
      dir <- file.path(
        analysisPath,
        "Estimation",
        outcomeId,
        compareOutcome
      )

      ps <- tryCatch(
        {
          readRDS(
            file.path(
              dir,
              paste0(
                paste(
                  "ps",
                  analysisType,
                  sep = "_"
                ),
                ".rds"
              )
            )
          )
        },
        error = function(e)
        {
          e$message
        }
      )

      if (is.character(ps))
      {
        next
      }

      incidence <-
        computeIncidenceOverall(ps) %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          stratOutcome = outcomeId,
          estOutcome = compareOutcome,
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          analysisType = analysisType
        ) %>%
        dplyr::bind_rows(
          incidence
        )

    }
  }

  return(incidence)
}


#' @importFrom dplyr %>%
#' @export
computeIncidenceAnalysis <- function(
  analysisSettings,
  analysisType,
  secondaryOutcomes = FALSE,
  threads = 1,
  alpha = 0.05,
  power = 0.8,
  twoSided = TRUE,
  modelType = "cox"
) {

  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  cluster <- ParallelLogger::makeCluster(
    threads
  )

  ParallelLogger::clusterRequire(
    cluster,
    c(
      "RiskStratifiedEstimation",
      "dplyr"
    )
  )

  res <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = predictOutcomes,
    fun = computeIncidenceCombined,
    analysisSettings = analysisSettings,
    analysisType = analysisType,
    alpha = alpha,
    power = power,
    twoSided = twoSided,
    modelType = modelType,
    secondaryOutcomes = secondaryOutcomes
  )

  ParallelLogger::stopCluster(cluster)

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  if (!dir.exists(saveDir)) {
    dir.create(
      saveDir,
      recursive = TRUE
    )
  }

  res %>%
    dplyr::bind_rows() %>%
    saveRDS(
      file.path(
        saveDir,
        paste0(
          paste(
            "temp",
            "incidence",
            analysisType,
            sep = "_"
          ),
          ".rds"
        )
      )
    )
}



#' @importFrom dplyr %>%
#' @export

predictionPerformance <- function(outcomeId,
                                  analysisSettings) {

  recoverEvaluation <- function(cohort,
                                outcomeId,
                                analysisSettings) {

    analysisPath <- file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "Prediction",
      outcomeId,
      analysisSettings$analysisId,
      cohort
    )

    performance <- readRDS(
      file.path(
        analysisPath,
        "performanceEvaluation.rds"
      )
    )$evaluationStatistics

    performance <- as.data.frame(
      performance
    )
    rownames(performance) <- NULL

    performance %>%
      dplyr::mutate(
        Value = as.numeric(
          as.character(
            Value
          )
        )
      ) %>%
      dplyr::filter(
        Metric %in% c(
          "AUC.auc",
          "AUC.auc_lb95ci",
          "AUC.auc_ub95ci",
          "AUPRC",
          "BrierScore",
          "BrierScaled",
          "CalibrationIntercept",
          "CalibrationSlope",
          "CalibrationInLarge"
        )
      ) %>%
      dplyr::mutate(
        metric = c(
          "auc",
          "aucLower",
          "aucUpper",
          "auprc",
          "brierScore",
          "brierScaled",
          "calibrationIntercept",
          "calibrationSlope",
          "calibrationInLarge"
        )
      ) %>%
      dplyr::select(
        metric,
        Value
      ) %>%
      tidyr::spread(
        metric,
        Value
      ) %>%
      dplyr::mutate(
        cohort = cohort,
        analysisId = analysisSettings$analysisId,
        stratOutcome = outcomeId,
        treatmentId = analysisSettings$treatmentCohortId,
        comparatorId = analysisSettings$comparatorCohortId,
        database = analysisSettings$databaseName
      ) %>%
      return()


  }

  cohorts <- c(
    "Comparator",
    "EntirePopulation",
    "Matched",
    "Treatment"
  )

  lapply(
    cohorts,
    recoverEvaluation,
    analysisSettings = analysisSettings,
    outcomeId = outcomeId
  ) %>%
    dplyr::bind_rows() %>%
    return()


}


#' @importFrom dplyr %>%
#' @export

predictionPerformanceAnalysis <- function(analysisSettings,
                                          save = TRUE) {

  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  performance <- lapply(
    predictOutcomes,
    predictionPerformance,
    analysisSettings
  ) %>%
    dplyr::bind_rows()

  if (save) {

    saveDir <- file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "shiny"
    )

    if (!dir.exists(saveDir)) {
      dir.create(
        saveDir,
        recursive = TRUE
      )
    }

    saveRDS(performance,
            file.path(
              saveDir,
              "predictionPerformance.rds"
            )
    )
  }

  return(performance)
}
