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
  predictionId,
  timepoint = NULL
) {
  .generateEvaluation <- function(
    population,
    plpData,
    plpModel,
    timepoint
  ) {
    modelEvaluationOnSubset <- PatientLevelPrediction::predictPlp(
      population = population,
      plpData = plpData,
      plpModel = plpModel,
      timepoint = timepoint
    ) %>%
      dplyr::mutate(
        evaluation = "Test"
      )

    modelType <- attr(modelEvaluationOnSubset, "metaData")$modelType

    calibrationData <- PatientLevelPrediction:::getCalibrationSummary(
      prediction = modelEvaluationOnSubset,
      predictionType = modelType,
      numberOfStrata = 10,
      truncateFraction = .01
    )
    evaluationStatistics <- PatientLevelPrediction:::getEvaluationStatistics(
      prediction = modelEvaluationOnSubset,
      predictionType = modelType,
      typeColumn = "evaluation"
    )

    return(
      list(
        prediction = modelEvaluationOnSubset,
        calibrationData = calibrationData,
        evaluationStatistics = evaluationStatistics
      )
    )
  }

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
      "Prediction",
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
    dplyr::mutate(
      outcomeCount = ifelse(
        outcomeCount > 0,
        yes = 1,
        no = 0
      )
    )

  # Evaluate on matched set
  populationSubset <- CohortMethod::matchOnPs(
    psFull
  )
  attr(populationSubset, "metaData") <- list(
    cohortId = 1,
    outcomeId = predictionId
  )

  evaluation <- .generateEvaluation(
    population = populationSubset,
    plpData = plpData,
    plpModel = plpResult$model,
    timepoint = timepoint
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

  directoryCheck(analysisPath)

  saveRDS(
    evaluation$calibrationData,
    file = file.path(
      analysisPath,
      "calibrationData.rds"
    )
  )

  saveRDS(
    evaluation$evaluationStatistics,
    file = file.path(
      analysisPath,
      "performanceEvaluation.rds"
    )
  )

  saveRDS(
    evaluation$prediction,
    file = file.path(
      analysisPath,
      "prediction.rds"
    )
  )

  # Load entire target population
  population <- plpData$cohorts %>%
    dplyr::right_join(
      psFull
    )

  attr(populationSubset, "metaData") <- list(
    cohortId = 1,
    outcomeId = predictionId
  )

  evaluation <- .generateEvaluation(
    population = population,
    plpData = plpData,
    plpModel = plpResult$model,
    timepoint = timepoint
  )

  # Save evaluation on the entire target population
  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "EntirePopulation"
  )

  directoryCheck(analysisPath)

  saveRDS(
    evaluation$calibrationData,
    file = file.path(
      analysisPath,
      "calibrationData.rds"
    )
  )
  saveRDS(
    evaluation$evaluationStatistics,
    file = file.path(
      analysisPath,
      "performanceEvaluation.rds"
    )
  )

  saveRDS(
    evaluation$prediction,
    file = file.path(
      analysisPath,
      "prediction.rds"
    )
  )

  # Evaluate on treatment arm
  populationSubset <- population %>%
    dplyr::filter(
      treatment == 1
    )

  evaluation <- .generateEvaluation(
    population = populationSubset,
    plpData = plpData,
    plpModel = plpResult$model,
    timepoint = timepoint
  )

  # Save evaluation on treatment arm
  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "Treatment"
  )

  directoryCheck(analysisPath)

  saveRDS(
    evaluation$calibrationData,
    file = file.path(
      analysisPath,
      "calibrationData.rds"
    )
  )

  saveRDS(
    evaluation$evaluationStatistics,
    file = file.path(
      analysisPath,
      "performanceEvaluation.rds"
    )
  )

  saveRDS(
    evaluation$prediction,
    file = file.path(
      analysisPath,
      "prediction.rds"
    )
  )

  # Evaluate on comparator arm
  populationSubset <- population %>%
    dplyr::filter(
      treatment == 0
    )

  evaluation <- .generateEvaluation(
    population = populationSubset,
    plpData = plpData,
    plpModel = plpResult$model,
    timepoint = timepoint
  )

  # Save evaluation on comparator arm
  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "Comparator"
  )

  directoryCheck(analysisPath)

  saveRDS(
    evaluation$calibrationData,
    file = file.path(
      analysisPath,
      "calibrationData.rds"
    )
  )

  saveRDS(
    evaluation$evaluationStatistics,
    file = file.path(
      analysisPath,
      "performanceEvaluation.rds"
    )
  )

  saveRDS(
    evaluation$prediction,
    file = file.path(
      analysisPath,
      "prediction.rds"
    )
  )

  return(NULL)
}



#-------------------------------------------------------------------------------
# PS density
#-------------------------------------------------------------------------------
#' @export
computePsDensity <- function(population) {

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


#' @export
computePsDensityOverall <- function(
  path,
  analysisSettings,
  stratOutcome,
  analysisType
) {

  analysisPath <- file.path(
    path,
    "ps_analysis.rds"
  )

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  estOutcome = as.numeric(
    basename(
      path
    )
  )

  # ----------------------------------------------------------------------------
  # If propensity score  estimation failed return nothing, otherwise continue
  # with applying computeIncidence to every risk stratum
  # ----------------------------------------------------------------------------
  if (!file.exists(analysisPath)) {
    return()
  }

  ps <- readRDS(analysisPath)

  lapply(
    ps,
    computePsDensity
  ) %>%
    dplyr::bind_rows(
      .id = "riskStratum"
    ) %>%
    dplyr::mutate(
      riskStratum = paste0(
        "Q",
        riskStratum
      )
    ) %>%
    dplyr::mutate(
      database     = analysisSettings$databaseName,
      analysisId   = analysisSettings$analysisId,
      stratOutcome = stratOutcome,
      estOutcome   = estOutcome,
      treatmentId  = analysisSettings$treatmentCohortId,
      comparatorId = analysisSettings$comparatorCohortId,
      analysisType = analysisType
    ) %>%
    dplyr::tibble() %>%
    saveRDS(
      file = file.path(
        saveDir,
        paste0(
          paste(
            "psDensity",
            analysisSettings$analysisId,
            analysisSettings$databaseName,
            analysisType,
            analysisSettings$treatmentCohortId,
            analysisSettings$comparatorCohortId,
            stratOutcome,
            estOutcome,
            sep = "_"
          ),
          ".rds"
        )
      )
    )
}



#' @export
computeRseePsDensity <- function(analysisSettings) {
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
    analysisSettings$balanceThreads
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

      dummy <- ParallelLogger::clusterApply(
        cluster = cluster,
        x       = compareOutcomeDirs,
        fun     = computePsDensityOverall,
        analysisSettings = analysisSettings,
        stratOutcome = predictOutcomes[j],
        analysisType = basename(labels[i])
      )
    }
  }
}


#-------------------------------------------------------------------------------
# Incidence
#-------------------------------------------------------------------------------

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


computeRseeIncidence <- function(analysisSettings) {
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
    analysisSettings$balanceThreads
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
                "incidence",
                basename(labels[i]),
                predictOutcomes[j],
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

predictionPerformance <- function(
  outcomeId,
  analysisSettings
) {

  .recoverEvaluation <- function(
    cohort,
    outcomeId,
    analysisSettings
  ) {

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
    )

    performance <- as.data.frame(
      performance
    )
    rownames(performance) <- NULL

    ret <- performance %>%
      dplyr::mutate(
        value = as.numeric(value)
      )  %>%
      dplyr::select(
        metric,
        value
      ) %>%
      tidyr::spread(
        metric,
        value
      ) %>%
      dplyr::mutate(
        cohort = cohort,
        analysisId = analysisSettings$analysisId,
        stratOutcome = outcomeId,
        treatmentId = analysisSettings$treatmentCohortId,
        comparatorId = analysisSettings$comparatorCohortId,
        database = analysisSettings$databaseName
      )

    return(ret)
  }

  cohorts <- c(
    "Comparator",
    "EntirePopulation",
    "Matched",
    "Treatment"
  )

  ret <- lapply(
    cohorts,
    .recoverEvaluation,
    analysisSettings = analysisSettings,
    outcomeId = outcomeId
  ) %>%
    dplyr::bind_rows()

  return(ret)
}


#' @importFrom dplyr %>%
#' @export

predictionPerformanceAnalysis <- function(
  analysisSettings,
  save = TRUE
) {

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

    directoryCheck(saveDir)

    saveRDS(
      performance,
      file.path(
        saveDir,
        "predictionPerformance.rds"
      )
    )
  }

  return(performance)
}
