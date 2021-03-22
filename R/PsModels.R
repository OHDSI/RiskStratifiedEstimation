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


#' Fits switched propensity score models
#'
#' Fits propensity score models where the population is stratified based on prediction model for a certain
#' outcome of interest and estimation is focused on a different outcome
#'
#' @param predictOutcome             The outcome of the prediction step
#' @param compareOutcome             The outcome of interest for the estimation step
#' @param analysisSettings           An R object of type \code{analysisSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createAnalysisSettings}}.
#' @param getDataSettings            An R object of type \code{getDataSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createGetDataSettings}}.
#' @param populationSettings         An R object of type \code{populationSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createPopulationSettings}}.
#' @param runSettings                An R object of type \code{runSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createRunSettings}}.
#' @importFrom dplyr %>%
#' @export

fitPsModelSwitch <- function(
  predictOutcome,
  compareOutcome,
  analysisSettings,
  getDataSettings,
  populationSettings,
  runSettings
) {

  # runSettings$runCmSettings <- runSettings$runCmSettings[[1]]

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  dummy <- tryCatch(
    {
      plpData <-
        PatientLevelPrediction::loadPlpData(
          getDataSettings$plpDataFolder
        )

      cohortMethodData <-
        CohortMethod::loadCohortMethodData(
          getDataSettings$cohortMethodDataFolder
        )
    },
    error = function(e)
    {
      e$message
    }
  )

  if (is.character(dummy)) {
    return(
      data.frame(
        stratOutcome = predictOutcome,
        estOutcome = compareOutcome,
        psFailed = TRUE
      )
    )
  }

  cohorts <- plpData$cohorts

  ParallelLogger::logInfo(
    "Creating combined population settings"
  )
  populationPlpCm <-
    CohortMethod::createStudyPopulation(
      cohortMethodData = cohortMethodData,
      outcomeId = predictOutcome,
      firstExposureOnly = populationSettings$populationCmSettings$firstExposureOnly,
      restrictToCommonPeriod = populationSettings$populationCmSettings$restrictToCommonPeriod,
      removeDuplicateSubjects = populationSettings$populationCmSettings$removeDuplicateSubjects,
      washoutPeriod = populationSettings$populationCmSettings$washoutPeriod,
      removeSubjectsWithPriorOutcome = populationSettings$populationCmSettings$removeSubjectsWithPriorOutcome,
      priorOutcomeLookback = populationSettings$populationCmSettings$priorOutcomeLookback,
      minDaysAtRisk = populationSettings$populationCmSettings$minDaysAtRisk,
      riskWindowStart = populationSettings$populationCmSettings$riskWindowStart,
      startAnchor = populationSettings$populationCmSettings$startAnchor,
      riskWindowEnd = populationSettings$populationCmSettings$riskWindowEnd,
      endAnchor = populationSettings$populationCmSettings$endAnchor,
      censorAtNewRiskWindow = populationSettings$populationCmSettings$censorAtNewRiskWindow
    ) %>%
    dplyr::mutate(
      cohortStartDate = lubridate::as_date(
        cohortStartDate
      )
    ) %>%
    dplyr::left_join(
      cohorts,
      by = c(
        "rowId",
        "subjectId",
        "cohortStartDate",
        "daysFromObsStart",
        "daysToCohortEnd",
        "daysToObsEnd"
      )
    )

  ParallelLogger::logInfo(
    paste(
      "Stratification outcome",
      predictOutcome,
      "results outcome:",
      compareOutcome
    )
  )
  ParallelLogger::logInfo(
    "Generating population with switched outcome"
  )

  populationCm <-
    CohortMethod::createStudyPopulation(
      cohortMethodData = cohortMethodData,
      population = populationPlpCm,
      outcomeId = compareOutcome,
      firstExposureOnly = populationSettings$populationCmSettings$firstExposureOnly,
      restrictToCommonPeriod = populationSettings$populationCmSettings$restrictToCommonPeriod,
      washoutPeriod = populationSettings$populationCmSettings$washoutPeriod,
      removeDuplicateSubjects = TRUE,
      removeSubjectsWithPriorOutcome = populationSettings$populationCmSettings$removeSubjectsWithPriorOutcome,
      priorOutcomeLookback = populationSettings$populationCmSettings$priorOutcomeLookback,
      minDaysAtRisk = populationSettings$populationCmSettings$minDaysAtRisk,
      riskWindowStart = populationSettings$populationCmSettings$riskWindowStart,
      startAnchor = populationSettings$populationCmSettings$startAnchor,
      riskWindowEnd = populationSettings$populationCmSettings$riskWindowEnd,
      endAnchor = populationSettings$populationCmSettings$endAnchor,
      censorAtNewRiskWindow = populationSettings$populationCmSettings$censorAtNewRiskWindow)

  #-----------------------------------------------------------------------------
  # Predictions in the population with switched outcome
  # They may be different from original, due to population
  # definitions, but the thresholds for risk stratification
  # should remain the same
  #-----------------------------------------------------------------------------
  riskPredictions <- PatientLevelPrediction::applyModel(
    population = populationCm,
    plpData = plpData,
    plpModel = PatientLevelPrediction::loadPlpModel(
      file.path(
        analysisSettings$saveDirectory,
        analysisSettings$analysisId,
        "Prediction",
        predictOutcome,
        analysisSettings$analysisId,
        "plpResult",
        "model"
      )
    ),
    calculatePerformance = FALSE
  ) %>%
    dplyr::as_tibble()

  # ----------------------------------------------------------------------------
  # Update HERE!!
  # ----------------------------------------------------------------------------
  analysisLabels <- names(runSettings$runCmSettings$analyses)

  failed <- purrr::map(
    .x               = analysisLabels,
    .f               = psAnalysisSwitch,
    cohortMethodData = cohortMethodData,
    predictOutcome   = predictOutcome,
    compareOutcome   = compareOutcome,
    analysisSettings = analysisSettings,
    runSettings      = runSettings,
    riskPredictions  = riskPredictions
  )


  return(
    data.frame(
      stratOutcome = predictOutcome,
      estOutcome = compareOutcome,
      psFailed = unlist(failed)
    )
  )

}




#' Fits a weighted cox regression model
#'
#' Fits a weighted cox regression model using an inverse probability of treatment weighting approach
#'
#' @param ps                          A dataframe wiht propensity scores as generated from
#'                                    \code{\link[CohortMethod]{createPs}}.
#' @param calculateWeights            Should weights be calculated?
#' @param weightsType                 The type of weights for the balancing of covariates. Should be either 'ATE' or
#'                                    'ATT'
#' @param useStabilizedWeights        Should stabilized weights be used?
#' @param truncationLevels            The level of truncation expressed in percentiles of the propensity score.
#'
#' @return                            A weighted cox regression model.
#'
#' @export

outcomeModelWeighted <- function(ps,
                                 calculateWeights = TRUE,
                                 weightsType = 'ATE',
                                 useStabilizedWeights = TRUE,
                                 truncationLevels){

  if (calculateWeights) {
    ps <- createIPW(
      ps,
      weightsType = weightsType,
      useStabilizedWeights = useStabilizedWeights,
      truncationLevels = truncationLevels
    )
  }

  ps$outcomeCount <- ifelse(
    ps$outcomeCount != 0,
    yes = 1,
    no = 0
  )

  model <- survival::coxph(
    survival::Surv(survivalTime, outcomeCount) ~ treatment,
    data = ps,
    weights = ps$weights,
    robust = TRUE
  )

  return(model)

}



#' Creates Inverse Probability Weights
#'
#' Calcuates inverse probability weights based on the propensity score
#'
#' @param ps                         A propensity score data frame as created from \code{\link[CohortMethod]{createPs}}
#' @param weightsType                The type of the weights to be used. Allowed options are 'ATE' for average treatment
#'                                   effect and 'ATT' for average treatment effect on the treated weights
#' @param useStabilizedWeights       Should stabilized weights be used?
#' @param truncationLevels           The level of truncation expressed in percentiles of the propensity score.
#'
#' @return                           The ps data frame provided as input along with a weights column
#'
#' @export

createIPW <- function(
  ps,
  weightsType = 'ATE',
  useStabilizedWeights = TRUE,
  truncationLevels = c(.01, .99)
)
{

  if (weightsType == 'ATE')
  {
    ps$weights <- ps$treatment / ps$propensityScore + (1 - ps$treatment) / (1 - ps$propensityScore)
  }
  else
  {
    ps$weights <- ps$treatment + ps$propensityScore*(1 - ps$treatment) / (1 - ps$propensityScore)
  }

  if (useStabilizedWeights)
  {
    ps$stability <- mean(
      ps$treatment
    )
    ps$weights <- ps$treatment*ps$weights*ps$stability + (1 - ps$treatment)*ps$weights*(1 - ps$stability)
    ps <- dplyr::select(
      ps,
      -stability
    )
  }

  ps <-  dplyr::mutate(
    ps,
    weights = pmin(
      pmax(
        weights,
        quantile(
          weights,
          truncationLevels[1]
        )
      ),
      quantile(
        weights,
        truncationLevels[2]
      )
    )
  )

  return(ps)

}



#' Calculate propensity scores for a specific outcome
#'
#' Fits a large-scale regularized regression model to estimate propensity scores within predicted risk strata. Designed
#' to be applied in a parallelized analysis.
#'
#' @param outcomeId                  The outcome of interest for which the risk stratification is performed.
#' @param getDataSettings            An R object of type \code{getDataSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createGetDataSettings}}.
#' @param populationSettings         An R object of type \code{covariateSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createPopulationSettings}}
#' @param analysisSettings           An R object of type \code{analysisSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createAnalysisSettings}}.
#' @param runCmSettings              A parameter object of type \code{runCmSettingsArgs} defined using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createRunCmSettingsArgs}}
#' @param isNegativeControl          Is the analysis performed for a negative control?
#'
#' @return                           \code{NULL}. The results are all saved.
#'
#' @export

fitPsModelOverall <- function(
  outcomeId,
  getDataSettings,
  populationSettings,
  analysisSettings,
  runCmSettings,
  isNegativeControl = FALSE
) {
  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = getDataSettings$cohortMethodDataFolder
  )

  populationCm <- CohortMethod::createStudyPopulation(
    cohortMethodData = cohortMethodData,
    outcomeId = outcomeId,
    firstExposureOnly = populationSettings$populationCmSettings$firstExposureOnly,
    restrictToCommonPeriod = populationSettings$populationCmSettings$restrictToCommonPeriod,
    washoutPeriod = populationSettings$populationCmSettings$washoutPeriod,
    removeDuplicateSubjects = populationSettings$populationCmSettings$removeDuplicateSubjects,
    removeSubjectsWithPriorOutcome = populationSettings$populationCmSettings$removeSubjectsWithPriorOutcome,
    priorOutcomeLookback = populationSettings$populationCmSettings$priorOutcomeLookback,
    minDaysAtRisk = populationSettings$populationCmSettings$minDaysAtRisk,
    riskWindowStart = populationSettings$populationCmSettings$riskWindowStart,
    startAnchor = populationSettings$populationCmSettings$startAnchor,
    riskWindowEnd = populationSettings$populationCmSettings$riskWindowEnd,
    endAnchor = populationSettings$populationCmSettings$endAnchor,
    censorAtNewRiskWindow = populationSettings$populationCmSettings$censorAtNewRiskWindow
  ) %>%
    dplyr::mutate(
      cohortStartDate = lubridate::as_date(
        cohortStartDate
      )
    )
  pop <- populationCm

  if (!isNegativeControl) {

    plpData <- PatientLevelPrediction::loadPlpData(
      file = getDataSettings$plpDataFolder
    )
    populationPlp <- PatientLevelPrediction::createStudyPopulation(
      plpData = plpData,
      # population = as.data.frame(startingPop),
      outcomeId = outcomeId,
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

    pop <- pop %>%
      dplyr::select(
        "rowId",
        "subjectId",
        "treatment"
      ) %>%
      dplyr::left_join(
        populationPlp
      )
  }

  ps <- CohortMethod::createPs(
    cohortMethodData = cohortMethodData,
    population = as.data.frame(pop),
    includeCovariateIds = runCmSettings$psSettings$includeCovariateIds,
    excludeCovariateIds = runCmSettings$psSettings$excludeCovariateIds,
    maxCohortSizeForFitting = runCmSettings$psSettings$maxCohortSizeForFitting,
    errorOnHighCorrelation = runCmSettings$psSettings$errorOnHighCorrelation,
    stopOnError = runCmSettings$psSettings$stopOnError,
    control = runCmSettings$psSettings$control,
    prior = runCmSettings$psSettings$prior
  )


  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "Estimation",
    outcomeId
  )

  dir.create(
    saveDir,
    recursive = TRUE
  )

  saveRDS(
    ps,
    file.path(
      saveDir,
      "psFull.rds"
    )
  )

  ParallelLogger::logInfo(
    paste(
      "Calculated overall propensity scores for outcome",
      outcomeId
    )
  )

  return(NULL)
}








#' Calculate propensity scores for a specific outcome
#'
#' Fits a large-scale regularized regression model to estimate propensity scores within predicted risk strata. Designed
#' to be applied in a parallelized analysis.
#'
#' @param outcomeId                  The outcome of interest for which the esitmation is performed. That is the outcome for
#'                                   which risk stratification is performed.
#' @param analysisSettings           An R object of type \code{analysisSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createAnalysisSettings}}.
#' @param getDataSettings            An R object of type \code{getDataSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createGetDataSettings}}.
#' @param populationSettings         An R object of type \code{populationSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createPopulationSettings}}.
#' @param runSettings                An R object of type \code{runSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createRunSettings}}.

#' @return                           \code{NULL}. The results are all saved.
#'
#' @export
#'
#' @importFrom dplyr %>%

fitPsModel <- function(
  outcomeId,
  runSettings,
  getDataSettings,
  populationSettings,
  analysisSettings
)
{

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  ParallelLogger::logInfo(
    "Reading cohort method data"
  )

  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = getDataSettings$cohortMethodDataFolder
  )

  ParallelLogger::logInfo(
    "Reading patient level prediction data"
  )

  plpData <- PatientLevelPrediction::loadPlpData(
    file = getDataSettings$plpDataFolder
  )

  ParallelLogger::logInfo(
    "Generating the prediction population"
  )

  populationPlp <- PatientLevelPrediction::createStudyPopulation(
    plpData = plpData,
    outcomeId = outcomeId,
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

  ParallelLogger::logInfo(
    "Generating the estimation poppulation"
  )

  populationCm <- CohortMethod::createStudyPopulation(
    cohortMethodData = cohortMethodData,
    outcomeId = outcomeId,
    firstExposureOnly = populationSettings$populationCmSettings$firstExposureOnly,
    restrictToCommonPeriod = populationSettings$populationCmSettings$restrictToCommonPeriod,
    washoutPeriod = populationSettings$populationCmSettings$washoutPeriod,
    removeDuplicateSubjects = populationSettings$populationCmSettings$removeDuplicateSubjects,
    removeSubjectsWithPriorOutcome = populationSettings$populationCmSettings$removeSubjectsWithPriorOutcome,
    priorOutcomeLookback = populationSettings$populationCmSettings$priorOutcomeLookback,
    minDaysAtRisk = populationSettings$populationCmSettings$minDaysAtRisk,
    riskWindowStart = populationSettings$populationCmSettings$riskWindowStart,
    startAnchor = populationSettings$populationCmSettings$startAnchor,
    riskWindowEnd = populationSettings$populationCmSettings$riskWindowEnd,
    endAnchor = populationSettings$populationCmSettings$endAnchor,
    censorAtNewRiskWindow = populationSettings$populationCmSettings$censorAtNewRiskWindow
  )

  populationCmMetaData <- attr(
    populationCm,
    "metaData"
  )

  attr(populationCm, "metaData") <- attr(
    populationPlp,
    "metaData"
  )

  ParallelLogger::logInfo(
    "Loading the prediction result"
  )

  pathToPlpResult <- runSettings$runPlpSettings$plpResults %>%
    dplyr::filter(
      outcomeId == !!outcomeId
    ) %>%
    dplyr::select(
      "directory"
    ) %>%
    unlist() %>%
    as.character()

  predictionResult <- PatientLevelPrediction::loadPlpResult(
    file.path(
      pathToPlpResult,
      "plpResult"
    )
  )

  ParallelLogger::logInfo(
    "Predicting on the estimation population"
  )

  riskPredictions <- predictionResult$model$predict(
    plpData = plpData,
    population = populationCm
  ) %>%
    dplyr::tibble() %>%
    dplyr::select(
      rowId,
      subjectId,
      value
    )

  attr(populationCm, "metaData") <- populationCmMetaData  # Delete that?

  ParallelLogger::logInfo(
    "Stratifying estimation population"
  )

  ParallelLogger::logInfo(
    "Estimating propensity scores within risk strata"
  )

  tmp <- purrr::map(
    .x               = analysisLabels,
    .f               = psAnalysis,
    runSettings      = runSettings,
    riskPredictions  = riskPredictions,
    outcomeId        = outcomeId,
    cohortMethodData = cohortMethodData,
    analysisSettings = analysisSettings
  )

  ParallelLogger::logInfo(
    paste(
      "Saved the map matrix for outcome",
      outcomeId
    )
  )

  return(NULL)

}





#' @export
psAnalysis <- function(
  label,
  cohortMethodData,
  outcomeId,
  riskPredictions,
  analysisSettings,
  runSettings
) {
  analysis <- runSettings$runCmSettings$analyses[[label]]
  mapMatrix <- createMapMatrix(
    riskPredictions = riskPredictions,
    analysis        = analysis
  )

  nRiskStrata <- ifelse(
    analysis$riskStratificationMethod == "equal",
    yes = analysis$riskStratificationThresholds,
    no  = length(analysis$riskStratificationThresholds) - 1
  )

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    outcomeId,
    label
  )

  failed <- runPsAnalysis(
    cohortMethodData = cohortMethodData,
    nRiskStrata      = nRiskStrata,
    mapMatrix        = mapMatrix,
    runSettings      = runSettings,
    saveDir          = saveDir
  )

  return(failed)
}


#'@export
psAnalysisSwitch <- function(
  label,
  riskPredictions,
  cohortMethodData,
  predictOutcome,
  compareOutcome,
  analysisSettings,
  runSettings
) {
  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "Estimation",
    predictOutcome,
    label
  )

  startingMapMatrix <- readRDS(
    file = file.path(
      analysisPath,
      "mapMatrix.rds"
    )
  )

  mapMatrix <- startingMapMatrix %>%
    dplyr::inner_join(riskPredictions)

  analysis <- runSettings$runCmSettings$analyses[[label]]

  nRiskStrata <- ifelse(
    analysis$riskStratificationMethod == "equal",
    yes = analysis$riskStratificationThresholds,
    no  = length(analysis$riskStratificationThresholds) - 1
  )

  saveDir <- file.path(
    analysisPath,
    compareOutcome
  )

  failed <- runPsAnalysis(
    cohortMethodData = cohortMethodData,
    nRiskStrata      = nRiskStrata,
    mapMatrix        = mapMatrix,
    runSettings      = runSettings,
    saveDir          = saveDir
  )
  return(failed)
}

#'@export
runPsAnalysis <- function(
  cohortMethodData,
  nRiskStrata,
  mapMatrix,
  runSettings,
  saveDir
) {
  ps <- list()
  failed <- FALSE
  for (i in 1:nRiskStrata) {
    population <- populationCm[populationCm$rowId %in% mapMatrix[mapMatrix$riskStratum == i,]$rowId, ]
    ps[[i]] <- tryCatch(
      {
        CohortMethod::createPs(
          cohortMethodData = cohortMethodData,
          population = population,
          excludeCovariateIds = runSettings$runCmSettings$psSettings$excludeCovariateIds,
          includeCovariateIds = runSettings$runCmSettings$psSettings$includeCovariateIds,
          maxCohortSizeForFitting = runSettings$runCmSettings$psSettings$maxCohortSizeForFitting,
          errorOnHighCorrelation = runSettings$runCmSettings$psSettings$errorOnHighCorrelation,
          stopOnError = runSettings$runCmSettings$psSettings$stopOnError,
          prior = runSettings$runCmSettings$psSettings$prior,
          control = runSettings$runCmSettings$psSettings$control
        )
      },
      error = function(e)
      {
        e$message
      }
    )

    if (is.character(ps[[i]])) {
      failed <- TRUE
      break()
    }

  }

  if (!failed) {
    if (!dir.exists(saveDir)) {
      dir.create(
        saveDir,
        recursive = TRUE
      )
    }

    saveRDS(
      lapply(
        ps, dplyr::as_tibble
      ),
      file.path(
        saveDir,
        "ps.rds"
      )
    )

    saveRDS(
      mapMatrix,
      file.path(
        saveDir,
        'mapMatrix.rds'
      )
    )

    mapMatrix %>%
      dplyr::mutate(
        riskStratum = paste0(
          "Q",
          riskStratum
        )
      ) %>%
      dplyr::group_by(
        riskStratum
      ) %>%
      dplyr::summarise(
        minRisk = min(value),
        maxRisk = max(value),
        meanRisk = mean(value)
      ) %>%
      saveRDS(
        file.path(
          saveDir,
          "riskOverall.rds"
        )
      )
  }

  return(failed)
}

