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
#' @param initialPopulation          The initial population to be used for the
#'                                   estimation of the switched-outcome propensity
#'                                   scores.
#' @param analysisSettings           An R object of type \code{analysisSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createAnalysisSettings}}.
#' @param getDataSettings            An R object of type \code{getDataSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createGetDataSettings}}.
#' @param populationSettings         An R object of type \code{populationSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createPopulationSettings}}.
#' @param runSettings                An R object of type \code{runSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createRunSettings}}.
#' @importFrom magrittr %>%
fitPsModelSwitch <- function(
  predictOutcome,
  compareOutcome,
  initialPopulation,
  analysisSettings,
  getDataSettings,
  populationSettings,
  runSettings
) {

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

  # cohorts <- plpData$cohorts

  ParallelLogger::logInfo(
    "Creating combined population settings"
  )

  populationCmSettings <- populationSettings$populationCmSettings
  populationPlpCm <-
    CohortMethod::createStudyPopulation(
      cohortMethodData               = cohortMethodData,
      population                     = initialPopulation,
      outcomeId                      = predictOutcome,
      firstExposureOnly              = populationCmSettings$firstExposureOnly,
      restrictToCommonPeriod         = populationCmSettings$restrictToCommonPeriod,
      removeDuplicateSubjects        = populationCmSettings$removeDuplicateSubjects,
      washoutPeriod                  = populationCmSettings$washoutPeriod,
      removeSubjectsWithPriorOutcome = populationCmSettings$removeSubjectsWithPriorOutcome,
      priorOutcomeLookback           = populationCmSettings$priorOutcomeLookback,
      minDaysAtRisk                  = populationCmSettings$minDaysAtRisk,
      maxDaysAtRisk                  = populationCmSettings$maxDaysAtRisk,
      riskWindowStart                = populationCmSettings$riskWindowStart,
      startAnchor                    = populationCmSettings$startAnchor,
      riskWindowEnd                  = populationCmSettings$riskWindowEnd,
      endAnchor                      = populationCmSettings$endAnchor,
      censorAtNewRiskWindow          = populationCmSettings$censorAtNewRiskWindow
    ) # %>%
  # dplyr::mutate(
  #   cohortStartDate = lubridate::as_date(
  #     cohortStartDate
  #   )
  # ) %>%
  # dplyr::left_join(
  #   cohorts,
  #   by = c(
  #     "rowId",
  #     # "subjectId",
  #     "cohortStartDate",
  #     "daysFromObsStart",
  #     "daysToCohortEnd",
  #     "daysToObsEnd"
  #   )
  # )

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

  populationCmSettings <- populationSettings$populationCmSettings
  populationCm <-
    CohortMethod::createStudyPopulation(
      cohortMethodData               = cohortMethodData,
      population                     = populationPlpCm,
      outcomeId                      = compareOutcome,
      firstExposureOnly              = populationCmSettings$firstExposureOnly,
      restrictToCommonPeriod         = populationCmSettings$restrictToCommonPeriod,
      washoutPeriod                  = populationCmSettings$washoutPeriod,
      removeDuplicateSubjects        = TRUE,
      removeSubjectsWithPriorOutcome = populationCmSettings$removeSubjectsWithPriorOutcome,
      priorOutcomeLookback           = populationCmSettings$priorOutcomeLookback,
      minDaysAtRisk                  = populationCmSettings$minDaysAtRisk,
      maxDaysAtRisk                  = populationCmSettings$maxDaysAtRisk,
      riskWindowStart                = populationCmSettings$riskWindowStart,
      startAnchor                    = populationCmSettings$startAnchor,
      riskWindowEnd                  = populationCmSettings$riskWindowEnd,
      endAnchor                      = populationCmSettings$endAnchor,
      censorAtNewRiskWindow          = populationCmSettings$censorAtNewRiskWindow
    )

  timepoint <- runSettings$runPlpSettings$plpResults %>%
    dplyr::filter(outcomeId == predictOutcome) %>%
    dplyr::pull(timepoint)

  #-----------------------------------------------------------------------------
  # Predictions in the population with switched outcome
  # They may be different from original, due to population
  # definitions, but the thresholds for risk stratification
  # should remain the same
  #-----------------------------------------------------------------------------
  riskPredictions <- PatientLevelPrediction::predictPlp(
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
    timepoint = timepoint
  ) %>%
    dplyr::as_tibble()

  analysisLabels <- names(runSettings$runCmSettings$analyses)

  failed <- purrr::map(
    .x               = analysisLabels,
    .f               = psAnalysis,
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
#' @param initialPopulation          The reference population object, usually
#'                                   defined by joining the cohorts fo the plpData
#'                                   and cohortMethodData objects.
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
fitPsModelOverall <- function(
  outcomeId,
  initialPopulation,
  getDataSettings,
  populationSettings,
  analysisSettings,
  runCmSettings,
  isNegativeControl = FALSE
) {

  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = getDataSettings$cohortMethodDataFolder
  )

  populationCmSettings <- populationSettings$populationCmSettings
  populationCm <- CohortMethod::createStudyPopulation(
    cohortMethodData               = cohortMethodData,
    population                     = initialPopulation,
    outcomeId                      = outcomeId,
    firstExposureOnly              = populationCmSettings$firstExposureOnly,
    restrictToCommonPeriod         = populationCmSettings$restrictToCommonPeriod,
    washoutPeriod                  = populationCmSettings$washoutPeriod,
    removeDuplicateSubjects        = populationCmSettings$removeDuplicateSubjects,
    removeSubjectsWithPriorOutcome = populationCmSettings$removeSubjectsWithPriorOutcome,
    priorOutcomeLookback           = populationCmSettings$priorOutcomeLookback,
    minDaysAtRisk                  = populationCmSettings$minDaysAtRisk,
    maxDaysAtRisk                  = populationCmSettings$maxDaysAtRisk,
    riskWindowStart                = populationCmSettings$riskWindowStart,
    startAnchor                    = populationCmSettings$startAnchor,
    riskWindowEnd                  = populationCmSettings$riskWindowEnd,
    endAnchor                      = populationCmSettings$endAnchor,
    censorAtNewRiskWindow          = populationCmSettings$censorAtNewRiskWindow
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
    populationPlpSettings <- populationSettings$populationPlpSettings
    populationPlp <- PatientLevelPrediction::createStudyPopulation(
      plpData            = plpData,
      population         = initialPopulation,
      outcomeId          = outcomeId,
      populationSettings = populationPlpSettings
    ) %>%
      dplyr::tibble() %>%
      dplyr::inner_join(
        initialPopulation,
        by = c(
          "rowId",
          "subjectId",
          "targetId",
          "cohortStartDate",
          "daysFromObsStart",
          "daysToCohortEnd",
          "daysToObsEnd",
          "ageYear",
          "gender"
        )
      )

    pop <- pop %>%
      dplyr::left_join(
        populationPlp,
        by = c(
          "rowId",
          "personSeqId",
          "personId",
          "treatment",
          "cohortStartDate",
          "daysFromObsStart",
          "daysToCohortEnd",
          "daysToObsEnd",
          "subjectId",
          "targetId",
          "ageYear",
          "gender",
          "outcomeCount",
          "timeAtRisk",
          "daysToEvent",
          "survivalTime"
        )
      )

    saveDir <- file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "Prediction",
      outcomeId
    )
  } else {
    saveDir <- file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "NegativeControls",
      outcomeId
    )
    if (!dir.exists(saveDir)) {
      dir.create(
        path      = saveDir,
        recursive = TRUE
      )
    }
  }

  psSettings <- runCmSettings$psSettings
  ps <- CohortMethod::createPs(
    cohortMethodData        = cohortMethodData,
    population              = as.data.frame(pop),
    includeCovariateIds     = psSettings$includeCovariateIds,
    excludeCovariateIds     = psSettings$excludeCovariateIds,
    maxCohortSizeForFitting = psSettings$maxCohortSizeForFitting,
    errorOnHighCorrelation  = psSettings$errorOnHighCorrelation,
    stopOnError             = psSettings$stopOnError,
    control                 = psSettings$control,
    prior                   = psSettings$prior
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
#' @param initialPopulation          The initial population on the subset of which
#'                                   the propensity scores will be estimated.
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
#' @importFrom magrittr %>%

fitPsModel <- function(
  outcomeId,
  initialPopulation,
  runSettings,
  getDataSettings,
  populationSettings,
  analysisSettings
) {

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  analysisLabels <- names(runSettings$runCmSettings$analyses)

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

  populationPlpSettings <- populationSettings$populationPlpSettings

  populationPlp <- PatientLevelPrediction::createStudyPopulation(
    plpData            = plpData,
    population         = initialPopulation,
    outcomeId          = outcomeId,
    populationSettings = populationPlpSettings
  ) %>%
    dplyr::tibble() %>%
    dplyr::inner_join(initialPopulation)

  ParallelLogger::logInfo(
    "Generating the estimation population"
  )

  populationCmSettings <- populationSettings$populationCmSettings
  populationCm <- CohortMethod::createStudyPopulation(
    cohortMethodData               = cohortMethodData,
    population                     = initialPopulation,
    outcomeId                      = outcomeId,
    firstExposureOnly              = populationCmSettings$firstExposureOnly,
    restrictToCommonPeriod         = populationCmSettings$restrictToCommonPeriod,
    washoutPeriod                  = populationCmSettings$washoutPeriod,
    removeDuplicateSubjects        = populationCmSettings$removeDuplicateSubjects,
    removeSubjectsWithPriorOutcome = populationCmSettings$removeSubjectsWithPriorOutcome,
    priorOutcomeLookback           = populationCmSettings$priorOutcomeLookback,
    minDaysAtRisk                  = populationCmSettings$minDaysAtRisk,
    maxDaysAtRisk                  = populationCmSettings$maxDaysAtRisk,
    riskWindowStart                = populationCmSettings$riskWindowStart,
    startAnchor                    = populationCmSettings$startAnchor,
    riskWindowEnd                  = populationCmSettings$riskWindowEnd,
    endAnchor                      = populationCmSettings$endAnchor,
    censorAtNewRiskWindow          = populationCmSettings$censorAtNewRiskWindow
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

  populationPlp <- populationPlp %>%
    dplyr::select("rowId", "ageYear", "gender")

  timepoint <- runSettings$runPlpSettings$timepoint
  riskPredictions <- PatientLevelPrediction::predictPlp(
    plpModel = predictionResult$model,
    plpData = plpData,
    population = populationCm,
    timepoint = timepoint
  ) %>%
    dplyr::tibble()

  # riskPredictions <- predictionResult$model$predict(
  #   plpData = plpData,
  #   population = populationCm
  # ) %>%
  #   dplyr::tibble()

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
    cohortMethodData = cohortMethodData,
    analysisSettings = analysisSettings,
    predictOutcome   = outcomeId,
    compareOutcome   = outcomeId
  )

  ParallelLogger::logInfo(
    paste(
      "Saved the map matrix for outcome",
      outcomeId
    )
  )

  return(tmp)

}




psAnalysis <- function(
  label,
  cohortMethodData,
  predictOutcome,
  compareOutcome,
  riskPredictions,
  analysisSettings,
  runSettings
) {
  analysis <- runSettings$runCmSettings$analyses[[label]]
  stratOutcomes <- analysis$stratificationOutcomes
  if (stratOutcomes != "all") {
    if (!predictOutcome %in% stratOutcomes) {
      return(NULL)
    }
  }

  if (predictOutcome == compareOutcome) {
    mapMatrix <- createMapMatrix(
      riskPredictions = riskPredictions,
      analysis        = analysis
    )
  } else {
    startingPath <- file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "Estimation",
      label,
      predictOutcome,
      predictOutcome
    )

    mapMatrix <- readRDS(
      file = file.path(
        startingPath,
        "mapMatrix.rds"
      )
    ) %>%
      dplyr::select(
        c(
          "rowId",
          # "subjectId",
          "treatment",
          "value",
          "labels",
          "riskStratum"
        )
      ) %>%
      dplyr::inner_join(riskPredictions)
  }

  # nRiskStrata <- ifelse(
  #   analysis$riskStratificationMethod == "equal",
  #   yes = analysis$riskStratificationThresholds,
  #   no  = length(analysis$riskStratificationThresholds) - 1
  # )

  riskStratificationMethod <- analysis$riskStratificationMethod
  if (riskStratificationMethod == "equal") {
    nRiskStrata <- analysis$riskStratificationThresholds
  } else if (riskStratificationMethod == "quantile") {
    nRiskStrata <- length(analysis$riskStratificationThresholds) - 1
  } else if (riskStratificationMethod == "custom") {
    nRiskStrata <- length(unique(mapMatrix$riskStratum))
  }

  # nRiskStrata <- dplyr::case_when(
  #   riskStratificationMethod == "equal"    ~ analysis$riskStratificationThresholds,
  #   riskStratificationMethod == "quantile" ~ length(analysis$riskStratificationThresholds) - 1,
  #   riskStratificationMethod == "custom"   ~ length(unique(mapMatrix$riskStratum))
  # )

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "Estimation",
    label,
    predictOutcome,
    compareOutcome
  )

  if (!dir.exists(saveDir))  {
    dir.create(
      path      = saveDir,
      recursive = TRUE
    )
  }

  failed <- runPsAnalysis(
    cohortMethodData = cohortMethodData,
    nRiskStrata      = nRiskStrata,
    mapMatrix        = mapMatrix,
    runSettings      = runSettings,
    saveDir          = saveDir
  )

  return(failed)
}


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
    population <- mapMatrix %>%
      dplyr::filter(riskStratum == i) %>%
      dplyr::select(-value)
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

