#' Fit outcome models
#'
#' Fits outcome models within risk strata, estimating relative and absolute differences. Designed to be performed within
#' a parellelized analysis.
#'
#' @param outcomeId               The outcome of interest for which the esitmation is performed. That is the outcome
#'                                for which risk stratification is performed.
#' @param getDataSettings         An R object of type \code{getDataSettings} created using the function
#'                                \code{\link[RiskStratifiedEstimation]{createGetDataSettings}}.
#' @param pathToPs                The path to the \code{RSEE} analysis results.
#' @param runSettings             The settings for running a \code{CohortMethod}
#'                                analysis created from
#'                                \code{\link[RiskStratifiedEstimation]{createRunCmSettings}}.
#'
#' @importFrom dplyr %>%
#' @export

fitOutcomeModels <- function(
  outcomeId,
  getDataSettings,
  pathToPs,
  runCmSettings
) {

  ParallelLogger::logInfo(
    paste(
      "Calculating main results for outcome:",
      outcomeId
    )
  )
  analysisPath <- file.path(
    pathToPs,
    outcomeId
  )

  ps <- tryCatch(
    {
      readRDS(
        file.path(
          analysisPath,
          "ps.rds"
        )
      )
    },
    error = function(e)
    {
      e$message
    }
  )

  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = getDataSettings$cohortMethodDataFolder
  )

  ParallelLogger::logInfo(
    "Read PS and CohortMethod data"
  )

  ParallelLogger::logInfo(
    "Starting estimation of treatment effects"
  )

  treatmentEffects <- tryCatch(
    {
      estimateTreatmentEffect(
        ps = ps,
        runCmSettings = runCmSettings
      )
    },
    error = function(e)
    {
      e$message
    }
  )


  ParallelLogger::logInfo(
    "Done estimating treatment effects"
  )

  if (!is.character(treatmentEffects)) {

    saveRDS(
      treatmentEffects$relativeRiskReduction,
      file = file.path(
        analysisPath,
        paste0(
          paste(
            "temp",
            "relativeRiskReduction",
            runCmSettings$label,
            sep = "_"
          ),
          ".rds"
        )
      )
    )
    saveRDS(
      treatmentEffects$absoluteRiskReduction,
      file = file.path(
        analysisPath,
        paste0(
          paste(
            "temp",
            "absoluteRiskReduction",
            runCmSettings$label,
            sep = "_"
          ),
          ".rds"
        )
      )
    )
    saveRDS(
      treatmentEffects$models,
      file = file.path(
        analysisPath,
        paste0(
          paste(
            "models",
            runCmSettings$label,
            sep = "_"
          ),
          ".rds"
        )
      )
    )
    saveRDS(
      treatmentEffects$cases,
      file = file.path(
        analysisPath,
        paste0(
          paste(
            "temp",
            "cases",
            runCmSettings$label,
            sep = "_"
          ),
          ".rds"
        )
      )
    )
    saveRDS(
      treatmentEffects$ps,
      file = file.path(
        analysisPath,
        paste0(
          paste(
            "ps",
            runCmSettings$label,
            sep = "_"
          ),
          ".rds"
        )
      )
    )

  }

  ParallelLogger::logInfo(
    'Saved results'
  )

  return(NULL)
}



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
)
{

  runSettings$runCmSettings <- runSettings$runCmSettings[[1]]

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )


  predictions <- readRDS(
    file.path(
      analysisPath,
      "Prediction",
      predictOutcome,
      analysisSettings$analysisId,
      "plpResult",
      "prediction.rds"
    )
  )

  riskStrata <- runSettings$runCmSettings$riskStrata
  predictionsQuantiles <- quantile(
    predictions$value,
    probs = 0:riskStrata/riskStrata
  )

  predictionsQuantiles[1] <- 0
  predictionsQuantiles[length(predictionsQuantiles)] <- 1
  # Need to link that to predictions as they read from the same directory
  predictionResult <-
    PatientLevelPrediction::loadPlpResult(
      file.path(
        analysisPath,
        "Prediction",
        predictOutcome,
        analysisSettings$analysisId,
        "plpResult"
      )
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

  if (is.character(dummy))
  {
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

  riskPredictions <-
    PatientLevelPrediction::applyModel(
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
    )
  riskPredictions <- within(
    riskPredictions,
    quantile <- as.integer(
      cut(
        value,
        predictionsQuantiles,
        include.lowest = TRUE
      )
    )
  )

  ps <- list()
  failed <- FALSE
  riskStrata = runSettings$runCmSettings$riskStrata

  for (i in 1:riskStrata) {
    ParallelLogger::logInfo(
      paste(
        "making population in stratum",
        i
      )
    )
    population <- riskPredictions %>%
      dplyr::filter(
        quantile == i
      ) %>%
      dplyr::select(
        -c(
          "value",
          "quantile"
        )
      )
    ParallelLogger::logInfo(
      'Done'
    )
    ParallelLogger::logInfo(
      'Fitting ps'
    )

    ps[[i]] <- tryCatch(
      {
        CohortMethod::createPs(
          cohortMethodData = cohortMethodData,
          population = population,
          excludeCovariateIds = runSettings$runCmSettings$psSettings$excludeCovariateIds,
          stopOnError = TRUE,
          errorOnHighCorrelation = TRUE,
          control = runSettings$runCmSettings$psSettings$control,
          prior = runSettings$runCmSettings$psSettings$prior
        )
      },
      error = function(e)
      {
        e$message
      }
    )

    if (is.character(ps[[i]]))
    {
      failed <- TRUE
      break()
    }

    ParallelLogger::logInfo(
      'Done'
    )
  }

  if (!failed)
  {

    saveDir <- file.path(
      analysisPath,
      "Estimation",
      predictOutcome,
      compareOutcome
    )

    if (!dir.exists(saveDir))
    {
      dir.create(
        saveDir,
        recursive = TRUE
      )
    }

    saveRDS(
      ps,
      file.path(
        saveDir,
        "ps.rds"
      )
    )
  }

  return(
    data.frame(
      stratOutcome = predictOutcome,
      estOutcome = compareOutcome,
      psFailed = failed
    )
  )

}

#' Absolute risk reduction
#'
#' Calculates absolute risk reduction based on the Kaplan-Meier estimates within risk strata
#'
#' @param population         The study population generated by \code{\link[CohortMethod]{matchOnPs}} when using
#'                           propensity score matching or by \code{\link[CohortMethod]{stratifyByPs}} when stratifying
#'                           on the propensity score. In case of inverse probability of treatment weighting approach,
#'                           it is a datframe with a \code{weights} column.
#' @param timePoint          The time at which the absolute risk difference is estimated
#' @param psMethod           Can be one of "matchOnPs", "stratifyByPs" or "inversePtWeighted".
#'
#' @return                   A dataframe with the absolute risk-stratum specific absolute risk difference estimates,
#'                           along with 95 percent confidence interval.
#'
#' @export

absoluteRiskReduction <- function(population,
                                  timePoint,
                                  psMethod){

  population <- as.data.frame(
    population
  )
  population$event <- ifelse(
    is.na(population$daysToEvent),
    yes = 0,
    no = 1
  )

  population$S <- survival::Surv(
    population$survivalTime,
    population$event
  )

  kaplanMeier <-  survival::survfit(
    S ~ treatment,
    data = population
  )

  if (psMethod == "matchOnPs")
  {

    summaryKM <- summary(
      kaplanMeier,
      times = timePoint
    )

    standardError <- sqrt(
      sum(
        summaryKM$std.err^2
      )
    )

    arr <- diff(
      summaryKM$surv
    )

    res <- c(
      arr,
      arr - 1.96*standardError,
      arr + 1.96*standardError
    )

  }
  else if (psMethod == "stratifyByPs")
  {
    kaplanMeier <- list()
    kk <- sort(
      unique(
        population$stratumId
      )
    )

    for (i in kk)
    {
      kaplanMeier[[i]] <- survival::survfit(
        S ~ treatment,
        data = subset(
          population,
          stratumId == i
        )
      )
    }

    summaryKMList <- lapply(
      kaplanMeier,
      summary,
      times = timePoint
    )

    arrList <- lapply(
      summaryKMList,
      getAbsoluteDifference
    )

    arr <- mean(
      unlist(
        arrList
      )
    )

    standardErrors <- lapply(
      summaryKMList,
      getStandadrdError
    )

    pooledStandardError <- sqrt(
      sum(unlist(standardErrors)^2)/25
    )

    res <- c(
      arr,
      arr - 1.96*pooledStandardError,
      arr + 1.96*pooledStandardError
    )
  }

  else if (psMethod == "inversePtWeighted")
  {
    kaplanMeier <-  survival::survfit(
      S ~ treatment,
      data = population,
      weights = weights
    )

    summaryKM <- summary(
      kaplanMeier,
      times = timePoint
    )

    standardError <- sqrt(
      sum(summaryKM$std.err^2)
    )

    arr <- diff(
      summaryKM$surv
    )

    res <- c(
      arr,
      arr - 1.96*standardError,
      arr + 1.96*standardError
    )
  }

  return(res)

}




#' Relative risk reduction
#'
#' Calculates hazard ratios within risk strata.
#' @param model    The model that was used to fit a cox regression model to the data.
#'
#' @return         A dataframe with hazard ratios for treatment effect across risk strata along with 95 percent
#'                 confidence intervals
#'
#' @export

relativeRiskReduction <- function(model){

  if (class(model) == "OutcomeModel")
  {
    return(
      exp(
        model$outcomeModelTreatmentEstimate[1:3]
      )
    )
  }
  else
  {
    return(
      summary(model)$conf.int[c(1, 3:4)]
    )
  }

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


#' Calculate stratified Kaplan-Meier estimates
#'
#' @param population                The population of interest stratified using the
#'                                  \code{\link[CohortMethod]{stratifyByPs}}
#' @param timePoint                 The point in time for which the absolute risk difference is required
#'
#' @return                          A vector of the absolute risk difference along with the lowest and highest limits
#'                                  of the the 95 percent confidence interval
#'
stratifiedKaplanMeier <- function(population, timePoint) {

  kaplanMeier <- list()

  for (i in unique(population$stratumId))
  {
    kaplanMeier[[i]] <- survival::survfit(
      S ~ treatment,
      data = subset(
        population,
        stratumId == i
      )
    )
  }

  summaryKMList <- lapply(
    kaplanMeier,
    summary,
    times = timePoint
  )

  arrList <- lapply(
    summaryKMList,
    getAbsoluteDifference
  )

  arr <- mean(
    unlist(
      arrList
    )
  )

  standardErrors <- lapply(
    summaryKMList,
    getStandadrdError
  )

  pooledStandardError <- sqrt(
    sum(unlist(standardErrors)^2)/25)

  return(
    c(
      arr,
      arr - 1.96*pooledStandardError,
      arr + 1.96*pooledStandardError
    )
  )

}



#' Estimate treatment effects within risk strata
#'
#' Estimates treatment effects within risk strata based on the length of list \code{ps}.
#'
#' @param ps               A list of objects created from \code{\link[CohortMethod]{createPs}} estimated within risk
#'                         strata.
#' @param runCmSettings    The settings for running a \code{CohortMethod} analysis.
#'
#' @return                 A list containing :
#'                         \itemize{
#'                           \item{relativeRiskReduction}{Hazard ratios along with confidence intervals within risk
#'                             strata}
#'                           \item{absoluteRiskReduction}{Absolute risk differences along with confidence intervals
#'                             within risk strata}
#'                           \item{cases}{Observed outcome proportions within risk strata}
#'                           \item{models}{The models used to estimate relative risk reduction within risk strata}
#'                         }
#'
#'
#' @export

estimateTreatmentEffect <- function(
  ps,
  runCmSettings
) {

  if (runCmSettings$psMethod == "matchOnPs") {
    ps <- lapply(
      ps,
      CohortMethod::matchOnPs,
      caliper = runCmSettings$effectEstimationSettings$caliper,
      caliperScale = runCmSettings$effectEstimationSettings$caliperScale,
      maxRatio = runCmSettings$effectEstimationSettings$maxRatio,
      stratificationColumns = runCmSettings$effectEstimationSettings$stratificationColumns
    )

    models <- lapply(
      ps,
      CohortMethod::fitOutcomeModel,
      stratified = FALSE,
      modelType = "cox"
    )

    cases <- do.call(
      rbind,
      lapply(
        ps,
        getCounts,
        timePoint = runCmSettings$timePoint,
        psMethod = runCmSettings$psMethod
      )
    )

    colnames(cases) <- c(
      "comparator",
      "treatment"
    )

    cases <- as.data.frame(
      cases
    )

    riskStrata <- length(ps)

    cases$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )

    arr <- do.call(
      rbind,
      lapply(
        ps,
        absoluteRiskReduction,
        timePoint = runCmSettings$timePoint,
        psMethod = runCmSettings$psMethod
      )
    )

    colnames(arr) <- c(
      "ARR",
      "lower",
      "upper"
    )

    arr <- as.data.frame(arr)

    arr$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )

    rrr <- do.call(
      rbind,
      lapply(
        models,
        relativeRiskReduction
      )
    )

    colnames(rrr) <- c(
      "HR",
      "lower",
      "upper"
    )

    rrr <- as.data.frame(rrr)

    rrr$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )

  }
  else if (runCmSettings$psMethod == "stratifyByPs")
  {
    ps <- lapply(
      ps,
      CohortMethod::stratifyByPs,
      numberOfStrata = runCmSettings$effectEstimationSettings$numberOfStrata,
      stratificationColumns = runCmSettings$effectEstimationSettings$stratificationColumns,
      baseSelection = runCmSettings$effectEstimationSettings$baseSelection
    )

    ParallelLogger::logInfo(
      "Stratified by PS in risk strata"
    )

    cases <- do.call(
      rbind,
      lapply(
        ps,
        getCounts,
        timePoint = runCmSettings$timePoint,
        psMethod = runCmSettings$psMethod
      )
    )

    ParallelLogger::logInfo(
      "Calculated number of cases"
    )

    colnames(cases) <- c(
      "comparator",
      "treatment"
    )

    cases <- as.data.frame(cases)
    riskStrata <- length(ps)
    cases$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )
    models <- lapply(
      ps,
      CohortMethod::fitOutcomeModel, stratified = TRUE, modelType = "cox")
    ParallelLogger::logInfo("Fitted outcome models")
    arr <- do.call(
      rbind,
      lapply(
        ps,
        absoluteRiskReduction,
        timePoint = runCmSettings$timePoint,
        psMethod = runCmSettings$psMethod
      )
    )

    ParallelLogger::logInfo(
      "Calculated absolute risk reduction across risk strata"
    )

    colnames(arr) <- c(
      "ARR",
      "lower",
      "upper"
    )

    arr <- as.data.frame(arr)
    arr$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )

    rrr <- do.call(
      rbind,
      lapply(
        models,
        relativeRiskReduction
      )
    )

    ParallelLogger::logInfo(
      "Calculated relative risk reduction across risk strata"
    )

    colnames(rrr) <- c(
      "HR",
      "lower",
      "upper"
    )

    rrr <- as.data.frame(rrr)
    rrr$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )

  }
  else if (runCmSettings$psMethod == "inversePtWeighted")
  {
    ps <- lapply(
      ps,
      createIPW,
      weightsType = runCmSettings$effectEstimationSettings$weightsType,
      useStabilizedWeights = runCmSettings$effectEstimationSettings$useStabilizedWeights,
      truncationLevels = runCmSettings$effectEstimationSettings$truncationLevels
    )

    models <- lapply(
      ps,
      outcomeModelWeighted,
      calculateWeights = FALSE
    )

    cases <- do.call(
      rbind,
      lapply(
        ps,
        getCounts,
        timePoint = runCmSettings$timePoint,
        psMethod = runCmSettings$psMethod
      )
    )

    colnames(cases) <- c(
      "comparator",
      "treatment"
    )

    cases <- as.data.frame(cases)
    riskStrata <- length(ps)

    cases$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )

    arr <- do.call(
      rbind,
      lapply(
        ps,
        absoluteRiskReduction,
        timePoint = runCmSettings$timePoint,
        psMethod = runCmSettings$psMethod
      )
    )

    colnames(arr) <- c(
      "ARR",
      "lower",
      "upper"
    )

    arr <- as.data.frame(arr)
    arr$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )

    rrr <- do.call(
      rbind,
      lapply(
        models,
        relativeRiskReduction
      )
    )

    colnames(rrr) <- c(
      "HR",
      "lower",
      "upper"
    )

    rrr <- as.data.frame(
      rrr
    )

    rrr$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )

  }

  return(
    list(
      relativeRiskReduction = rrr %>% dplyr::mutate(analysisType = runCmSettings$label),
      absoluteRiskReduction = arr %>% dplyr::mutate(analysisType = runCmSettings$label),
      cases = cases %>% dplyr::mutate(analysisType = runCmSettings$label),
      models = models,
      ps = ps
    )
  )

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
#'
#' @return                           \code{NULL}. The results are all saved.
#'
#' @export

fitPsModelOverall <- function(
  outcomeId,
  getDataSettings,
  populationSettings,
  analysisSettings,
  runCmSettings
)
{
  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = getDataSettings$cohortMethodDataFolder
  )

  plpData <- PatientLevelPrediction::loadPlpData(
    file = getDataSettings$plpDataFolder
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

  startingPop <- populationCm %>%
    dplyr::left_join(
      plpData$cohorts
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

  pop <- populationCm %>%
    dplyr::select(
      "rowId",
      "subjectId",
      "treatment"
    ) %>%
    dplyr::left_join(
      populationPlp
    )

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
  else if (runCmSettings$psMethod == "stratifyByPs")
  {
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
  getDataSettings,
  populationSettings,
  runSettings,
  analysisSettings
)
{

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  runSettings$runCmSettings <- runSettings$runCmSettings[[1]]

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
    dplyr::select(
      rowId,
      subjectId,
      value
    )

  nRiskStrata <- runSettings$runCmSettings$riskStrata
  attr(populationCm, "metaData") <- populationCmMetaData

  ParallelLogger::logInfo(
    "Stratifying estimation population"
  )

  mapMatrix <- riskPredictions %>%
    dplyr::mutate(
      riskStratum = dplyr::ntile(
        riskPredictions$value,
        nRiskStrata
      )
    )

  ParallelLogger::logInfo(
    "Estimating propensity scores within risk strata"
  )

  ps <- list()
  for (i in 1:nRiskStrata)
  {
    population <- populationCm[populationCm$subjectId %in% mapMatrix[mapMatrix$riskStratum == i,]$subjectId, ]
    ps[[i]] <- CohortMethod::createPs(
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
  }

  saveDir <- file.path(
    analysisPath,
    "Estimation",
    outcomeId
  )

  dir.create(
    saveDir,
    recursive = TRUE
  )

  saveRDS(
    lapply(
      ps, as.data.frame
    ),
    file.path(
      saveDir,
      "ps.rds"
    )
  )

  saveRDS(
    mapMatrix,
    file.path(
      analysisPath,
      "Estimation",
      outcomeId,
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

  ParallelLogger::logInfo(
    paste(
      "Saved the map matrix for outcome",
      outcomeId
    )
  )

  return(NULL)

}

#' @export
includeOverallResults <- function(
  analysisSettings,
  getDataSettings,
  runCmSettings
)
{
  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  outcomeIds <- analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]
  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = file.path(
      getDataSettings$cohortMethodDataFolder
    )
  )

  covariates <- cohortMethodData$covariates %>%
    dplyr::collect()
  data.table::setDT(covariates)

  covariateRef <- cohortMethodData$covariateRef %>%
    dplyr::collect()
  data.table::setDT(covariateRef)

  cohorts <- cohortMethodData$cohorts %>%
    dplyr::collect()
  data.table::setDT(cohorts)

  overallTreatmentEffects <- incidence <- NULL

  for (outcomeId in outcomeIds) {
    ps <- readRDS(
      file = file.path(
        analysisPath,
        "Estimation",
        outcomeId,
        "psFull.rds"
      )
    )

    if (runCmSettings$psMethod == "matchOnPs") {
      ps <- CohortMethod::matchOnPs(
        population = ps,
        caliper = runCmSettings$effectEstimationSettings$caliper,
        caliperScale = runCmSettings$effectEstimationSettings$caliperScale,
        maxRatio = runCmSettings$effectEstimationSettings$maxRatio
      )
    } else if (runCmSettings$psMethod == "stratifyByPs") {
      ps <- CohortMethod::stratifyByPs(
        population = ps,
        numberOfStrata = runCmSettings$effectEstimationSettings$numberOfStrata,
        baseSelection = runCmSettings$effectEstimationSettings$baseSelection
      )
    }

    model <- CohortMethod::fitOutcomeModel(
      population = ps,
      cohortMethodData = cohortMethodData,
      modelType = "cox",
      stratified = TRUE
    )

    overallTreatmentEffects <- data.frame(
      estimate = exp(model$outcomeModelTreatmentEstimate$logRr),
      lower = exp(model$outcomeModelTreatmentEstimate$logLb95),
      upper = exp(model$outcomeModelTreatmentEstimate$logUb95),
      outcomeId = outcomeId,
      database = analysisSettings$databaseName,
      analysisType = runCmSettings$label,
      treatmentId = analysisSettings$treatmentCohortId,
      comparatorId = analysisSettings$comparatorCohortId
    ) %>%
      dplyr::bind_rows(overallTreatmentEffects)

    incidence <- computeIncidence(ps) %>%
      dplyr::mutate(
        database = analysisSettings$databaseName,
        analysisId = analysisSettings$analysisId,
        treatmentId = analysisSettings$treatmentCohortId,
        comparatorId = analysisSettings$comparatorCohortId,
        outcomeId = outcomeId,
        analysisType = runCmSettings$label
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
        analysisType = runCmSettings$label
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
              runCmSettings$label,
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
        afterMatchingStdDiff
      ) %>%
      dplyr::mutate(
        analysisId = analysisSettings$analysisId,
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
              runCmSettings$label,
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
  rownames(overallTreatmentEffects) <- rownames(incidence) <- NULL
  saveRDS(
    object = incidence,
    file = file.path(
      analysisPath,
      "shiny",
      paste0(
        paste(
          "temp",
          "incidenceOverall",
          runCmSettings$label,
          sep = "_"
        ),
        ".rds"
      )
    )
  )

  saveRDS(
    object = overallTreatmentEffects,
    file = file.path(
      analysisPath,
      "shiny",
      paste0(
        paste(
          "temp",
          "mappedOverallResults",
          runCmSettings$label,
          sep = "_"
        ),
        ".rds"
      )
    )
  )
  return(NULL)
}

## Non-exports ##

getCounts <- function(population,
                      timePoint,
                      psMethod){

  population <- as.data.frame(
    population
  )

  population$event <- ifelse(
    is.na(population$daysToEvent),
    yes = 0,
    no = 1
  )

  population$S <- survival::Surv(
    population$survivalTime,
    population$event
  )

  kaplanMeier <-  survival::survfit(
    S ~ treatment,
    data = population
  )

  if (psMethod == "matchOnPs")
  {
    summaryKM <- summary(
      kaplanMeier,
      times = timePoint
    )

    res <- 1 - summaryKM$surv

  }
  else if (psMethod == "stratifyByPs")
  {
    kaplanMeier <- list()
    stratId <- sort(
      unique(
        population$stratumId
      )
    )

    for (i in stratId)
    {
      kaplanMeier[[i]] <- survival::survfit(
        S ~ treatment,
        data = subset(
          population,
          stratumId == i
        )
      )
    }

    summaryKMList <- lapply(
      kaplanMeier,
      summary,
      times = timePoint
    )

    res <- colMeans(
      do.call(
        rbind,
        lapply(
          summaryKMList,
          function(s) {
            1 - s$surv
          }
        )
      )
    )
  }
  else if (psMethod == "inversePtWeighted")
  {
    kaplanMeier <-  survival::survfit(
      S ~ treatment,
      data = population,
      weights = weights
    )

    summaryKM <- summary(
      kaplanMeier,
      times = timePoint
    )

    res <- 1 - summaryKM$surv

  }

  return(res)

}




getStandadrdError <- function(summaryKmList)
{

  sqrt(sum(summaryKmList$std.err^2))
}




getAbsoluteDifference <- function(summaryKMList)
{
  diff(summaryKMList$surv)
}
