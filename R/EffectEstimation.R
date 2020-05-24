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
#' @param runSettings             An R object of type \code{runSettings} created using the function
#'                                \code{\link[RiskStratifiedEstimation]{createRunSettings}}.
#' @return                        \code{NULL}. The results are all saved.
#'
#' @importFrom dplyr %>%
#' @export

fitOutcomeModels <- function(outcomeId,
                             getDataSettings,
                             pathToPs,
                             runSettings){

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

  ps <- readRDS(
    file.path(
      analysisPath,
      "ps.rds"
    )
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

  treatmentEffects <- tryCatch({
    estimateTreatmentEffect(
      ps = ps,
      runSettings = runSettings
    )
  },
  error = function(e){
    e$message
  })


  ParallelLogger::logInfo(
    "Done estimating treatment effects"
  )

  if (!is.character(treatmentEffects)) {

    saveRDS(
      treatmentEffects$relativeRiskReduction,
      file = file.path(
        analysisPath,
        'relativeRiskReduction.rds'
      )
    )
    saveRDS(
      treatmentEffects$absoluteRiskReduction,
      file = file.path(
        analysisPath,
        'absoluteRiskReduction.rds'
      )
    )
    saveRDS(
      treatmentEffects$models,
      file = file.path(
        analysisPath,
        'models.rds'
      )
    )
    saveRDS(
      treatmentEffects$cases,
      file = file.path(
        analysisPath,
        'cases.rds'
      )
    )
    saveRDS(
      treatmentEffects$ps,
      file = file.path(
        analysisPath,
        'ps.rds'
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

fitPsModelSwitch <- function(predictOutcome,
                             compareOutcome,
                             analysisSettings,
                             getDataSettings,
                             populationSettings,
                             runSettings){

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


  plpData <-
    PatientLevelPrediction::loadPlpData(
      getDataSettings$plpDataFolder
    )
  cohortMethodData <-
    CohortMethod::loadCohortMethodData(
      getDataSettings$cohortMethodDataFolder
    )
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

  ParallelLogger::logInfo('Switch outcome test')
  riskStrata = runSettings$runCmSettings$riskStrata

  ps <- list()
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

    ps[[i]] <- CohortMethod::createPs(
      cohortMethodData = cohortMethodData,
      population = population,
      excludeCovariateIds = runSettings$runCmSettings$psSettings$excludeCovariateIds,
      stopOnError = TRUE,
      errorOnHighCorrelation = TRUE,
      control = runSettings$runCmSettings$psSettings$control,
      prior = runSettings$runCmSettings$psSettings$prior
    )
    ParallelLogger::logInfo(
      'Done'
    )
  }
  saveDir <- file.path(
    analysisPath,
    "Estimation",
    predictOutcome,
    compareOutcome
  )
  if (!dir.exists(saveDir)) {
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

  if (psMethod == "matchOnPs") {

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
  else if (psMethod == "stratifyByPs") {

    kaplanMeier <- list()
    kk <- sort(
      unique(
        population$stratumId
      )
    )

    for (i in kk) {

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

  else if (psMethod == "inversePtWeighted") {

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

  if (class(model) == "outcomeModel") {
    return(
      exp(
        model$outcomeModelTreatmentEstimate[1:3]
      )
    )
  } else {
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

createIPW <- function(ps,
                      weightsType = 'ATE',
                      useStabilizedWeights = TRUE,
                      truncationLevels = c(.01, .99)){

  if (weightsType == 'ATE') {

    ps$weights <- ps$treatment / ps$propensityScore + (1 - ps$treatment) / (1 - ps$propensityScore)

  } else {

    ps$weights <- ps$treatment + ps$propensityScore*(1 - ps$treatment) / (1 - ps$propensityScore)

  }

  if (useStabilizedWeights) {

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

  for (i in unique(population$stratumId)) {

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
#' @param runSettings      An R object of type \code{runSettings} created using the function
#'                         \code{\link[RiskStratifiedEstimation]{createRunSettings}}
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

estimateTreatmentEffect <- function(ps,
                                    runSettings) {

  if (runSettings$runCmSettings$psMethod == "matchOnPs") {

    ps <- lapply(
      ps,
      CohortMethod::matchOnPs,
      caliper = runSettings$runCmSettings$effectEstimationSettings$caliper,
      caliperScale = runSettings$runCmSettings$effectEstimationSettings$caliperScale,
      maxRatio = runSettings$runCmSettings$effectEstimationSettings$maxRatio,
      stratificationColumns = runSettings$runCmSettings$effectEstimationSettings$stratificationColumns
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
        timePoint = runSettings$runCmSettings$timePoint,
        psMethod = runSettings$runCmSettings$psMethod
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
        timePoint = runSettings$runCmSettings$timePoint,
        psMethod = runSettings$runCmSettings$psMethod
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

  } else if (runSettings$runCmSettings$psMethod == "stratifyByPs") { # Need to fix the cases variable for stratification on ps!!!!

    ps <- lapply(
      ps,
      CohortMethod::stratifyByPs,
      numberOfStrata = runSettings$runCmSettings$effectEstimationSettings$numberOfStrata,
      stratificationColumns = runSettings$runCmSettings$effectEstimationSettings$stratificationColumns,
      baseSelection = runSettings$runCmSettings$effectEstimationSettings$baseSelection
    )

    ParallelLogger::logInfo(
      "Stratified by PS in risk strata"
    )

    cases <- do.call(
      rbind,
      lapply(
        ps,
        getCounts,
        timePoint = runSettings$runCmSettings$timePoint,
        psMethod = runSettings$runCmSettings$psMethod
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
        timePoint = runSettings$runCmSettings$timePoint,
        psMethod = runSettings$runCmSettings$psMethod
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

  } else if (runSettings$runCmSettings$psMethod == "inversePtWeighted") {

    ps <- lapply(
      ps,
      createIPW,
      weightsType = runSettings$runCmSettings$effectEstimationSettings$weightsType,
      useStabilizedWeights = runSettings$runCmSettings$effectEstimationSettings$useStabilizedWeights,
      truncationLevels = runSettings$runCmSettings$effectEstimationSettings$truncationLevels
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
        timePoint = runSettings$runCmSettings$timePoint,
        psMethod = runSettings$runCmSettings$psMethod
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
        timePoint = runSettings$runCmSettings$timePoint,
        psMethod = runSettings$runCmSettings$psMethod
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
      relativeRiskReduction = rrr,
      absoluteRiskReduction = arr,
      cases = cases,
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

fitPsModelOverall <- function(outcomeId,
                              getDataSettings,
                              populationSettings,
                              analysisSettings,
                              runCmSettings){

  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = getDataSettings$cohortMethodDataFolder
  )

  plpData <- PatientLevelPrediction::loadPlpData(
    file = getDataSettings$plpDataFolder
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

  populationSubset <- cohortMethodData$cohorts %>%
    dplyr::filter(
      subjectId %in% populationPlp$subjectId
    )

  ps <- CohortMethod::createPs(
    cohortMethodData = cohortMethodData,
    population = populationSubset,
    includeCovariateIds = runCmSettings$psSettings$includeCovariateIds,
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

fitOutcomeModelsOverall <- function(outcomeId,
                                    analysisSettings,
                                    getDataSettings,
                                    runCmSettings){

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

  if (runCmSettings$psMethod == "matchOnPs") {

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

  } else if (runCmSettings$psMethod == "stratifyByPs") {

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
  else if (psMethod == "inversePtWeighted") {

    ps <- createIPW(
      ps,
      weightsType = runCmSettings$effectEstimationSettings$weightsType,
      useStabilizedWeights = runCmSettings$effectEstimationSettings$useStabilizedWeights,
      truncationLevels = runCmSettings$effectEstimationSettings$truncationLevels
    )

    outcomeModel <- outcomeModelWeighted(
      ps,
      calculateWeights = FALSE,
      weightsType = runSettings$runCmSettings$effectEstimationSettings$weightsType,
      useStabilizedWeights = runSettings$runCmSettings$effectEstimationSettings$useStabilizedWeights,
      truncationLevels = runSettings$runCmSettings$effectEstimationSettings$truncationLevels
    )

    arr <- absoluteRiskReduction(
      ps,
      timePoint = runCmSettings$timePoint,
      psMethod = "inversePtWeighted"
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

fitPsModel <- function(outcomeId,
                       getDataSettings,  # For cohortMethodDataFolder, plpDataFolder
                       populationSettings, # For populationCmSettings, populationPlpSettings
                       runSettings,
                       analysisSettings){

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

  predictionResult <- PatientLevelPrediction::loadPlpResult(
    file.path(
      analysisPath,
      "Prediction",
      outcomeId,
      analysisSettings$analysisId,
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
  for (i in 1:nRiskStrata) {
    population <- populationCm[populationCm$subjectId %in% mapMatrix[mapMatrix$riskStratum == i,]$subjectId, ] # dplyr!!

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

  if (psMethod == "matchOnPs") {

    summaryKM <- summary(
      kaplanMeier,
      times = timePoint
    )

    res <- 1 - summaryKM$surv

  } else if (psMethod == "stratifyByPs") {

    kaplanMeier <- list()
    stratId <- sort(
      unique(
        population$stratumId
      )
    )

    for (i in stratId) {

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
  } else if (psMethod == "inversePtWeighted") {

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




getStandadrdError <- function(summaryKmList){

  sqrt(sum(summaryKmList$std.err^2))
}




getAbsoluteDifference <- function(summaryKMList){
  diff(summaryKMList$surv)
}
