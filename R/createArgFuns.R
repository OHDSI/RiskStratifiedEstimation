#' Create a parameter object for the function createStudyPopulation
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param population                                 If specified, this population will be used as the startingpoint
#'                                                   instead of the cohorts in the cohortMethodDataobject.
#' @param firstExposureOnly                          Should only the first exposure per subject be included? Note that
#'                                                   this is typically done in thecreateStudyPopulation function,
#' @param restrictToCommonPeriod                     Restrict the analysis to the period when both exposures are
#'                                                   observed?
#' @param washoutPeriod                              The mininum required continuous observation time prior to
#'                                                   index date for a person to be included in the cohort.
#' @param removeDuplicateSubjects                    Remove subjects that are in both the target and comparator cohort?
#'                                                   See details for allowed values.
#' @param removeSubjectsWithPriorOutcome             Remove subjects that have the outcome prior to the risk window
#'                                                   start?
#' @param priorOutcomeLookback                       How many days should we look back when identifying prioroutcomes?
#' @param minDaysAtRisk                              The minimum required number of days at risk.
#' @param riskWindowStart                            The start of the risk window (in days) relative to the indexdate
#'                                                   (+ days of exposure if theaddExposureDaysToStart parameter is
#'                                                   specified).
#' @param addExposureDaysToStart                     Add the length of exposure the start of the risk window?
#' @param riskWindowEnd                              The end of the risk window (in days) relative to the index data
#'                                                   (+ days of exposure if the addExposureDaysToEndparameter is
#'                                                   specified).
#' @param addExposureDaysToEnd                       Add the length of exposure the risk window?
#' @param censorAtNewRiskWindow                      If a subject is in multiple cohorts, should time-at-risk be
#'                                                   censored when the new time-at-risk starts to prevent overlap?
#'
#' @export

createStudyPopulationCmSettings <-
  function(population = NULL,
           firstExposureOnly = FALSE,
           restrictToCommonPeriod = FALSE,
           washoutPeriod = 0,
           removeDuplicateSubjects = FALSE,
           removeSubjectsWithPriorOutcome = TRUE,
           priorOutcomeLookback = 99999,
           minDaysAtRisk = 1,
           riskWindowStart = 0,
           addExposureDaysToStart = FALSE,
           riskWindowEnd = 0,
           addExposureDaysToEnd = TRUE,
           censorAtNewRiskWindow = FALSE) {


    # First: get default values:
    analysis <- list()
    for (name in names(formals(createStudyPopulationCmSettings))) {
      analysis[[name]] <- get(name)
    }
    # Second: overwrite defaults with actual values:
    values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
    for (name in names(values)) {
      if (name %in% names(analysis))
        analysis[[name]] <- values[[name]]
    }
    class(analysis) <- "args"
    return(analysis)


  }




#' Create a parameter object for the function runPlp
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param minCovariateFraction            The minimum fraction of target population who must have a covariate for it to
#'                                        be included in the model training
#' @param normalizeData                   Whether to normalise the covariates before training (Default: TRUE)
#' @param modelSettings                   An object of class modelSettings created using one of the
#'                                        function:setLassoLogisticRegression() A lasso logistic regression model
#'                                        setGradientBoostingMachine() A gradient boosting machine
#'                                        setAdaBoost() An ada boost model setRandomForest() A random forest model
#'                                        setDecisionTree() A decision tree model
#'                                        setCovNN()) A convolutional neural network model setCIReNN() A recurrent
#'                                        neural network model setMLP() A neural network model setDeepNN() A deep
#'                                        neural network model setKNN() A KNN model
#' @param testSplit                       Either 'person' or 'time' specifying the type of evaluation used.'time' find
#'                                        the date where testFraction of patients had an index after the date and
#'                                        assigns patients with an index prior to this date into the training set and
#'                                        post the date into the test set'person' splits the data into test
#'                                        (1-testFraction of the data) andtrain (validationFraction of the data) sets.
#'                                        The split is stratified by the class label.
#' @param testFraction                    The fraction of the data to be used as the test set in the patient split
#'                                        evaluation.
#' @param trainFraction                   A real number between 0 and 1 indicating the train set fraction of the data.
#'                                        If not set trainFraction is equal to 1 - test
#' @param splitSeed                       The seed used to split the test/train set when using a person type testSplit
#' @param nfold                           The number of folds used in the cross validation (default 3)
#' @param indexes                         A dataframe containing a rowId and index column where the index value of
#'                                        -1 means in the test set, and positive integer represents the cross
#'                                        validation fold (default is NULL)
#' @param savePlpData                     Binary indicating whether to save the plpData object (default is T)
#' @param savePlpResult                   Binary indicating whether to save the object returned by runPlp
#'                                        (default is T)
#' @param savePlpPlots                    Binary indicating whether to save the performance plots as pdf files
#'                                        (default is T)
#' @param saveEvaluation                  Binary indicating whether to save the oerformance as csv files (default is T)
#' @param verbosity                       Sets the level of the verbosity. If the log level is at or higher in priority
#'                                        than the logger threshold, a message will print. The levels are:
#'                                        \itemize{
#'                                               \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                               \item{TRACE}{Showing information about start and end of steps}
#'                                               \item{INFO}{Show informative information (Default)}
#'                                               \item{WARN}{Show warning messages}
#'                                               \item{ERROR}{Show error messages}
#'                                               \item{FATAL}{Be silent except for fatal errors}}
#' @param timeStamp                       If TRUE a timestamp will be added to each logging statement. Automatically
#'                                        switched on for TRACE level.
#' @param analysisId                      Identifier for the analysis. It is used to create, e.g., the result folder.
#'                                        Default is a timestamp.
#'
#' @export

createRunPlpArgs <- function(minCovariateFraction = 0.001,
                             normalizeData = T,
                             modelSettings,
                             testSplit = "time",
                             testFraction = 0.25,
                             trainFraction = NULL,
                             splitSeed = NULL,
                             nfold = 3,
                             indexes = NULL,
                             savePlpData = T,
                             savePlpResult = T,
                             savePlpPlots = T,
                             saveEvaluation = T,
                             verbosity = "INFO",
                             timeStamp = FALSE,
                             analysisId = NULL) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createRunPlpArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "args"
  return(analysis)
}




createRunSettings <- function(runPlpSettings =
                                createRunPlpArgs(modelSettings =
                                                   PatientLevelPrediction::setLassoLogisticRegression()),
                              runCmSettings =
                                createRunCmArgs()){

  res <- list(runPlpSettings = runPlpSettings,
              runCmSettings = runCmSettings)

  return(res)

}



#' Create a parameter object for the function getPlpData
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param studyStartDate                       A calendar date specifying the minimum date that a cohort index date can
#'                                             appear. Date format is 'yyyymmdd'.
#' @param studyEndDate                         A calendar date specifying the maximum date that a cohort index date can
#'                                             appear. Date format is 'yyyymmdd'. Important: the studyend data is also
#'                                             used to truncate risk windows, meaning no outcomes beyond the study end
#'                                             date will be considered.
#' @param firstExposureOnly                    Should only the first exposure per subject be included? Note that this is
#'                                             typically done in the createStudyPopulation function, but can already be
#'                                             done here for efficiency reasons.
#' @param washoutPeriod                        The mininum required continuous observation time prior to index date for
#'                                             a person to be included in the at risk cohort. Note that this is
#'                                             typically done in the createStudyPopulation function,but can already be
#'                                             done here for efficiency reasons.
#' @param excludeDrugsFromCovariates           A redundant option
#'
#' @export
createGetPlpDataArgs <- function(studyStartDate = "",
                                 studyEndDate = "",
                                 firstExposureOnly = FALSE,
                                 washoutPeriod = 0,
                                 excludeDrugsFromCovariates = FALSE) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createGetPlpDataArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "args"
  return(analysis)
}

#' Create a parameter object for the function getDbCohortMethodData
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param studyStartDate                      A calendar date specifying the minimum date that a cohort index date can
#'                                            appear. Date format is 'yyyymmdd'.
#' @param studyEndDate                        A calendar date specifying the maximum date that a cohort index date can
#'                                            appear. Date format is 'yyyymmdd'. Important: the study end data is also
#'                                            used to truncate risk windows, meaning no outcomes beyond the study end
#'                                            date will be considered.
#' @param excludeDrugsFromCovariates          Should the target and comparator drugs (and their descendant concepts) be
#'                                            excluded from the covariates? Note that this will work if the drugs are
#'                                            actualy drug concept IDs (and not cohortIDs).
#' @param firstExposureOnly                   Should only the first exposure per subject be included? Note that this is
#'                                            typically done in the createStudyPopulationfunction, but can already be
#'                                            done here for efficiency reasons.
#' @param removeDuplicateSubjects             Remove subjects that are in both the target and comparator cohort? See
#'                                            details for allowed values.Note that this is typically done in the
#'                                            createStudyPopulation function, but can already be done here for
#'                                            efficiency reasons.
#' @param restrictToCommonPeriod              Restrict the analysis to the period when both treatments are observed?
#' @param washoutPeriod                       The mininum required continuous observation time prior to index date for
#'                                            a person to be included in the cohort. Note that thisis typically done in
#'                                            the createStudyPopulation function, but can already be done here for
#'                                            efficiency reasons.
#' @param maxCohortSize                       If either the target or the comparator cohort is larger than this number
#'                                            it will be sampled to this size. maxCohortSize = 0 indicates no maximum
#'                                            size.
#'
#' @export
createGetCmDataArgs <- function(studyStartDate = "",
                                studyEndDate = "",
                                excludeDrugsFromCovariates = TRUE,
                                firstExposureOnly = FALSE,
                                removeDuplicateSubjects = FALSE,
                                restrictToCommonPeriod = FALSE,
                                washoutPeriod = 0,
                                maxCohortSize = 0) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createGetCmDataArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "args"
  return(analysis)
}




#' Create a parameter object for the function createStudyPopulation
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param firstExposureOnly                Should only the first exposure per subject be included?
#'                                         Notethat this is typically done in thecreateStudyPopulation
#'                                         function,
#' @param restrictToCommonPeriod           Restrict the analysis to the period when both exposures are
#'                                         observed?
#' @param washoutPeriod                    The mininum required continuous observation time prior
#'                                         toindex date for a person to be included in the cohort.
#' @param removeDuplicateSubjects          Remove subjects that are in both the target and
#'                                         comparatorcohort? See details for allowed values.
#' @param removeSubjectsWithPriorOutcome   Remove subjects that have the outcome prior to the
#'                                         riskwindow start?
#' @param priorOutcomeLookback             How many days should we look back when identifying
#'                                         prioroutcomes?
#' @param minDaysAtRisk                    The minimum required number of days at risk.
#' @param riskWindowStart                  The start of the risk window (in days) relative to the
#'                                         startAnchor.
#' @param addExposureDaysToStart           DEPRECATED: Add the length of exposure the start of the risk
#'                                         window?Use startAnchor instead.
#' @param startAnchor                      The anchor point for the start of the risk window. Can be
#'                                         "cohort start"or "cohort end".
#' @param riskWindowEnd                    The end of the risk window (in days) relative to the
#'                                         endAnchor.
#' @param addExposureDaysToEnd             DEPRECATED: Add the length of exposure the risk window?Use
#'                                         endAnchor instead.
#' @param endAnchor                        The anchor point for the end of the risk window. Can be
#'                                         "cohort start"or "cohort end".
#' @param censorAtNewRiskWindow            If a subject is in multiple cohorts, should time-at-risk be
#'                                         censoredwhen the new time-at-risk starts to prevent overlap?
#'
#' @export

createPopulationCmSettingsArgs <- function(firstExposureOnly = FALSE,
                                           restrictToCommonPeriod = FALSE,
                                           washoutPeriod = 0,
                                           removeDuplicateSubjects = FALSE,
                                           removeSubjectsWithPriorOutcome = TRUE,
                                           priorOutcomeLookback = 99999,
                                           minDaysAtRisk = 1,
                                           riskWindowStart = 0,
                                           addExposureDaysToStart = NULL,
                                           startAnchor = "cohort start",
                                           riskWindowEnd = 0,
                                           addExposureDaysToEnd = NULL,
                                           endAnchor = "cohort end",
                                           censorAtNewRiskWindow = FALSE) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createCreateStudyPopulationArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "args"
  return(analysis)
}





#' create the study population settings
#'
#' @details
#' Takes as input the inputs to create study population
#' @param binary                                 Forces the outcomeCount to be 0 or 1 (use for binary prediction
#'                                               problems)
#' @param includeAllOutcomes                     (binary) indicating whether to include people with outcomes who are
#'                                               not observed for the whole at risk period
#' @param firstExposureOnly                      Should only the first exposure per subject be included? Note that
#'                                               this is typically done in the \code{createStudyPopulation} function,
#' @param washoutPeriod                          The mininum required continuous observation time prior to index
#'                                               date for a person to be included in the cohort.
#' @param removeSubjectsWithPriorOutcome         Remove subjects that have the outcome prior to the risk window start?
#' @param priorOutcomeLookback                   How many days should we look back when identifying prior outcomes?
#' @param requireTimeAtRisk                      Should subject without time at risk be removed?
#' @param minTimeAtRisk                          The minimum number of days at risk required to be included
#' @param riskWindowStart                        The start of the risk window (in days) relative to the index date (+
#'                                               days of exposure if the \code{addExposureDaysToStart} parameter is
#'                                               specified).
#' @param addExposureDaysToStart                 Add the length of exposure the start of the risk window?
#' @param riskWindowEnd                          The end of the risk window (in days) relative to the index data (+
#'                                               days of exposure if the \code{addExposureDaysToEnd} parameter is
#'                                               specified).
#' @param addExposureDaysToEnd                   Add the length of exposure the risk window?
#' @param verbosity                              Sets the level of the verbosity. If the log level is at or higher in
#'                                               priority than the logger threshold, a message will print.
#'                                               The levels are:
#'                                               \itemize{
#'                                                 \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                                 \item{TRACE}{Showing information about start and end of steps}
#'                                                 \item{INFO}{Show informative information (Default)}
#'                                                 \item{WARN}{Show warning messages}
#'                                                 \item{ERROR}{Show error messages}
#'                                                 \item{FATAL}{Be silent except for fatal errors}
#'                                                }
#' @return
#' A list containing all the settings required for creating the study population
#' @export

createPopulationPlpSettingsArgs <- function(binary = T,
                                            includeAllOutcomes = T,
                                            firstExposureOnly = FALSE,
                                            washoutPeriod = 0,
                                            removeSubjectsWithPriorOutcome = TRUE,
                                            priorOutcomeLookback = 99999,
                                            requireTimeAtRisk = T,
                                            minTimeAtRisk=364,
                                            riskWindowStart = 1,
                                            addExposureDaysToStart = FALSE,
                                            riskWindowEnd = 365,
                                            addExposureDaysToEnd = F,
                                            verbosity = "INFO"){

  result <- list(binary = binary,
                 includeAllOutcomes = includeAllOutcomes,
                 firstExposureOnly = firstExposureOnly,
                 washoutPeriod = washoutPeriod,
                 removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                 priorOutcomeLookback = priorOutcomeLookback,
                 requireTimeAtRisk = requireTimeAtRisk,
                 minTimeAtRisk = minTimeAtRisk,
                 riskWindowStart = riskWindowStart,
                 addExposureDaysToStart = addExposureDaysToStart,
                 riskWindowEnd = riskWindowEnd,
                 addExposureDaysToEnd = addExposureDaysToEnd,
                 verbosity = verbosity)

  class(result) <- 'populationSettings'
  return(result)

}




#' Create a parameter object for the function createPs
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param excludeCovariateIds             Exclude these covariates from the propensity model.
#' @param includeCovariateIds             Include only these covariates in the propensity model.
#' @param maxCohortSizeForFitting         If the target or comparator cohort are larger than this number,
#'                                        they will be downsampled before fitting the propensity model. The model will
#'                                        be used to compute propensity scores for all subjects. The purpose of
#'                                        the sampling is to gain speed. Setting this number to 0 means no
#'                                        downsampling will be applied.
#' @param errorOnHighCorrelation          If true, the function will test each covariate for correlation with the
#'                                        treatment assignment. If any covariate has an unusually highcorrelation
#'                                        (either positive or negative), this will throw an error.
#' @param stopOnError                     If an error occurrs, should the function stop? Else, the two cohorts will be
#'                                        assumed to be perfectly separable.
#' @param prior                           The prior used to fit the model. See createPrior for details.
#' @param control                         The control object used to control the cross-validation used to determine the
#'                                        hyperparameters of the prior (if applicable). See createControl for details.
#'
#' @export

createCreatePsArgs <- function(excludeCovariateIds = c(),
                               includeCovariateIds = c(),
                               maxCohortSizeForFitting = 250000,
                               errorOnHighCorrelation = TRUE,
                               stopOnError = TRUE,
                               prior = createPrior("laplace", exclude = c(0), useCrossValidation = TRUE),
                               control = createControl(noiseLevel = "silent", cvType = "auto", tolerance = 2e-07,
                                                       cvRepetitions = 10, startingVariance = 0.01)) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createCreatePsArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "args"
  return(analysis)
}




createRunCmArgs <- function(psMethod = "inversePtWeighted",
                            effectEstimationSettings = createCreateIPWArgs(),
                            psSettings = createCreatePsArgs(),
                            createPsThreads = 1,
                            fitOutcomeModelsThreads = 1,
                            estimateOverallResults = FALSE,
                            timePoint = 365){


  res <- list(psMethod = psMethod,
              psSettings = psSettings,
              effectEstimationSettings = effectEstimationSettings,
              createPsThreads = createPsThreads,
              fitOutcomeModelsThreads = fitOutcomeModelsThreads,
              estimateOverallResults = estimateOverallResults)

  return(res)

}




#' Create a parameter object for the function stratifyByPs
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param numberOfStrata                How many strata? The boundaries of the strata are automatically defined to
#'                                      contain equal numbers of target persons.
#' @param stratificationColumns         Names of one or more columns in the data data.frame on which subjects should
#'                                      also be stratified in addition to stratification on propensity score.
#' @param baseSelection                 What is the base selection of subjects where the strata bounds areto be
#'                                      determined? Strata are defined as equally-sized strata inside this selection.
#'                                      Possible values are "all", "target", and "comparator".
#'
#' @export

createStratifyByPsArgs <- function(numberOfStrata = 5,
                                   stratificationColumns = c(),
                                   baseSelection = "all") {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createStratifyByPsArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "args"
  return(analysis)
}




#' Create a parameter object for the function matchOnPs
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param caliper                        The caliper for matching. A caliper is the distance which is acceptable for any
#'                                       match. Observations which are outside of the caliper are dropped. A caliper of
#'                                       0 means no caliper is used.
#' @param caliperScale                   The scale on which the caliper is defined. Three scales are supported:
#'                                       caliperScale = 'propensity score', caliperScale ='standardized', or
#'                                       caliperScale = 'standardized logit'. On the standardized scale, the caliper is
#'                                       interpreted in standard deviations of the propensity score distribution.
#'                                       'standardized logit'is similar, except that the propensity score is transformed
#'                                       to the logitscale because the PS is more likely to be normally distributed on
#'                                       that scale(Austin, 2011).
#' @param maxRatio                       The maximum number of persons int the comparator arm to be matched to each
#'                                       person in the treatment arm. A maxRatio of 0 means no maximum:all comparators
#'                                       will be assigned to a target person.
#' @param stratificationColumns          Names or numbers of one or more columns in the data data.frameon which subjects
#'                                       should be stratified prior to matching. No persons will be matched with persons
#'                                       outside of the strata identified by the values in these columns.
#'
#' @export

createMatchOnPsArgs <- function(caliper = 0.2,
                                caliperScale = "standardized logit",
                                maxRatio = 1,
                                stratificationColumns = c()) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createMatchOnPsArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "args"
  return(analysis)
}




#' Create a parameter object for the function createIPW
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param weightsType                The type of the weights to be used. Allowed options are 'ATE' for average treatment
#'                                   effect and 'ATT' for average treatment effect on the treated weights
#' @param useStabilizedWeights       Should stabilized weights be used?
#' @param truncationLevels           The level of truncation expressed in percentiles of the propensity score.
#'
#' @export

createCreateIPWArgs <- function(weightsType = "ATE",
                                useStabilizedWeights = TRUE,
                                truncationLevels = c(0.01, 0.99)) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createCreateIPWArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  class(analysis) <- "args"
  return(analysis)
}




createAnalysisSettings <- function(analysisId = NULL,
                                   treatmentCohortId,
                                   comparatorCohortId,
                                   outcomeIds,
                                   analysisMatrix = diag(length(outcomeIds)),
                                   verbosity = NULL,
                                   saveDirectory = NULL){

  res <- list(analysisId = analysisId,
              treatmentCohortId = treatmentCohortId,
              comparatorCohortId = comparatorCohortId,
              outcomeIds = outcomeIds,
              analysisMatrix = analysisMatrix,
              verbosity = verbosity,
              saveDirectory = saveDirectory)

  return(res)

}




# Make sure saved data can be loaded!!!!
createGetDataSettings <- function(getPlpDataSettings = createGetPlpDataArgs(),
                                  getCmDataSettings = createGetCmDataArgs(),
                                  plpDataFolder = NULL,
                                  cohortMethodDataFolder = NULL){

  res <- list(getPlpDataSettings = getPlpDataSettings,
              getCmDataSettings = getCmDataSettings,
              plpDataFolder = plpDataFolder,
              cohortMethodDataFolder = cohortMethodDataFolder)

  return(res)
}




createPopulationSettings <- function(populationPlpSettings = createPopulationPlpSettingsArgs(),
                                     populationCmSettings = createPopulationCmSettingsArgs()){

  res <- list(populationPlpSettings = populationPlpSettings,
              populationCmSettings = populationCmSettings)

  return(res)

}




createDatabaseSettings <- function(cdmVersion = "5",
                                   cdmDatabaseschema,
                                   cohortDatabaseSchema = cdmDatabaseschema,
                                   outcomeDatabasesSchema = cdmDatabaseschema,
                                   resultsDatabaseSchema = cdmDatabaseschema,
                                   exposureDatabaseSchema = cdmDatabaseschema,
                                   cohortTable,
                                   outcomeTable,
                                   mergedCohortTable,
                                   attributeDefinitionTable,
                                   cohortAttributeTable,
                                   targetCohortId = 1){

  res <- list(cdmVersion = cdmVersion,
              cdmDatabaseschema = cdmDatabaseschema,
              cohortDatabaseSchema = cohortDatabaseSchema,
              outcomeDatabasesSchema = outcomeDatabasesSchema,
              resultsDatabaseSchema = resultsDatabaseSchema,
              exposureDatabaseSchema = exposureDatabaseSchema,
              cohortTable = cohortTable,
              outcomeTable = outcomeTable,
              mergedCohortTable = mergedCohortTable,
              attributeDefinitionTable = attributeDefinitionTable,
              cohortAttributeTable = cohortAttributeTable,
              targetCohortId = targetCohortId)

  return(res)

}




getCovariateSettings <- function(covariateSettingsCm =
                                   FeatureExtraction::createCovariateSettings(),
                                 covariateSettingsPlp =
                                   FeatureExtraction::createCovariateSettings()){

  res <- list(covariateSettingsCm = covariateSettingsCm,
              covariateSettingsPlp = covariateSettingsPlp)

  return(res)

}
