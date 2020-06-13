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
  for (name in names(formals(createPopulationCmSettingsArgs))) {
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
#' @param binary                           Forces the outcomeCount to be 0 or 1 (use for binary
#'                                         prediction problems)
#' @param includeAllOutcomes               (binary) indicating whether to include people with outcomes
#'                                         who are not observed for the whole at risk period
#' @param firstExposureOnly                Should only the first exposure per subject be included? Note
#'                                         thatthis is typically done in the createStudyPopulation
#'                                         function,
#' @param washoutPeriod                    The mininum required continuous observation time prior to
#'                                         indexdate for a person to be included in the cohort.
#' @param removeSubjectsWithPriorOutcome   Remove subjects that have the outcome prior to the risk
#'                                         window start?
#' @param priorOutcomeLookback             How many days should we look back when identifying prior
#'                                         outcomes?
#' @param requireTimeAtRisk                Should subject without time at risk be removed?
#' @param minTimeAtRisk                    The minimum number of days at risk required to be included
#' @param riskWindowStart                  The start of the risk window (in days) relative to the
#'                                         startAnchor.
#' @param startAnchor                      The anchor point for the start of the risk window. Can be
#'                                         "cohort start" or "cohort end".
#' @param addExposureDaysToStart           DEPRECATED: Add the length of exposure the start of the risk
#'                                         window? Use startAnchor instead.
#' @param riskWindowEnd                    The end of the risk window (in days) relative to the
#'                                         endAnchor parameter
#' @param endAnchor                        The anchor point for the end of the risk window. Can be
#'                                         "cohort start" or "cohort end".
#' @param addExposureDaysToEnd             DEPRECATED: Add the length of exposure the risk window? Use
#'                                         endAnchor instead.
#' @param verbosity                        Sets the level of the verbosity. If the log level is at or
#'                                         higher in priority than the logger threshold, a message will
#'                                         print. The levels are:DEBUGHighest verbosity showing all
#'                                         debug statementsTRACEShowing information about start and end
#'                                         of stepsINFOShow informative information (Default)WARNShow
#'                                         warning messagesERRORShow error messagesFATALBe silent
#'                                         except for fatal errors
#'
#' @export
createPopulationPlpSettingsArgs <- function(binary = T,
                                            includeAllOutcomes = T,
                                            firstExposureOnly = FALSE,
                                            washoutPeriod = 0,
                                            removeSubjectsWithPriorOutcome = TRUE,
                                            priorOutcomeLookback = 99999,
                                            requireTimeAtRisk = F,
                                            minTimeAtRisk = 365,
                                            riskWindowStart = 0,
                                            startAnchor = "cohort start",
                                            addExposureDaysToStart = NULL,
                                            riskWindowEnd = 365,
                                            endAnchor = "cohort start",
                                            addExposureDaysToEnd = NULL,
                                            verbosity = "INFO") {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createPopulationPlpSettingsArgs))) {
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
#' @param plpResults             A data frame containig the path to an existing plpResult. The column
#'                               names should be "outcomeId" and "directory".
#' @param minCovariateFraction   The minimum fraction of target population who must have a covariate
#'                               for it to be included in the model training
#' @param normalizeData          Whether to normalise the covariates before training (Default: TRUE)
#' @param modelSettings          An object of class modelSettings created using one of the
#'                               function:setLassoLogisticRegression() A lasso logistic regression
#'                               model setGradientBoostingMachine() A gradient boosting machine
#'                               setAdaBoost() An ada boost model setRandomForest() A random forest
#'                               model setDecisionTree() A decision tree model setCovNN()) A
#'                               convolutional neural network model setCIReNN() A recurrent neural
#'                               network model setMLP() A neural network model setDeepNN() A deep
#'                               neural network model setKNN() A KNN model
#' @param testSplit              Either 'person' or 'time' specifying the type of evaluation
#'                               used.'time' find the date where testFraction of patients had an index
#'                               after the date and assigns patients with an index prior to this date
#'                               into the training set and post the date into the test set'person'
#'                               splits the data into test (1-testFraction of the data) andtrain
#'                               (validationFraction of the data) sets. The split is stratified by the
#'                               class label.
#' @param testFraction           The fraction of the data to be used as the test set in the patient
#'                               split evaluation.
#' @param trainFraction          A real number between 0 and 1 indicating the train set fraction of the
#'                               data. If not set trainFraction is equal to 1 - test
#' @param splitSeed              The seed used to split the test/train set when using a person type
#'                               testSplit
#' @param nfold                  The number of folds used in the cross validation (default 3)
#' @param indexes                A dataframe containing a rowId and index column where the index value
#'                               of -1 means in the test set, and positive integer represents the cross
#'                               validation fold (default is NULL)
#' @param savePlpData            Binary indicating whether to save the plpData object (default is
#'                               FALSE)
#' @param savePlpPlots           Binary indicating whether to save the performance plots as pdf files
#'                               (default is TRUE)
#' @param saveEvaluation         Binary indicating whether to save the oerformance as csv files
#'                               (default is TRUE)
#' @param verbosity              Sets the level of the verbosity. If the log level is at or higher in
#'                               priority than the logger threshold, a message will print. The levels
#'                               are:
#'                               \itemize{
#'                                 \item {DEBUG}{Highest verbosity showing all debug statements}
#'                                 \item {TRACE}{Showing information about start and end of steps}
#'                                 \item {INFO}{Show informative information (Default)}
#'                                 \item {WARN}{Show warning messages}
#'                                 \item {ERROR}{Show error messages}
#'                                 \item {FATAL}{Be silent except for fatal errors}
#'                               }
#'
#'
#'
#' @param timeStamp              If TRUE a timestamp will be added to each logging statement.
#'                               Automatically switched on for TRACE level.
#' @param analysisId             Identifier for the analysis. It is used to create, e.g., the result
#'                               folder. Default is a timestamp.
#'
#' @export

createRunPlpSettingsArgs <- function(plpResults = NULL,
                                     minCovariateFraction = 0.001,
                                     normalizeData = TRUE,
                                     modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
                                     testSplit = "person",
                                     testFraction = 0.25,
                                     trainFraction = NULL,
                                     splitSeed = NULL,
                                     nfold = 3,
                                     indexes = NULL,
                                     savePlpData = FALSE,
                                     savePlpPlots = TRUE,
                                     saveEvaluation = TRUE,
                                     verbosity = "INFO",
                                     timeStamp = FALSE,
                                     analysisId = NULL) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createRunPlpSettingsArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  attr(analysis, "fun") <- "createRunPlpSettingsArgs"
  class(analysis) <- "runPlpSettingsArgs"
  return(analysis)
}




#' Create a settings object for running the analyses Create the settings for running the analyses. The
#' input consists of two parts: 1) the settings for running the prediction algorithms and 2) the
#' settings for estimating treatment effects within strata of predicted risk.
#'
#' @param runPlpSettings   A parameterer object of type \code{runPlpSettingsArgs} defined using the
#'                         function \code{\link[RiskStratifiedEstimation]{createRunPlpSettingsArgs}}.
#' @param runCmSettings    A parameter object of type \code{runCmSettingsArgs} defined using the
#'                         function \code{\link[RiskStratifiedEstimation]{createRunCmSettingsArgs}}
#'
#' @return
#' An R object of type \code{runSettings}
#' @export

createRunSettings <- function(runPlpSettings = createRunPlpSettingsArgs(modelSettings = PatientLevelPrediction::setLassoLogisticRegression()),
                              runCmSettings = createRunCmSettingsArgs()) {

  res <- list(runPlpSettings = runPlpSettings, runCmSettings = runCmSettings)
  attr(res, "fun") <- "createRunSettings"
  class(res) <- "runSettings"
  return(res)

}



#' Create a parameter object for the function getPlpData
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param studyStartDate               A calendar date specifying the minimum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'.
#' @param studyEndDate                 A calendar date specifying the maximum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'. Important: the
#'                                     studyEnd data is also used to truncate risk windows, meaning no
#'                                     outcomes beyond the study end date will be considered.
#' @param firstExposureOnly            Should only the first exposure per subject be included? Note
#'                                     that this is typically done in the createStudyPopulation
#'                                     function, but can already be done here for efficiency reasons.
#' @param washoutPeriod                The mininum required continuous observation time prior to index
#'                                     date for a person to be included in the at risk cohort. Note
#'                                     that this is typically done in the createStudyPopulation
#'                                     function,but can already be done here for efficiency reasons.
#' @param excludeDrugsFromCovariates   A redundant option
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
  attr(analysis, "fun") <- "createGetPlpDataArgs"
  class(analysis) <- "args"
  return(analysis)
}

#' Create a parameter object for the function getDbCohortMethodData
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param studyStartDate               A calendar date specifying the minimum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'.
#' @param studyEndDate                 A calendar date specifying the maximum date that a cohort index
#'                                     date can appear. Date format is 'yyyymmdd'. Important: the study
#'                                     end data is also used to truncate risk windows, meaning no
#'                                     outcomes beyond the study end date will be considered.
#' @param excludeDrugsFromCovariates   Should the target and comparator drugs (and their descendant
#'                                     concepts) be excluded from the covariates? Note that this will
#'                                     work if the drugs are actualy drug concept IDs (and not
#'                                     cohortIDs).
#' @param firstExposureOnly            Should only the first exposure per subject be included? Note
#'                                     that this is typically done in the
#'                                     createStudyPopulationfunction, but can already be done here for
#'                                     efficiency reasons.
#' @param removeDuplicateSubjects      Remove subjects that are in both the target and comparator
#'                                     cohort? See details for allowed values.Note that this is
#'                                     typically done in the createStudyPopulation function, but can
#'                                     already be done here for efficiency reasons.
#' @param restrictToCommonPeriod       Restrict the analysis to the period when both treatments are
#'                                     observed?
#' @param washoutPeriod                The mininum required continuous observation time prior to index
#'                                     date for a person to be included in the cohort. Note that thisis
#'                                     typically done in the createStudyPopulation function, but can
#'                                     already be done here for efficiency reasons.
#' @param maxCohortSize                If either the target or the comparator cohort is larger than
#'                                     this number it will be sampled to this size. maxCohortSize = 0
#'                                     indicates no maximum size.
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
  attr(analysis, "fun") <- "createGetCmDataArgs"
  class(analysis) <- "args"
  return(analysis)
}









#' Create a parameter object for the function createPs
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param excludeCovariateIds       Exclude these covariates from the propensity model.
#' @param includeCovariateIds       Include only these covariates in the propensity model.
#' @param maxCohortSizeForFitting   If the target or comparator cohort are larger than this number,
#'                                  they will be downsampled before fitting the propensity model. The
#'                                  model will be used to compute propensity scores for all subjects.
#'                                  The purpose of the sampling is to gain speed. Setting this number
#'                                  to 0 means no downsampling will be applied.
#' @param errorOnHighCorrelation    If true, the function will test each covariate for correlation with
#'                                  the treatment assignment. If any covariate has an unusually
#'                                  highcorrelation (either positive or negative), this will throw an
#'                                  error.
#' @param stopOnError               If an error occurrs, should the function stop? Else, the two
#'                                  cohorts will be assumed to be perfectly separable.
#' @param prior                     The prior used to fit the model. See createPrior for details.
#' @param control                   The control object used to control the cross-validation used to
#'                                  determine the hyperparameters of the prior (if applicable). See
#'                                  createControl for details.
#'
#' @return
#' A parameter object for creating propensity scores.
#'
#' @export

createCreatePsArgs <- function(excludeCovariateIds = c(),
                               includeCovariateIds = c(),
                               maxCohortSizeForFitting = 250000,
                               errorOnHighCorrelation = TRUE,
                               stopOnError = TRUE,
                               prior = createPrior("laplace",
                                                   exclude = c(0),
                                                   useCrossValidation = TRUE),
                               control = createControl(noiseLevel = "silent",
                                                       cvType = "auto",
                                                       tolerance = 2e-07,
                                                       cvRepetitions = 10,
                                                       startingVariance = 0.01)) {
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
  attr(analysis, "fun") <- "createCreatePsArgs"
  class(analysis) <- "args"
  return(analysis)
}




#' Create a parameter object for running the estimation step Create a parameter object for running the
#' estimation step. This function is used to create part of the input of
#' \code{\link[RiskStratifiedEstimation]{createRunSettings}}.
#'
#' @param psMethod                          How should the propensity scores be used? Can be one of
#'                                          "inversePtWeighted", "stratifyByPs" or "matchOnPs".
#' @param effectEstimationSettings          Parameter object providing further settings for the
#'                                          implementation of selected \code{psMethod} to the
#'                                          estimation process. Can be created using one of
#'                                          \code{\link[RiskStratifiedEstimation]{createCreateIPWArgs}},
#'                                          when \code{inversePtWeighted} is selected,
#'                                          \code{\link[RiskStratifiedEstimation]{createStratifyByPsArgs}}
#'                                          when \code{stratifyByPs} is selected or
#'                                          \code{\link[RiskStratifiedEstimation]{createMatchOnPsArgs}}
#'                                          when \code{matchOnPs} is selected.
#' @param psSettings                        Parameter object for \code{\link[CohortMethod]{createPs}}
#' @param createPsThreads                   The number of parallel threads for the estimation of the
#'                                          propensity scores. Default is 1.
#' @param fitOutcomeModelsThreads           The number of parallel threads for the estimation of the
#'                                          outcome models
#' @param createPsThreadsNegativeControls   The number of parallel threads for the estimation of the
#'                                          negative control outcomes
#' @param estimateOverallResults            Should overall results be estimated? Default is
#'                                          \code{FALSE}
#' @param timePoint                         The time point after cohort start that absolute differences
#'                                          should be estimated.
#' @param riskStrata                        The number of risk strata. Default is 4.
#'
#' @return
#' A parameter object for running the the estimation step.
#' @export

createRunCmSettingsArgs <- function(psMethod = "inversePtWeighted",
                                    effectEstimationSettings = createCreateIPWArgs(),
                                    psSettings = createCreatePsArgs(),
                                    createPsThreads = 1,
                                    fitOutcomeModelsThreads = 1,
                                    createPsThreadsNegativeControls = 1,
                                    estimateOverallResults = FALSE,
                                    timePoint = 365,
                                    riskStrata = 4) {


  res <- list(psMethod = psMethod,
              psSettings = psSettings,
              effectEstimationSettings = effectEstimationSettings,
              createPsThreads = createPsThreads,
              fitOutcomeModelsThreads = fitOutcomeModelsThreads,
              createPsThreadsNegativeControls = createPsThreadsNegativeControls,
              estimateOverallResults = estimateOverallResults,
              riskStrata = riskStrata,
              timePoint = timePoint)

  class(res) <- "args"

  return(res)

}




#' Create a parameter object for the function stratifyByPs
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param numberOfStrata          How many strata? The boundaries of the strata are automatically
#'                                defined to contain equal numbers of target persons.
#' @param stratificationColumns   Names of one or more columns in the data data.frame on which subjects
#'                                should also be stratified in addition to stratification on propensity
#'                                score.
#' @param baseSelection           What is the base selection of subjects where the strata bounds areto
#'                                be determined? Strata are defined as equally-sized strata inside this
#'                                selection. Possible values are "all", "target", and "comparator".
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
#' @param caliper                 The caliper for matching. A caliper is the distance which is
#'                                acceptable for any match. Observations which are outside of the
#'                                caliper are dropped. A caliper of 0 means no caliper is used.
#' @param caliperScale            The scale on which the caliper is defined. Three scales are
#'                                supported: caliperScale = 'propensity score', caliperScale
#'                                ='standardized', or caliperScale = 'standardized logit'. On the
#'                                standardized scale, the caliper is interpreted in standard deviations
#'                                of the propensity score distribution. 'standardized logit'is similar,
#'                                except that the propensity score is transformed to the logitscale
#'                                because the PS is more likely to be normally distributed on that
#'                                scale(Austin, 2011).
#' @param maxRatio                The maximum number of persons int the comparator arm to be matched to
#'                                each person in the treatment arm. A maxRatio of 0 means no
#'                                maximum:all comparators will be assigned to a target person.
#' @param stratificationColumns   Names or numbers of one or more columns in the data data.frameon
#'                                which subjects should be stratified prior to matching. No persons
#'                                will be matched with persons outside of the strata identified by the
#'                                values in these columns.
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




#' Create a parameter object for the function createIPW Create an object defining the parameter
#' values.
#'
#' @param weightsType            The type of the weights to be used. Allowed options are 'ATE' for
#'                               average treatment effect and 'ATT' for average treatment effect on the
#'                               treated weights
#' @param useStabilizedWeights   Should stabilized weights be used?
#' @param truncationLevels       The level of truncation expressed in percentiles of the propensity
#'                               score.
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




#' Create a parameter defining the performed risk stratified analysis
#'
#'
#' @param analysisId           The analysis ID.
#' @param databaseName         The name of the database.
#' @param analysisType         The type of the analysis. Could be "matching", "stratifyByPs" or
#'                             "inversePtWeighted".
#' @param treatmentCohortId    The cohort definition id of the treatment cohort in the cohortTable.
#' @param comparatorCohortId   The cohort definition id of the comparator cohort in the cohortTable.
#' @param outcomeIds           The cohort definition ids of the outcome cohorts in the outcomeTable.
#' @param analysisMatrix       Boolean matrix defining the outcomes to be assessed (rows) within risk
#'                             strata (columns). The order in columns should match the the order of
#'                             \code{outcomeIds}. Default is the diagonal matrix, which leads to the
#'                             risk stratified assessment of only the outcome for which the risk strata
#'                             were defined.
#' @param mapTreatments        Dataframe containing 2 columns: "exposure_id" with the id numbers of the
#'                             treatment and comparator cohorts and "exposure_name" the cohort names.
#' @param mapOutcomes          Dataframe containing 2 columns: "outcome_id" with the cohort names of the
#'                             outcomes of interest and "outcome_name" with their names.
#' @param verbosity            Sets the level of the verbosity. If the log level is at or higher in
#'                             priority than the logger threshold, a message will print. The levels
#'                             are:
#'                             \itemize{
#'                               \item {DEBUG}{Highest verbosity showing all debug statements}
#'                               \item {TRACE}{Showing information about start and end of steps}
#'                               \item {INFO}{Show informative information (Default)}
#'                               \item {WARN}{Show warning messages}
#'                               \item {ERROR}{Show error messages}
#'                               \item {FATAL}{Be silent except for fatal errors}
#'                             }
#'
#'
#'
#' @param saveDirectory        The directory name where the results of the analyses will be stored.
#' @return
#' An analysisSettings object providing the identification information of the analysis.
#'
#' @export

createAnalysisSettings <- function(analysisId = NULL,
                                   databaseName,
                                   analysisType,
                                   treatmentCohortId,
                                   comparatorCohortId,
                                   outcomeIds,
                                   analysisMatrix = diag(length(outcomeIds)),
                                   mapTreatments,
                                   mapOutcomes,
                                   verbosity = NULL,
                                   saveDirectory = NULL) {

  res <- list(analysisId = analysisId,
              databaseName = databaseName,
              analysisType = analysisType,
              treatmentCohortId = treatmentCohortId,
              comparatorCohortId = comparatorCohortId,
              outcomeIds = outcomeIds,
              analysisMatrix = analysisMatrix,
              mapTreatments = mapTreatments,
              mapOutcomes = mapOutcomes,
              verbosity = verbosity,
              saveDirectory = saveDirectory)

  attr(res, "fun") <- "createAnalysisSettings"
  class(res) <- "analysisSettings"

  return(res)

}




#' Create parameter object for extracting the data
#'
#'
#'
#' @param getPlpDataSettings       Parameter object for the extraction of the \code{plpData} object
#'                                 created from
#'                                 \code{\link[RiskStratifiedEstimation]{createGetPlpDataArgs}}.
#' @param getCmDataSettings        Parameter object for the extraction of the \code{cohortMethodData}
#'                                 object created from
#'                                 \code{\link[RiskStratifiedEstimation]{createGetPlpDataArgs}}.
#' @param plpDataFolder            The directory path where the \code{plpData} object has already been
#'                                 saved locally.
#' @param cohortMethodDataFolder   The directory path where the \code{cohortMethodData} object has
#'                                 already been saved locally.
#'
#' @export

createGetDataSettings <- function(getPlpDataSettings = createGetPlpDataArgs(),
                                  getCmDataSettings = createGetCmDataArgs(),
                                  plpDataFolder = NULL,
                                  cohortMethodDataFolder = NULL) {

  res <- list(getPlpDataSettings = getPlpDataSettings,
              getCmDataSettings = getCmDataSettings,
              plpDataFolder = plpDataFolder,
              cohortMethodDataFolder = cohortMethodDataFolder)

  attr(res, "fun") <- "createGetDataSettings"
  class(res) <- "getDataSettings"

  return(res)
}




#' Create parameter object for defining the analysis populations Contains the settings for defining
#' both the \code{populationPlp} and the \code{populationCm} objects.
#
#' @param populationPlpSettings   Parameter object for the definition of the \code{populationPlp}
#'                                object created from
#'                                \code{\link[RiskStratifiedEstimation]{createPopulationPlpSettingsArgs}}.
#' @param populationCmSettings    Parameter object for the definition of the \code{populationCm} object
#'                                created from
#'                                \code{\link[RiskStratifiedEstimation]{createPopulationCmSettingsArgs}}.
#'
#' @export

createPopulationSettings <- function(populationPlpSettings = createPopulationPlpSettingsArgs(),
                                     populationCmSettings = createPopulationCmSettingsArgs()) {

  res <- list(populationPlpSettings = populationPlpSettings,
              populationCmSettings = populationCmSettings)


  attr(res, "fun") <- "createPopulationSettings"
  class(res) <- "populationSettings"

  return(res)

}




#' Create parameter object for database to be reached
#'
#' @param cdmVersion               Define the OMOP CDM version
#' @param cdmDatabaseSchema        The name of the database schema that contains the OMOP CDM instance.
#'                                 Requires read permissions to this database. On SQL Server, this
#'                                 should specify both the database and the schema, so for example
#'                                 "cdm_instance.dbo"
#' @param cohortDatabaseSchema     The name of the database schema that is the location where the
#'                                 cohort data used to define the at risk cohort is available. If
#'                                 cohortTable = DRUG_ERA, \code{cohortDatabaseSchema} is not used by
#'                                 assumed to be \code{cdmDatabaseSchema}.
#' @param outcomeDatabaseSchema    The name of the database schema that is the location where the data
#'                                 used to define the outcome cohorts is available. If cohortTable =
#'                                 CONDITION_ERA, exposureDatabaseSchema is not used by assumed to be
#'                                 cdmSchema. Requires read permissions to this database.
#' @param resultsDatabaseSchema    The name of the database schema with write permissions.
#' @param exposureDatabaseSchema   Input of function \code{\link[CohortMethod]{getDbCohortMethodData}}:
#'                                 The name of the database schema that is the location where the
#'                                 exposure data used to define the exposure cohorts is available.
#' @param cohortTable              The tablename that contains the at risk cohort. If cohortTable <>
#'                                 DRUG_ERA, then expectation is cohortTable has format of COHORT
#'                                 table: cohort_concept_id, SUBJECT_ID, COHORT_START_DATE,
#'                                 COHORT_END_DATE.
#' @param outcomeTable             The tablename that contains the outcome cohorts. If outcomeTable <>
#'                                 CONDITION_OCCURRENCE, then expectation is outcomeTable has format of
#'                                 COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                 COHORT_END_DATE.
#' @param exposureTable            The tablename that contains the exposure cohorts. If exposureTable
#'                                 <> DRUG_ERA, then expectation is exposureTable has format of COHORT
#'                                 table: cohort_concept_id, SUBJECT_ID, COHORT_START_DATE,
#'                                 COHORT_END_DATE.
#' @param mergedCohortTable        The name of the table where the merged treatment and comparator
#'                                 cohorts will be stored.
#' @param targetCohortId           The cohort definition id of of the merged cohort in the
#'                                 \code{mergedCohortTable}.
#'
#' @export

createDatabaseSettings <- function(cdmVersion = "5",
                                   cdmDatabaseSchema,
                                   cohortDatabaseSchema = cdmDatabaseSchema,
                                   outcomeDatabaseSchema = cdmDatabaseSchema,
                                   resultsDatabaseSchema = cdmDatabaseSchema,
                                   exposureDatabaseSchema = cdmDatabaseSchema,
                                   cohortTable,
                                   outcomeTable,
                                   exposureTable = "drug_era",
                                   mergedCohortTable,
                                   targetCohortId = 1) {

  res <- list(cdmVersion = cdmVersion,
              cdmDatabaseSchema = cdmDatabaseSchema,
              cohortDatabaseSchema = cohortDatabaseSchema,
              outcomeDatabaseSchema = outcomeDatabaseSchema,
              resultsDatabaseSchema = resultsDatabaseSchema,
              exposureDatabaseSchema = exposureDatabaseSchema,
              cohortTable = cohortTable,
              outcomeTable = outcomeTable,
              exposureTable = exposureTable,
              mergedCohortTable = mergedCohortTable,
              targetCohortId = targetCohortId)


  attr(res, "fun") <- "createDatabaseSettings"
  class(res) <- "databaseSettings"

  return(res)

}




#' Create the parameter object for extracting the covariates Contains the arguments for the extraction
#' of both the covariates related to the prediction step and the estimation step.
#'
#' @param covariateSettingsCm    The covariate settings object related to the estimation step created
#'                               from \code{\link[FeatureExtraction]{createCovariateSettings}}.
#' @param covariateSettingsPlp   The covariate settings objec related to the prediction step created
#'                               from \code{\link[FeatureExtraction]{createCovariateSettings}}.
#'
#' @export

createGetCovariateSettings <- function(covariateSettingsCm = FeatureExtraction::createCovariateSettings(),
                                       covariateSettingsPlp = FeatureExtraction::createCovariateSettings()) {

  res <- list(covariateSettingsCm = covariateSettingsCm,
              covariateSettingsPlp = covariateSettingsPlp)

  return(res)

}
