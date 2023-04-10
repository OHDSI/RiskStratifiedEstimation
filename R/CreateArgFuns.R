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


#' Create a parameter object for the function runPlp
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param outcomeId              The outcomeId for the prediction.
#' @param splitSettings          An object of type splitSettings that specifies
#'                               how to split the data into train/validation/test.
#'                               The default settings can be created using
#'                               \code{\link[PatientLevelPrediction]{createDefaultSplitSetting}}
#' @param sampleSettings         An object of type \code{sampleSettings} that specifies any
#'                               under/over sampling to be done. Should be created with
#'                               \code{\link[PatientLevelPrediction]{createSampleSettings}}
#' @param featureEngineeringSettings An object of \code{featureEngineeringSettings} specifying
#'                                   any feature engineering to be learned (using the train data)
#' @param preprocessSettings      An object of \code{preprocessSettings}. This setting specifies
#'                                the minimum fraction of target population who must have
#'                                a covariate for it to be included in the model training
#'                                and whether to normalise the covariates before training.
#'                                Should be created with
#'                                \code{\link[PatientLevelPrediction]{createPreprocessSettings}}.
#' @param modelSettings           An object of class \code{modelSettings}.
#' @param logSettings             An object of \code{logSettings} created using
#'                                \code{\link[PatientLevelPrediction]{createLogSettings}} specifying
#'                                how the logging is done.
#' @param executeSettings         An object of type \code{executeSettings} specifying which parts
#'                                of the analysis to run. Should be created using
#'                                \code{\link[PatientLevelPrediction]{createExecuteSettings}}.
#' @param matchingSettings       The settings for the construction of the population on which the
#'                               prediction model will be developed.
#' @param timepoint              The timepoint to predict risk (survival models only)
#'
#' @export
#'
createRunPlpAnalysesArgs <- function(
    outcomeId                  = NULL,
    splitSettings              = PatientLevelPrediction::createDefaultSplitSetting(),
    sampleSettings             = PatientLevelPrediction::createSampleSettings(),
    featureEngineeringSettings = PatientLevelPrediction::createFeatureEngineeringSettings(),
    preprocessSettings         = PatientLevelPrediction::createPreprocessSettings(),
    modelSettings              = PatientLevelPrediction::setLassoLogisticRegression(),
    logSettings                = PatientLevelPrediction::createLogSettings(),
    executeSettings            = PatientLevelPrediction::createDefaultExecuteSettings(),
    matchingSettings           = CohortMethod::createMatchOnPsArgs(),
    timepoint                  = NULL
) {

  # First: get default values:
  analysis <- list()
  for (name in names(formals(createRunPlpAnalysesArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  attr(analysis, "fun") <- "createRunPlpAnalysesArgs"
  class(analysis) <- "runPlpAnalysesArgs"
  return(analysis)
}


#' @title                   Create run settings for prediction
#' @param analyses          A list with settings for the prediction of each of
#'                          the risk stratification outcomes.
#'
#' @param defaultSettings  An object of type \code{runPlpAnalysesArgs} used as
#'                         the default settings for outcome risk prediction. Should
#'                         be created with
#'                         \code{\link[RiskStratifiedEstimation]{createRunPlpAnalysesArgs}}
#' @return                 An object of type \code{runPlpSettingsArgs} to be passed
#'                         to \code{\link[RiskStratifiedEstimation]{createRunSettings}}
#'
#' @export
createRunPlpSettingsArgs <- function(
    analyses = list(),
    defaultSettings = createRunPlpAnalysesArgs()
) {

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



#' @title Create prediction settings for an existing model
#' @param outcomeId              The outcome ID of the existing model.
#'
#' @param plpResultDirectory     The directory where the existing model is store.d
#' @param predictionSettings     A list with prediction settings.
#'
#' @export
createRunExistingPlpSettingsArgs <- function(
    outcomeId,
    plpResultDirectory,
    predictionSettings = list()
) {

  analysis <- list()
  for (name in names(formals(createRunExistingPlpSettingsArgs))) {
    analysis[[name]] <- get(name)
  }
  # Second: overwrite defaults with actual values:
  values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
  for (name in names(values)) {
    if (name %in% names(analysis))
      analysis[[name]] <- values[[name]]
  }
  attr(analysis, "fun") <- "createRunExistingPlpSettingsArgs"
  class(analysis) <- "runExistingPlpSettingsArgs"
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

createRunSettings <- function(
    runPlpSettings = createRunPlpSettingsArgs(),
    runCmSettings = createRunCmSettingsArgs()
) {

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

createCreatePsArgs <- function(
    excludeCovariateIds = c(),
    includeCovariateIds = c(),
    maxCohortSizeForFitting = 250000,
    errorOnHighCorrelation = TRUE,
    stopOnError = TRUE,
    prior = createPrior(
      "laplace",
      exclude = c(0),
      useCrossValidation = TRUE
    ),
    control = createControl(
      noiseLevel = "silent",
      cvType = "auto",
      tolerance = 2e-07,
      cvRepetitions = 10,
      startingVariance = 0.01
    )
) {
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




#' Create a parameter object for defining the estimation analyses.
#'
#' @param label                         A single-word description of the analysis.
#' @param riskStratificationMethod      Can be "equal" for equal-sized risk strata;
#'                                      "quantile" for user-specified risk quantiles
#'                                      as thresholds; "custom" for user-specified risk
#'                                      thresholds
#' @param stratificationOutcomes        The outcome IDs on which risk stratification
#'                                      will be carried out. Should be a subset
#'                                      of the stratification outcomes defined
#'                                      in the `analysisSettings`. Default is "all".
#' @param riskStratificationThresholds  The thresholds to be used for risk stratification.
#'                                      If `riskStratificationMethod` is "equal" then
#'                                      it should be a single number denoting the number
#'                                      of equal-sized risk subgroups; if `riskStratificationMethod`
#'                                      is "quantile", then it is a vector from 0 to 1 (increasing);
#'                                      if `riskStratificationMethod` is "custom", then it is a vector
#'                                      with the predicted probabilities to be used as thresholds
#' @param psMethod                      How should the propensity scores be used? Can
#'                                      be one of "stratifyByPs" or "matchOnPs".
#' @param effectEstimationSettings      Parameter object providing further settings
#'                                      for the implementation of selected \code{psMethod}
#'                                      to the estimation process. Can be created using
#'                                      one of
#'                                      \code{\link[RiskStratifiedEstimation]{createStratifyByPsArgs}}
#'                                      when \code{stratifyByPs} is selected or
#'                                      \code{\link[RiskStratifiedEstimation]{createMatchOnPsArgs}}
#'                                      when \code{matchOnPs} is selected.
#' @param timePoint                     The time point after cohort start that absolute differences
#'                                      should be estimated.
#'
#' @return
#' A parameter object for running the the estimation step.
#'
#' @export

createRunCmAnalysesArgs <- function(
    label                        = NULL,
    riskStratificationMethod     = "equal",
    stratificationOutcomes       = "all",
    riskStratificationThresholds = 4,
    psMethod                     = "stratifyByPs",
    effectEstimationSettings     = createStratifyByPsArgs(),
    timePoint                    = 365
) {

  if (is.null(label)) {
    label <- psMethod
  }

  res <- list(
    label                        = label,
    riskStratificationMethod     = riskStratificationMethod,
    riskStratificationThresholds = riskStratificationThresholds,
    stratificationOutcomes       = stratificationOutcomes,
    psMethod                     = psMethod,
    effectEstimationSettings     = effectEstimationSettings,
    timePoint                    = timePoint
  )

  class(res) <- "args"

  return(res)

}

#' Create a parameter object for defining the estimation analyses.
#'
#' @param label                         Label of the analysis (no spaces allowed).
#' @param description                   Description of the analysis.
#' @param stratificationOutcome         The outcome ID on which risk stratification
#'                                      will be carried out. Should be one of the
#'                                      stratification outcomes defined in the
#'                                      `analysisSettings`.
#' @param estimationOutcomes            Outcomes to estimate risk-stratified treatment
#'                                      effects.
#' @param riskStratificationMethod      Risk stratification method
#' @param psAdjustmentMethod            Propensity score adjustment method
#' @param timePoint                     The time point after cohort start that absolute differences
#'                                      should be estimated.
#'
#' @return
#' A parameter object for running the the estimation step.
#'
#' @export

createRunCmAnalysis <- function(
  label,
  description = "",
  stratificationOutcome,
  estimationOutcomes,
  riskStratificationMethod,
  psAdjustmentMethod,
  timePoint
) {
  res <- list(
    label = label,
    description = description,
    stratificationOutcome = stratificationOutcome,
    estimationOutcomes = estimationOutcomes,
    riskStratificationMethod = riskStratificationMethod,
    psAdjustmentMethod = psAdjustmentMethod,
    timePoint = timePoint
  )

  return(res)
}

#' Create a parameter object for running the estimation step
#'
#' @description
#' Create a parameter object for running the estimation step. This function is
#' used to create the computational part of the input of
#' \code{\link[RiskStratifiedEstimation]{createRunSettings}}.
#'
#' @param analyses                  A list of the analyses to run. Each element of
#'                                  the list can be creaeted using
#'                                  \code{\link[RiskStratifiedEstimation]{createRunCmAnalysesArgs}}
#'                                  \code{\link[CohortMethod]{createPs}}
#' @param psSettings                The settings for estimating the propensity scores
#' @param createPsThreads           The number of parallel threads for the
#'                                  estimation of the propensity scores. Default is 1.
#' @param fitOutcomeModelsThreads   The number of parallel threads for the estimation of the
#'                                  outcome models.
#' @param balanceThreads            The number of parallel threads for the estimation
#'                                  of covariate balance
#' @param negativeControlThreads    The number of parallel threads for the negative
#'                                  control analyses
#' @param runRiskStratifiedNcs      Should risk stratified negative control analyses
#'                                  be performed? Default is FALSE as it can take
#'                                  a very long time to complete.
#'
#'
#' @return
#' A parameter object for running the the estimation step.
#' @export

createRunCmSettingsArgs <- function(
    analyses,
    psSettings                   = createCreatePsArgs(),
    createPsThreads              = 1,
    fitOutcomeModelsThreads      = 1,
    balanceThreads               = 1,
    negativeControlThreads       = 1,
    runRiskStratifiedNcs         = FALSE
) {

  res <- list(
    analyses                = analyses,
    psSettings              = psSettings,
    createPsThreads         = createPsThreads,
    fitOutcomeModelsThreads = fitOutcomeModelsThreads,
    balanceThreads          = balanceThreads,
    negativeControlThreads  = negativeControlThreads,
    runRiskStratifiedNcs    = runRiskStratifiedNcs
  )
  class(res) <- "args"

  return(res)
}




#' Create a parameter object for the function stratifyByPs
#'
#' @description
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

createStratifyByPsArgs <- function(
    numberOfStrata = 5,
    stratificationColumns = c(),
    baseSelection = "all"
) {
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
  analysis$psMethod <- "stratifyByPs"
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

createMatchOnPsArgs <- function(
    caliper = 0.2,
    caliperScale = "standardized logit",
    maxRatio = 1,
    stratificationColumns = c()
) {
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
  analysis$psMethod <- "matchOnPs"
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
#' @param analysisId                The analysis ID.
#' @param description               Text describing the analysis.
#' @param databaseName              The name of the database.
#' @param treatmentCohortId         The cohort definition id of the treatment cohort
#'                                  in the cohortTable.
#' @param comparatorCohortId        The cohort definition id of the comparator cohort
#'                                  in the cohortTable.
#' @param outcomeIds                The cohort definition ids of the outcome cohorts
#'                                  in the outcomeTable.
#' @param analysisMatrix            Boolean matrix defining the outcomes to be assessed
#'                                  (rows) within risk strata (columns). The order
#'                                  in columns should match the the order of
#'                                  \code{outcomeIds}. Default is the diagonal matrix,
#'                                  which leads to the risk stratified assessment
#'                                  of only the outcome for which the risk strata
#'                                  were defined.
#' @param mapTreatments             Dataframe containing 2 columns: "exposure_id"
#'                                  with the id numbers of the
#'                                  treatment and comparator cohorts and "exposure_name"
#'                                  the cohort names.
#' @param mapOutcomes               Dataframe containing 2 columns: "outcome_id" with
#'                                  the cohort names of the outcomes of interest and
#'                                  "outcome_name" with their names.
#' @param negativeControlOutcomes   The outcome Ids to be used as negative controls
#' @param balanceThreads            The number of threads to be used for the estimation
#'                                  of covariate balance
#' @param negativeControlThreads    The number of threads to be used for running
#'                                  the negative control analyses
#' @param verbosity                 Sets the level of the verbosity. If the log level
#'                                  is at or higher in priority than the logger threshold,
#'                                  a message will print. The levels are:
#'                                  \itemize{
#'                                    \item {DEBUG}{Highest verbosity showing all debug statements}
#'                                    \item {TRACE}{Showing information about start and end of steps}
#'                                    \item {INFO}{Show informative information (Default)}
#'                                    \item {WARN}{Show warning messages}
#'                                    \item {ERROR}{Show error messages}
#'                                    \item {FATAL}{Be silent except for fatal errors}
#'                                  }
#' @param saveDirectory             The directory name where the results of the analyses will
#'                                  be stored.
#' @return
#' An analysisSettings object providing the identification information of the analysis.
#'
#' @export

createAnalysisSettings <- function(
    analysisId = NULL,
    description = "",
    databaseName,
    treatmentCohortId,
    comparatorCohortId,
    outcomeIds,
    analysisMatrix = diag(length(outcomeIds)),
    mapTreatments,
    mapOutcomes,
    negativeControlOutcomes = c(),
    balanceThreads = 1,
    negativeControlThreads = 1,
    verbosity = NULL,
    saveDirectory = NULL
) {
  res <- list(
    analysisId              = analysisId,
    description             = description,
    databaseName            = databaseName,
    treatmentCohortId       = treatmentCohortId,
    comparatorCohortId      = comparatorCohortId,
    outcomeIds              = outcomeIds,
    analysisMatrix          = analysisMatrix,
    mapTreatments           = mapTreatments,
    mapOutcomes             = mapOutcomes,
    negativeControlOutcomes = negativeControlOutcomes,
    balanceThreads          = balanceThreads,
    negativeControlThreads  = negativeControlThreads,
    verbosity               = verbosity,
    saveDirectory           = saveDirectory
  )

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
#'                                \code{\link[PatientLevelPrediction]{createStudyPopulationSettings}}.
#' @param populationCmSettings    Parameter object for the definition of the \code{populationCm} object
#'                                created from
#'                                \code{\link[CohortMethod]{createCreateStudyPopulationArgs}}.
#' @param postProcessing          A function to run on the initial population after
#'                                creating the prediction and estimation populations
#'
#' @export

createPopulationSettings <- function(
    populationPlpSettings = PatientLevelPrediction::createStudyPopulationSettings(),
    populationCmSettings  = CohortMethod::createCreateStudyPopulationArgs(),
    postProcessing        = "none"
) {

  res <- list(
    populationPlpSettings = populationPlpSettings,
    populationCmSettings  = populationCmSettings,
    postProcessing        = postProcessing
  )


  attr(res, "fun") <- "createPopulationSettings"
  class(res)       <- "populationSettings"

  return(res)

}




#' Create parameter object for database to be reached
#'
#' @param cdmVersion               Define the OMOP CDM version
#' @param cdmDatabaseSchema        The name of the database schema that contains the OMOP CDM instance.
#'                                 Requires read permissions to this database. On SQL Server, this
#'                                 should specify both the database and the schema, so for example
#'                                 "cdm_instance.dbo"
#' @param databaseName             The name of the database.
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
#' @param tempEmulationSchema      Some database platforms like Oracle and Impala do not truly support
#'                                 temp tables. To emulate temp tables, provide a schema with write
#'                                 privileges where temp tables can be created.
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
                                   databaseName,
                                   cohortDatabaseSchema = cdmDatabaseSchema,
                                   outcomeDatabaseSchema = cdmDatabaseSchema,
                                   resultsDatabaseSchema = cdmDatabaseSchema,
                                   exposureDatabaseSchema = cdmDatabaseSchema,
                                   tempEmulationSchema = cdmDatabaseSchema,
                                   cohortTable,
                                   outcomeTable,
                                   exposureTable = "drug_era",
                                   mergedCohortTable,
                                   targetCohortId = 1) {

  res <- list(cdmVersion = cdmVersion,
              cdmDatabaseSchema = cdmDatabaseSchema,
              databaseName = databaseName,
              cohortDatabaseSchema = cohortDatabaseSchema,
              outcomeDatabaseSchema = outcomeDatabaseSchema,
              resultsDatabaseSchema = resultsDatabaseSchema,
              exposureDatabaseSchema = exposureDatabaseSchema,
              cohortTable = cohortTable,
              outcomeTable = outcomeTable,
              exposureTable = exposureTable,
              tempEmulationSchema = tempEmulationSchema,
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

#' @export
createRiskStratificationMethod <- function(
    label,
    descritption = "",
    riskStratificationThresholds
) {
  return(
    list(
      label = label,
      description = descritption,
      riskStratificationThresholds = riskStratificationThresholds
    )
  )
}

#' @export
createPsAdjsutmentMethod <- function(
    label,
    description = "",
    psAdjustmentSettings
) {
  return(
    list(
      label = label,
      description = description,
      psAdjustmentSettings = psAdjustmentSettings
    )
  )
}
