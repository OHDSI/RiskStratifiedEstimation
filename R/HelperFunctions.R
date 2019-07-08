#' Create a parameter object for the function createStudyPopulation
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param population If specified, this population will be used as the startingpoint instead of the cohorts in the cohortMethodDataobject.
#' @param firstExposureOnly Should only the first exposure per subject be included? Notethat this is typically done in thecreateStudyPopulation function,
#' @param restrictToCommonPeriod Restrict the analysis to the period when both exposures are observed?
#' @param washoutPeriod The mininum required continuous observation time prior toindex date for a person to be included in the cohort.
#' @param removeDuplicateSubjects Remove subjects that are in both the target and comparatorcohort? See details for allowed values.
#' @param removeSubjectsWithPriorOutcome Remove subjects that have the outcome prior to the riskwindow start?
#' @param priorOutcomeLookback How many days should we look back when identifying prioroutcomes?
#' @param minDaysAtRisk The minimum required number of days at risk.
#' @param riskWindowStart The start of the risk window (in days) relative to the indexdate (+ days of exposure if theaddExposureDaysToStart parameter is specified).
#' @param addExposureDaysToStart Add the length of exposure the start of the risk window?
#' @param riskWindowEnd The end of the risk window (in days) relative to the indexdata (+ days of exposure if the addExposureDaysToEndparameter is specified).
#' @param addExposureDaysToEnd Add the length of exposure the risk window?
#' @param censorAtNewRiskWindow If a subject is in multiple cohorts, should time-at-risk be censoredwhen the new time-at-risk starts to prevent overlap?
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
#' @param minCovariateFraction The minimum fraction of target population who must have a covariate for it to be included in the model training
#' @param normalizeData Whether to normalise the covariates before training (Default: TRUE)
#' @param testSplit Either 'person' or 'time' specifying the type of evaluation used.'time' find the date where testFraction of patients had an index after the date and assigns patients with an index prior to this date into the training set and post the date into the test set'person' splits the data into test (1-testFraction of the data) andtrain (validationFraction of the data) sets.  The split is stratified by the class label.
#' @param testFraction The fraction of the data to be used as the test set in the patientsplit evaluation.
#' @param trainFraction A real number between 0 and 1 indicating the train set fraction of the data.If not set trainFraction is equal to 1 - test
#' @param splitSeed The seed used to split the test/train set when using a person type testSplit
#' @param nfold The number of folds used in the cross validation (default 3)
#' @param indexes A dataframe containing a rowId and index column where the index value of -1 means in the test set, and positive integer represents the cross validation fold (default is NULL)
#' @param savePlpData Binary indicating whether to save the plpData object (default is T)
#' @param savePlpResult Binary indicating whether to save the object returned by runPlp (default is T)
#' @param savePlpPlots Binary indicating whether to save the performance plots as pdf files (default is T)
#' @param saveEvaluation Binary indicating whether to save the oerformance as csv files (default is T)
#' @param verbosity Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:DEBUGHighest verbosity showing all debug statementsTRACEShowing information about start and end of stepsINFOShow informative information (Default)WARNShow warning messagesERRORShow error messagesFATALBe silent except for fatal errors
#' @param timeStamp If TRUE a timestamp will be added to each logging statement. Automatically switched on for TRACE level.
#' @param analysisId Identifier for the analysis. It is used to create, e.g., the result folder. Default is a timestamp.
#'
#' @export
createRunPlpArgs <- function(minCovariateFraction = 0.001,
                             normalizeData = TRUE,
                             testSplit = "person",
                             testFraction = 0.25,
                             trainFraction = NULL,
                             splitSeed = NULL,
                             nfold = 3,
                             indexes = NULL,
                             savePlpData = TRUE,
                             savePlpResult = TRUE,
                             savePlpPlots = TRUE,
                             saveEvaluation = TRUE,
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




#' Create a parameter object for the function getPlpData
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param studyStartDate A calendar date specifying the minimum date that a cohort indexdate can appear. Date format is 'yyyymmdd'.
#' @param studyEndDate A calendar date specifying the maximum date that a cohort indexdate can appear. Date format is 'yyyymmdd'. Important: the studyend data is also used to truncate risk windows, meaning no outcomesbeyond the study end date will be considered.
#' @param firstExposureOnly Should only the first exposure per subject be included? Note thatthis is typically done in the createStudyPopulation function,but can already be done here for efficiency reasons.
#' @param washoutPeriod The mininum required continuous observation time prior to indexdate for a person to be included in the at risk cohort. Note thatthis is typically done in the createStudyPopulation function,but can already be done here for efficiency reasons.
#' @param excludeDrugsFromCovariates A redundant option
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




#' Prepares for the running the PatientLevelPrediction package
#'
#' Prepares for running the PatientLevelPrediction package by merging the treatment and comparator cohorts and defining a new covariate for treatment.
#'
#' @param treatmentCohortId The treatment cohort id
#' @param comparatorCohortId The comparator cohort id
#' @param targetCohortId The id of the merged cohorts
#' @param cohortDatabaseSchema The name of the database schema that is the location where the cohort data used to define the at risk cohort is available
#' @param cohortTable The table that contains the treatment and comparator cohorts.
#' @param resultsDatabaseSchema The name of the database schema to store the new tables. Need to have write access.
#' @param mergedCohortTable The table that will contain the merged cohorts.
#' @param attributeDefinitionTable The table that will contain the definition of the treatment variable.
#' @param cohortAttributeTable The table that will contain the patients along with their new covariate values.
#' @param connectionDetails The connection details required to connect to a database.
#'
#' @return Creates the tables resultsDatabaseSchema.mergedCohortTable, resultsDatabaseSchema.attributeDefinitionTable and resultsDatabaseSchema.cohortAttributeTable
#' @return A covariate settings object for the treatment covariate.
#'
#' @export

prepareForPlpData <- function(treatmentCohortId,
                              comparatorCohortId,
                              targetCohortId,
                              cohortDatabaseSchema,
                              cohortTable,
                              resultsDatabaseSchema,
                              mergedCohortTable,
                              attributeDefinitionTable,
                              cohortAttributeTable,
                              connectionDetails){

  connection <- DatabaseConnector::connect(connectionDetails)

  renderedSql <- SqlRender::loadRenderTranslateSql("mergeCohorts.sql",
                                                   packageName = "RiskStratifiedEstimation",
                                                   result_database_schema = resultsDatabaseSchema,
                                                   merged_cohort_table = mergedCohortTable,
                                                   cohort_database_schema = cohortDatabaseSchema,
                                                   cohort_table = cohortTable,
                                                   target_cohort_id = targetCohortId,
                                                   cohort_ids = c(treatmentCohortId, comparatorCohortId),
                                                   dbms = connectionDetails$dbms)

  DatabaseConnector::executeSql(connection, renderedSql)


  renderedSql <- SqlRender::loadRenderTranslateSql("treatmentCovariateSettingsTest.sql",
                                                   packageName = "RiskStratifiedEstimation",
                                                   result_database_schema = resultsDatabaseSchema,
                                                   cohort_database_schema = cohortDatabaseSchema,
                                                   cohort_table = cohortTable,
                                                   merged_cohort_table = mergedCohortTable,
                                                   treatment_cohort_id = treatmentCohortId,
                                                   attribute_definition_table = attributeDefinitionTable,
                                                   cohort_attribute_table = cohortAttributeTable,
                                                   dbms = connectionDetails$dbms)
  DatabaseConnector::executeSql(connection, renderedSql)

}





# Removes treatment from the plpData object

removeTreatment <- function(plpData,
                            treatmentCovariateId){

  plpData$covariates <- ffbase::subset.ffdf(plpData$covariates,
                                            covariateId != treatmentCovariateId)
  plpData$covariateRef <- ffbase::subset.ffdf(plpData$covariateRef,
                                              covariateId != treatmentCovariateId)

  return(plpData)

}




#' @importFrom dplyr %>%
switchOutcome <- function(ffPsDataFrame,
                           populationCm){

  ffPsDataFrame <- ffPsDataFrame[]

  result <- ffPsDataFrame %>%
    dplyr::select(subjectId, propensityScore) %>%
    dplyr::left_join(populationCm,
                     by = "subjectId")%>%
    dplyr::filter(!is.na(survivalTime))
  ff::as.ffdf(result)

}
