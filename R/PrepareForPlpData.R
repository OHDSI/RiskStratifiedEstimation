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
                                                   cohort_ids = c(treatmentCohortId, comparatorCohortId))

  DatabaseConnector::executeSql(connection, renderedSql)


  renderedSql <- SqlRender::loadRenderTranslateSql("treatmentCovariateSettings.sql",
                                                   packageName = "RiskStratifiedEstimation",
                                                   result_database_schema = resultsDatabaseSchema,
                                                   cohort_database_schema = cohortDatabaseSchema,
                                                   cohort_table = cohortTable,
                                                   merged_cohort_table = mergedCohortTable,
                                                   treatment_cohort_id = treatmentCohortId,
                                                   attribute_definition_table = attributeDefinitionTable,
                                                   cohort_attribute_table = cohortAttributeTable)
  DatabaseConnector::executeSql(connection, renderedSql)

}
