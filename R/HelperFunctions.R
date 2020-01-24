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
                              connectionDetails){

  addTable(connectionDetails,
           resultsDatabaseSchema = resultsDatabaseSchema,
           table = mergedCohortTable)

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
  DatabaseConnector::disconnect(connection)

}



addTable <- function(connectionDetails,
                     resultsDatabaseSchema,
                     table){

  renderedSql <- SqlRender::loadRenderTranslateSql("createTable.sql",
                                                   packageName = "RiskStratifiedEstimation",
                                                   result_database_schema = resultsDatabaseSchema,
                                                   target_cohort_table = table,
                                                   dbms = connectionDetails$dbms)

  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::executeSql(connection, renderedSql)
  DatabaseConnector::disconnect(connection)

}




#' @importFrom dplyr %>%
switchOutcome <- function(ps,
                           populationCm){


  result <- ps %>%
    dplyr::select(subjectId, propensityScore) %>%
    dplyr::left_join(populationCm,
                     by = "subjectId")%>%
    dplyr::filter(!is.na(survivalTime))
  return(result)

}
