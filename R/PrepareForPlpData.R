prepareForPlpData <- function(treatmentCohortId,
                              comparatorCohortId,
                              targetCohortId,
                              resultsDatabaseSchema,
                              resultTable,
                              cohortDatabaseSchema,
                              cohortTable,
                              connectionDetails){

  renderedSql <- SqlRender::loadRenderTranslateSql("mergeCohorts.sql",
                                                   packageName = "RiskStratifiedEstimation",
                                                   result_database_schema = resultsDatabaseSchema,
                                                   merged_cohorts = resultTable,
                                                   cohort_database_schema = cohortDatabaseSchema,
                                                   cohort_table = cohortTable,
                                                   target_cohort_id = targetCohortId,
                                                   cohort_ids = c(treatmentCohortId, comparatorCohortId))

  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::executeSql(connection, renderedSql)

}
