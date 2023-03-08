library(testthat)
library(dplyr)
require(Eunomia)

test_that("prepareForPlpData works", {

  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  Eunomia::createCohorts(connectionDetails)


  RiskStratifiedEstimation:::prepareForPlpData(
    treatmentCohortId = 1,
    comparatorCohortId = 2,
    targetCohortId = 1,
    cohortDatabaseSchema = "main",
    cohortTable = "cohort",
    resultsDatabaseSchema = "main",
    mergedCohortTable = "merged",
    connectionDetails = connectionDetails
  )

  connection <- DatabaseConnector::connect(connectionDetails)
  tables <- DatabaseConnector::dbListTables(connection, "main")

  expect_true(
    "merged" %in% tables
  )

  cohortCounts <- DatabaseConnector::querySql(
    connection,
    "select cohort_definition_id, count(subject_id) from main.cohort group by cohort_definition_id"
  ) %>%
    dplyr::filter(COHORT_DEFINITION_ID %in% c(1, 2))

  mergedCounts <- DatabaseConnector::querySql(
    connection,
    "select cohort_definition_id, count(subject_id) from main.merged group by cohort_definition_id"
  )

  colnames(cohortCounts)[2] <- "n"
  colnames(mergedCounts)[2] <- "n"

  expect_equal(mergedCounts$n, sum(cohortCounts$n))
})

