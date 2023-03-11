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

test_that("createMapMatrix works", {
  ps <- data.frame(
    rowId = 1:4,
    value = c(.1, .2, .3, .4)
  )

  analysisEqual <- list(
    riskStratificationMethod = "equal",
    riskStratificationThresholds = 4
  )

  analysisQuantile <- list(
    riskStratificationMethod = "quantile",
    riskStratificationThresholds = c(0, .5, 1)
  )

  analysisCustom <- list(
    riskStratificationMethod = "custom",
    riskStratificationThresholds = function(prediction) {
      prediction %>%
        dplyr::mutate(
          riskStratum = dplyr::case_when(
            value < .2 ~ 1,
            value < .4 ~ 2,
            TRUE       ~ 3
          )
        )
    }
  )

  expect_equal(
    createMapMatrix(ps, analysisEqual) %>% dplyr::pull(riskStratum),
    1:4
  )

  expect_equal(
    createMapMatrix(ps, analysisQuantile) %>% dplyr::pull(riskStratum),
    c(1, 1, 2, 2)
  )

  expect_equal(
    createMapMatrix(ps, analysisCustom) %>% dplyr::pull(riskStratum),
    c(1, 2, 2, 3)
  )
})
