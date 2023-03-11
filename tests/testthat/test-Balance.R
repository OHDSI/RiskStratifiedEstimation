test_that("computeMeansPerGroupFast works", {
  cohorts <- data.frame(
    rowId = 1:10,
    treatment = c(rep(0, 5), rep(1, 5)),
    stratumId = c(1:5, 1:5)
  )

  covariates <- data.frame(
    rowId = 1:10,
    covariateId = 1991,
    covariateValue = 1
  )
  setDT(cohorts)
  setDT(covariates)

  expect_equal(
    computeMeansPerGroupFast(cohorts, covariates),
    data.frame(
      covariateId = 1991,
      sumTarget = 5,
      meanTarget = 1,
      sumComparator = 5,
      meanComparator = 1,
      sd = 0
    )
  )
})

test_that("computeCovariateBalance works", {
  cohorts <- data.frame(
    rowId = 1:10,
    treatment = c(rep(0, 5), rep(1, 5)),
    stratumId = c(1:5, 1:5)
  )

  covariates <- data.frame(
    rowId = 1:10,
    covariateId = 1991,
    covariateValue = 1
  )

  covariateRef <- data.frame(
    covariateId = 1991,
    covariateName = "test covariate",
    analysisId = 91,
    concpetId = 19
  )
  setDT(cohorts); setDT(covariates); setDT(covariateRef)

  expect_equal(
    computeCovariateBalance(cohorts, cohorts, covariates, covariateRef),
    tibble(
      covariateId = 1991,
      beforeMatchingSumTarget = 5,
      beforeMatchingMeanTarget = 1,
      beforeMatchingSumComparator = 5,
      beforeMatchingMeanComparator = 1,
      beforeMatchingSd = 0,
      afterMatchingSumTarget = 5,
      afterMatchingMeanTarget = 1,
      afterMatchingSumComparator = 5,
      afterMatchingMeanComparator = 1,
      afterMatchingSd = 0,
      covariateName = "test covariate",
      analysisId = 91,
      concpetId = 19,
      beforeMatchingStdDiff = 0,
      afterMatchingStdDiff = 0
    )
  )
})
