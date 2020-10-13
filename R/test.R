#' @export
includeOverallResults <- function(
  analysisSettings,
  getDataSettings,
  runSettings
)
{
  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  outcomeIds <- analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]
  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = file.path(
      getDataSettings$cohortMethodDataFolder
    )
  )

  covariates <- cohortMethodData$covariates %>%
    dplyr::collect()
  data.table::setDT(covariates)

  covariateRef <- cohortMethodData$covariateRef %>%
    dplyr::collect()
  data.table::setDT(covariateRef)

  cohorts <- cohortMethodData$cohorts %>%
    dplyr::collect()
  data.table::setDT(cohorts)

  overallTreatmentEffects <- incidence <- NULL

  for (outcomeId in outcomeIds)
  {
    ps <- readRDS(
      file = file.path(
        analysisPath,
        "Estimation",
        outcomeId,
        "psFull.rds"
      )
    )

    if (runSettings$runCmSettings$psMethod == "matchOnPs")
    {
      ps <- CohortMethod::matchOnPs(
        population = ps,
        caliper = runSettings$runCmSettings$effectEstimationSettings$caliper,
        caliperScale = runSettings$runCmSettings$effectEstimationSettings$caliperScale,
        maxRatio = runSettings$runCmSettings$effectEstimationSettings$maxRatio
      )
    }
    else if (runSettings$runCmSettings$psMethod == "stratifyByPs")
    {
      ps <- CohortMethod::stratifyByPs(
        population = ps,
        numberOfStrata = runSettings$runCmSettings$effectEstimationSettings$numberOfStrata,
        baseSelection = runSettings$runCmSettings$effectEstimationSettings$baseSelection
      )
    }

    model <- CohortMethod::fitOutcomeModel(
      population = ps,
      cohortMethodData = cohortMethodData,
      modelType = "cox",
      stratified = FALSE
    )

    overallTreatmentEffects <- data.frame(
      estimate = exp(model$outcomeModelTreatmentEstimate$logRr),
      lower = exp(model$outcomeModelTreatmentEstimate$logLb95),
      upper = exp(model$outcomeModelTreatmentEstimate$logUb95),
      outcomeId = outcomeId,
      database = analysisSettings$databaseName,
      analysisType = runSettings$runCmSettings$psMethod,
      treatmentId = analysisSettings$treatmentCohortId,
      comparatorId = analysisSettings$comparatorCohortId
    ) %>%
      dplyr::bind_rows(overallTreatmentEffects)

    incidence <- computeIncidence(ps) %>%
      dplyr::mutate(
        database = analysisSettings$databaseName,
        analysisId = analysisSettings$analysisId,
        treatmentId = analysisSettings$treatmentCohortId,
        comparatorId = analysisSettings$comparatorCohortId,
        outcomeId = outcomeId,
        analysisType = analysisSettings$analysisType
      ) %>%
      dplyr::bind_rows(incidence)

    psDensity(ps) %>%
      dplyr::mutate(
        database = analysisSettings$databaseName,
        analysisId = analysisSettings$analysisId,
        outcomeId = outcomeId,
        treatment = ifelse(
          treatment == 1,
          yes = analysisSettings$treatmentCohortId,
          no = analysisSettings$comparatorCohortId
        ),
        treatmentId = analysisSettings$treatmentCohortId,
        comparatorId = analysisSettings$comparatorCohortId,
        analysisType = analysisSettings$analysisType
      ) %>%
      saveRDS(
        file = file.path(
          analysisPath,
          "shiny",
          paste(
            paste(
              "overall",
              "psDensity",
              analysisSettings$analysisId,
              analysisSettings$treatmentCohortId,
              analysisSettings$comparatorCohortId,
              outcomeId,
              sep = "_"
            ),
            "rds",
            sep = "."
          )
        )
      )

    computeCovariateBalance4(
      population = ps,
      cohorts = cohorts,
      covariates = covariates,
      covariateRef = covariateRef
    ) %>%
      dplyr::select(
        covariateId,
        covariateName,
        beforeMatchingStdDiff,
        afterMatchingStdDiff
      ) %>%
      dplyr::mutate(
        analysisId = analysisSettings$analysisId,
        treatmentId = analysisSettings$treatmentCohortId,
        comparatorId = analysisSettings$comparatorCohortId,
        outcomeId = outcomeId
      ) %>%
      saveRDS(
        file = file.path(
          analysisPath,
          "shiny",
          paste(
            paste(
              "overall",
              "balance",
              analysisSettings$analysisId,
              analysisSettings$treatmentCohortId,
              analysisSettings$comparatorCohortId,
              outcomeId,
              sep = "_"
            ),
            "rds",
            sep = "."
          )
        )
      )
  }
  rownames(overallTreatmentEffects) <- rownames(incidence) <- NULL
  saveRDS(
    object = incidence,
    file = file.path(
      analysisPath,
      "shiny",
      "incidenceOverall.rds"
    )
  )

  saveRDS(
    object = overallTreatmentEffects,
    file = file.path(
      analysisPath,
      "shiny",
      "mappedOverallResults.rds"
    )
  )
  return(NULL)
}
