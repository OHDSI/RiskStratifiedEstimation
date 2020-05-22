#' @importFrom dplyr %>%
#' @export

evaluatePrediction <- function(analysisSettings,
                               getDataSettings,
                               populationSettings,
                               predictionId){

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  plpData <- PatientLevelPrediction::loadPlpData(
    getDataSettings$plpDataFolder
  )

  cohortMethodData <- CohortMethod::loadCohortMethodData(
    getDataSettings$cohortMethodDataFolder
  )



  plpResult <- PatientLevelPrediction::loadPlpResult(
    file.path(
      saveDir,
      "Prediction",
      predictionId,
      analysisSettings$analysisId,
      "plpResult"
    )
  )

  psFull <- readRDS(
    file.path(
      saveDir,
      "Estimation",
      predictionId,
      "psFull.rds"
    )
  )

  startingPopulation <- plpData$cohorts %>%
    dplyr::filter(
      subjectId %in% psFull$subjectId
    )

  psFull <- PatientLevelPrediction::createStudyPopulation(
    plpData = plpData,
    population = startingPopulation,
    outcomeId = predictionId,
    binary = populationSettings$populationPlpSettings$binary,
    includeAllOutcomes = populationSettings$populationPlpSettings$includeAllOutcomes,
    firstExposureOnly = populationSettings$populationPlpSettings$firstExposureOnly,
    washoutPeriod = populationSettings$populationPlpSettings$washoutPeriod,
    removeSubjectsWithPriorOutcome = populationSettings$populationPlpSettings$removeSubjectsWithPriorOutcome,
    priorOutcomeLookback = populationSettings$populationPlpSettings$priorOutcomeLookback,
    requireTimeAtRisk = populationSettings$populationPlpSettings$requireTimeAtRisk,
    minTimeAtRisk = populationSettings$populationPlpSettings$minTimeAtRisk,
    riskWindowStart = populationSettings$populationPlpSettings$riskWindowStart,
    startAnchor = populationSettings$populationPlpSettings$startAnchor,
    riskWindowEnd = populationSettings$populationPlpSettings$riskWindowEnd,
    endAnchor = populationSettings$populationPlpSettings$endAnchor,
    verbosity = populationSettings$populationPlpSettings$verbosity
  ) %>%
    dplyr::left_join(
      psFull
    ) %>%
    dplyr::mutate(
      outcomeCount = ifelse(
        outcomeCount > 0,
        yes = 1,
        no = 0
      )
    )

  # Evaluate on matched set
  populationSubset <- CohortMethod::matchOnPs(
    psFull
  )
  attr(populationSubset, "metaData") <- list(
    cohortId = 1,
    outcomeId = predictionId
  )

  modelEvaluationOnSubset <- PatientLevelPrediction::applyModel(
    population = populationSubset,
    plpData = plpData,
    plpModel = plpResult$model
  )

  # Save evaluation on matched set
  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "Matched"
  )

  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "Matched"
  )

  if (!dir.exists(analysisPath)) {

    dir.create(
      analysisPath,
      recursive = TRUE
    )

  }

  saveRDS(
    modelEvaluationOnSubset$performanceEvaluation,
    file = file.path(
      analysisPath,
      "performanceEvaluation.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$prediction,
    file = file.path(
      analysisPath,
      "prediction.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$covariateSummary,
    file = file.path(
      analysisPath,
      "covariateSummary.rds"
    )
  )

  # Load entire target population
  population <- plpData$cohorts %>%
    dplyr::right_join(
      psFull
    )

  attr(population, "metaData") <- list(
    cohortId = 1,
    outcomeId = predictionId
  )

  modelEvaluationOnPopulation <- PatientLevelPrediction::applyModel(
    population = population,
    plpData = plpData,
    plpModel = plpResult$model
  )

  # Save evaluation on the entire target population
  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "EntirePopulation"
  )

  if (!dir.exists(analysisPath)) {

    dir.create(
      analysisPath,
      recursive = TRUE
    )

  }

  saveRDS(
    modelEvaluationOnPopulation$performanceEvaluation,
    file = file.path(
      analysisPath,
      "performanceEvaluation.rds"
    )
  )

  saveRDS(
    modelEvaluationOnPopulation$prediction,
    file = file.path(
      analysisPath,
      "prediction.rds"
    )
  )

  saveRDS(
    modelEvaluationOnPopulation$covariateSummary,
    file = file.path(
      analysisPath,
      "covariateSummary.rds"
    )
  )


  # Evaluate on treatment arm
  populationSubset <- population %>%
    dplyr::filter(
      treatment == 1
    )

  modelEvaluationOnSubset <- PatientLevelPrediction::applyModel(
    population = populationSubset,
    plpData = plpData,
    plpModel = plpResult$model
  )

  # Save evaluation on treatment arm
  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "Treatment"
  )

  if (!dir.exists(analysisPath)) {

    dir.create(
      analysisPath,
      recursive = TRUE
    )

  }

  saveRDS(
    modelEvaluationOnSubset$performanceEvaluation,
    file = file.path(
      analysisPath,
      "performanceEvaluation.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$prediction,
    file = file.path(
      analysisPath,
      "prediction.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$covariateSummary,
    file = file.path(
      analysisPath,
      "covariateSummary.rds"
    )
  )

  # Evaluate on comparator arm
  populationSubset <- population %>%
    dplyr::filter(
      treatment == 0
    )

  modelEvaluationOnSubset <- PatientLevelPrediction::applyModel(
    population = populationSubset,
    plpData = plpData,
    plpModel = plpResult$model
  )

  # Save evaluation on comparator arm
  analysisPath <- file.path(
    saveDir,
    "Prediction",
    predictionId,
    analysisSettings$analysisId,
    "Comparator"
  )

  if (!dir.exists(analysisPath)) {

    dir.create(
      analysisPath,
      recursive = TRUE
    )

  }

  saveRDS(
    modelEvaluationOnSubset$performanceEvaluation,
    file = file.path(
      analysisPath,
      "performanceEvaluation.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$prediction,
    file = file.path(
      analysisPath,
      "prediction.rds"
    )
  )

  saveRDS(
    modelEvaluationOnSubset$covariateSummary,
    file = file.path(
      analysisPath,
      "covariateSummary.rds"
    )
  )

  return(NULL)

}
