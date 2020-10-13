#' @importFrom dplyr %>%
#' @export

evaluatePrediction <- function(
  analysisSettings,
  getDataSettings,
  populationSettings,
  predictionId
)
{
  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  plpData <- PatientLevelPrediction::loadPlpData(
    getDataSettings$plpDataFolder
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

  startingPopulation <- psFull %>%
    dplyr::select(
      -"daysToEvent"
    )
  attr(startingPopulation, "metaData") <- attr(plpData$cohorts, "metaData")

  psFull <-
    psFull %>%
    dplyr::right_join(
      PatientLevelPrediction::createStudyPopulation(
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
      )
    ) %>%
    dplyr::mutate(
      outcomeCount = ifelse(
        outcomeCount > 0,
        yes = 1,
        no = 0
      )
    )

  # attr(psFull, "metaData") <- attr(psFull, "metaData")

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

  # calibrationData <- PatientLevelPrediction::calibrationLine(
  #   prediction = modelEvaluationOnSubset$prediction
  # )

  calibrationData <- modelEvaluationOnSubset$prediction %>%
    dplyr::mutate(
      group = dplyr::ntile(
        value,
        10
      )
    ) %>%
    dplyr::group_by(
      group
    ) %>%
    dplyr::summarise(
      personsWithOutcome = sum(
        outcomeCount
      ),
      PersonCountAtRisk = dplyr::n(),
      observedIncidence = mean(
        outcomeCount
      ),
     averagePredictedProbability = mean(
       value
     )
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
    calibrationData,
    file = file.path(
      analysisPath,
      "calibrationData.rds"
    )
  )

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

  calibrationData <- modelEvaluationOnPopulation$prediction %>%
    dplyr::mutate(
      group = dplyr::ntile(
        value,
        10
      )
    ) %>%
    dplyr::group_by(
      group
    ) %>%
    dplyr::summarise(
      personsWithOutcome = sum(
        outcomeCount
      ),
      PersonCountAtRisk = dplyr::n(),
      observedIncidence = mean(
        outcomeCount
      ),
     averagePredictedProbability = mean(
       value
     )
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
    calibrationData,
    file = file.path(
      analysisPath,
      "calibrationData.rds"
    )
  )
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

  calibrationData <- modelEvaluationOnSubset$prediction %>%
    dplyr::mutate(
      group = dplyr::ntile(
        value,
        10
      )
    ) %>%
    dplyr::group_by(
      group
    ) %>%
    dplyr::summarise(
      personsWithOutcome = sum(
        outcomeCount
      ),
      PersonCountAtRisk = dplyr::n(),
      observedIncidence = mean(
        outcomeCount
      ),
      averagePredictedProbability = mean(
        value
      )
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
    calibrationData,
    file = file.path(
      analysisPath,
      "calibrationData.rds"
    )
  )

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

  calibrationData <- modelEvaluationOnSubset$prediction %>%
    dplyr::mutate(
      group = dplyr::ntile(
        value,
        10
      )
    ) %>%
    dplyr::group_by(
      group
    ) %>%
    dplyr::summarise(
      personsWithOutcome = sum(
        outcomeCount
      ),
      PersonCountAtRisk = dplyr::n(),
      observedIncidence = mean(
        outcomeCount
      ),
      averagePredictedProbability = mean(
        value
      )
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
    calibrationData,
    file = file.path(
      analysisPath,
      "calibrationData.rds"
    )
  )

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

#' @importFrom dplyr %>%
#' @export
computeCovariateBalanceWeighted <- function(population,
                                            cohortMethodData){

  pops <- population %>%
    dplyr::group_by(
      treatment
    ) %>%
    dplyr::summarise(
      n = n(),
      nWeighted = sum(weights)
    )

  covariates <- cohortMethodData$covariates[]
  tt <- covariates %>%
    dplyr::filter(
      rowId %in% population$rowId
    ) %>%
    dplyr::left_join(
      population %>%
        dplyr::select(
          c(
            "rowId",
            "treatment",
            "weights"
          )
        )
    )

  test = tt %>%
    dplyr::group_by(
      treatment,
      covariateId
    ) %>%
    dplyr::summarise(
      covariateSum = sum(covariateValue)
    ) %>%
    dplyr::left_join(
      pops
    ) %>%
    dplyr::mutate(
      unweighted = covariateSum/n,
      weighted = covariateSum/nWeighted,
      sdWeighted = weighted*(1 - weighted)/2,
      sdUnweighted = unweighted*(1 - unweighted)/2
    ) %>%
    as.data.frame()

  t1 <- test %>%
    dplyr::mutate(
      treatment = ifelse(
        treatment == 1,
        yes = 1,
        no = -1
      )
    ) %>%
    dplyr::group_by(
      covariateId
    ) %>%
    dplyr::summarise(
      beforeWeighting = 100*abs(sum(unweighted*treatment))/sqrt(sum(sdUnweighted))
    ) %>%
    as.data.frame()
  t2 <- test %>%
    dplyr::mutate(
      treatment = ifelse(
        treatment == 1,
        yes = 1,
        no = -1
      )
    ) %>%
    dplyr::group_by(
      covariateId
    ) %>%
    dplyr::summarise(
      afterWeighting = 100*abs(sum(weighted*treatment))/sqrt(sum(sdWeighted))
    ) %>%
    as.data.frame()

  t1 %>%
    dplyr::left_join(
      t2
    ) %>%
    return()
}


#' @importFrom dplyr %>%
#' @export
psDensityOverall <- function(ps){

  lapply(ps,
         psDensity) %>%
    dplyr::bind_rows(
      .id = "riskStratum"
    ) %>%
    dplyr::mutate(
      riskStratum = paste0(
        "Q",
        riskStratum
      ) %>%
        return()
    )

}

#' @importFrom dplyr %>%
#' @export
psDensityCombined <- function(outcomeId,
                              analysisSettings,
                              secondaryOutcomes = FALSE){

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  if (!dir.exists(saveDir)) {
    dir.create(
      saveDir,
      recursive = TRUE
    )
  }

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  dir <- file.path(
    analysisPath,
    "Estimation",
    outcomeId
  )

  ps <- tryCatch(
    {
      readRDS(
        file.path(
          dir,
          "ps.rds"
        )
      )
    },
    error = function(e)
    {
      e$message
    }
  )

  if (is.character(ps))
  {
    return(NULL)
  }

  psDensityOverall(ps) %>%
    dplyr::mutate(
      database = analysisSettings$databaseName,
      analysisId = analysisSettings$analysisId,
      stratOutcome = outcomeId,
      estOutcome = outcomeId,
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
      file.path(
        saveDir,
        paste0(
          paste(
            "psDensity",
            analysisSettings$analysisId,
            analysisSettings$treatmentCohortId,
            analysisSettings$comparatorCohortId,
            outcomeId,
            outcomeId,
            sep = "_"
          ),
          ".rds"
        )
      )
    )

  if (secondaryOutcomes) {
    predLoc <- which(analysisSettings$outcomeIds == outcomeId)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- compareOutcomes[compareOutcomes != outcomeId]
    compareOutcomes <- sort(
      compareOutcomes[compareOutcomes != outcomeId]
    )

    for (compareOutcome in compareOutcomes) {
      dir <- file.path(
        analysisPath,
        "Estimation",
        outcomeId,
        compareOutcome
      )

      ps <- tryCatch(
        {
          readRDS(
            file.path(
              dir,
              "ps.rds"
            )
          )
        },
        error = function(e)
        {
          e$message
        }
      )

      if (is.character(ps))
      {
        return(NULL)
      }

      psDensityOverall(ps) %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          stratOutcome = outcomeId,
          estOutcome = compareOutcome,
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
          file.path(
            saveDir,
            paste0(
              paste(
                "psDensity",
                analysisSettings$analysisId,
                analysisSettings$treatmentCohortId,
                analysisSettings$comparatorCohortId,
                outcomeId,
                compareOutcome,
                sep = "_"
              ),
              ".rds"
            )
          )
        )
    }
  }
  return(NULL)
}

#' @export
psDensityAnalysis <- function(analysisSettings,
                              secondaryOutcomes = FALSE,
                              threads = 1){
  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  cluster <- ParallelLogger::makeCluster(threads)
  ParallelLogger::clusterRequire(
    cluster,
    "RiskStratifiedEstimation"
  )


  dummy <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = predictOutcomes,
    fun = psDensityCombined,
    analysisSettings = analysisSettings,
    secondaryOutcomes = secondaryOutcomes
  )

  ParallelLogger::stopCluster(cluster)

  return(NULL)
}




#### Non-exports ####

psDensity <- function(population) {

  treatmentDensity <- density(
    population %>%
      dplyr::filter(
        treatment == 1
      ) %>%
      dplyr::select(
        preferenceScore
      ) %>%
      unlist()
  )
  comparatorDensity <- density(
    population %>%
      dplyr::filter(
        treatment == 0
      ) %>%
      dplyr::select(
        preferenceScore
      ) %>%
      unlist()
  )

  data.frame(
    x = treatmentDensity$x,
    y = treatmentDensity$y,
    treatment = 1
  ) %>%
    dplyr::bind_rows(
      data.frame(
        x = comparatorDensity$x,
        y = comparatorDensity$y,
        treatment = 0
      )
    ) %>%
    return()


}



#' @export
reanalyzePsMethod <- function(analysisSettings,
                              newRunSettings,
                              newAnalysisSettings,
                              threads = 1){

  previousAnalysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  reRunAnalyses <- function(outcomeId,
                            pathToPs,
                            saveDirectory,
                            runSettings){

    analysisPath <- file.path(
      saveDirectory,
      outcomeId
    )

    if (!dir.exists(analysisPath)) {
      dir.create(
        analysisPath,
        recursive = TRUE
      )
    }

    ps <- readRDS(
      file.path(
        pathToPs,
        outcomeId,
        "ps.rds"
      )
    )

    saveRDS(
      ps,
      file = file.path(
        analysisPath,
        "ps.rds"
      )
    )

    treatmentEffects <- tryCatch({
      estimateTreatmentEffect(
        ps = ps,
        runSettings = runSettings
      )
    },
    error = function(e){
      e$message
    })

    if (!is.character(treatmentEffects)) {

      saveRDS(
        treatmentEffects$ps,
        file = file.path(
          analysisPath,
          'ps.rds'
        )
      )
      saveRDS(
        treatmentEffects$relativeRiskReduction,
        file = file.path(
          analysisPath,
          'relativeRiskReduction.rds'
        )
      )
      saveRDS(
        treatmentEffects$absoluteRiskReduction,
        file = file.path(
          analysisPath,
          'absoluteRiskReduction.rds'
        )
      )
      saveRDS(
        treatmentEffects$models,
        file = file.path(
          analysisPath,
          'models.rds'
        )
      )
      saveRDS(
        treatmentEffects$cases,
        file = file.path(
          analysisPath,
          'cases.rds'
        )
      )
    }
  }

  ParallelLogger::logInfo(
    "Re-analyzing for main outcomes"
  )

  cluster <- ParallelLogger::makeCluster(threads)
  ParallelLogger::clusterRequire(
    cluster,
    c(
      "RiskStratifiedEstimation",
      "CohortMethod"
    )
  )

  pathToPs <- file.path(
    previousAnalysisPath,
    "Estimation"
  )

  saveDirectory <- file.path(
    newAnalysisSettings$saveDirectory,
    newAnalysisSettings$analysisId,
    "Estimation"
  )

  if (!dir.exists(saveDirectory)) {
    dir.create(
      saveDirectory,
      recursive = TRUE
    )
  }

  dummy <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = predictOutcomes,
    fun = reRunAnalyses,
    pathToPs = pathToPs,
    saveDirectory = saveDirectory,
    runSettings = runSettings
  )

  ParallelLogger::stopCluster(cluster)


  for (predictOutcome in predictOutcomes) {

    ParallelLogger::logInfo(
      paste(
        "Re-analyzing for other outcomes in risk strata of",
        predictOutcome
      )
    )
    predLoc <- which(analysisSettings$outcomeIds == predictOutcome)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- compareOutcomes[compareOutcomes != predictOutcome]

    if (length(compareOutcomes) == 0)
      compareOutcomes <- NULL

    if (!is.null(compareOutcomes)) {

      cluster <- ParallelLogger::makeCluster(
        threads
      )
      ParallelLogger::clusterRequire(
        cluster,
        c(
          "RiskStratifiedEstimation",
          "CohortMethod"
        )
      )

      pathToPs <- file.path(
        previousAnalysisPath,
        "Estimation",
        predictOutcome
      )

      saveDirectory <- file.path(
        newAnalysisSettings$saveDirectory,
        newAnalysisSettings$analysisId,
        "Estimation",
        predictOutcome
      )

      if (!dir.exists(saveDirectory)) {
        dir.create(
          saveDirectory,
          recursive = TRUE
        )
      }

      dummy <- ParallelLogger::clusterApply(
        cluster = cluster,
        x = compareOutcomes,
        fun = reRunAnalyses,
        pathToPs = pathToPs,
        saveDirectory = saveDirectory,
        runSettings = runSettings
      )

      ParallelLogger::stopCluster(cluster)

    }
  }
  return(NULL)
}



#' @importFrom dplyr %>%
#' @export

computeIncidence <- function(population,
                             alpha = .05,
                             power = .8,
                             twoSided = TRUE,
                             modelType = "cox"){

  population <- population %>%
    dplyr::mutate(
      outcomeCount = ifelse(
        outcomeCount > 0,
        yes = 1,
        no = 0
      )
    )

  res <-  population %>%
    CohortMethod::computeMdrr() %>%
    dplyr::select(
      -c(
        "targetExposures",
        "comparatorExposures",
        "se"
      )
    )  %>%
    dplyr::rename(
      "treatmentPersons" = "targetPersons",
      "treatmentDays" = "targetDays"
    )

  treatmentArmOutcomes <- population %>%
    dplyr::group_by(
      treatment
    ) %>%
    dplyr::summarise(
      outcomes = sum(
        outcomeCount
      )
    )

  res %>%
    dplyr::mutate(
      treatmentOutcomes = treatmentArmOutcomes %>%
        dplyr::filter(
          treatment == 1
        ) %>%
        dplyr::select(
          outcomes
        ) %>%
        unlist(),
      comparatorOutcomes = treatmentArmOutcomes %>%
        dplyr::filter(
          treatment == 0
        ) %>%
        dplyr::select(
          outcomes
        ) %>%
        unlist()
    ) %>%
    return()

}






#' @importFrom dplyr %>%
#' @export
computeIncidenceOverall <- function(ps,
                                    alpha = 0.05,
                                    power = 0.8,
                                    twoSided = TRUE,
                                    modelType = "cox") {

  lapply(
    ps,
    computeIncidence,
    alpha = alpha,
    power = power,
    twoSided = twoSided,
    modelType = modelType
  ) %>%
    dplyr::bind_rows(
      .id = "riskStratum"
    ) %>%
    dplyr::mutate(
      riskStratum = paste0(
        "Q",
        riskStratum
      )
    ) %>%
    return()

}


#' @importFrom dplyr %>%
#' @export
computeIncidenceCombined <- function(
  outcomeId,
  analysisSettings,
  alpha = 0.05,
  power = 0.8,
  twoSided = TRUE,
  modelType = "cox",
  secondaryOutcomes = FALSE
)
{

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  if (!dir.exists(saveDir)) {
    dir.create(
      saveDir,
      recursive = TRUE
    )
  }

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  ps <- readRDS(
    file.path(
      analysisPath,
      "Estimation",
      outcomeId,
      "ps.rds"
    )
  )

  incidence <-
    computeIncidenceOverall(
      ps,
      alpha = alpha,
      power = power,
      twoSided = twoSided,
      modelType = modelType
    ) %>%
    dplyr::mutate(
      database = analysisSettings$databaseName,
      analysisId = analysisSettings$analysisId,
      stratOutcome = outcomeId,
      estOutcome = outcomeId,
      treatmentId = analysisSettings$treatmentCohortId,
      comparatorId = analysisSettings$comparatorCohortId,
      analysisType = analysisSettings$analysisType
    )

  if (secondaryOutcomes) {

    # failed <- analysisSettings$failed$ps %>%
    #   dplyr::filter(
    #     .$stratOutcome == outcomeId
    #   ) %>%
    #   dplyr::select(
    #     "estOutcome"
    #   ) %>%
    #   unlist(
    #     use.names = FALSE
    #   )

    predLoc <- which(analysisSettings$outcomeIds == outcomeId)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- sort(
      compareOutcomes[compareOutcomes != outcomeId]
    )

    for (compareOutcome in compareOutcomes) {
      dir <- file.path(
        analysisPath,
        "Estimation",
        outcomeId,
        compareOutcome
      )

      ps <- tryCatch(
        {
          readRDS(
            file.path(
              dir,
              "ps.rds"
            )
          )
        },
        error = function(e)
        {
          e$message
        }
      )

      if (is.character(ps))
      {
        next
      }

      incidence <-
        computeIncidenceOverall(ps) %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          stratOutcome = outcomeId,
          estOutcome = compareOutcome,
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          analysisType = analysisSettings$analysisType
        ) %>%
        dplyr::bind_rows(
          incidence
        )

    }
  }

  return(incidence)
}


#' @importFrom dplyr %>%
#' @export
computeIncidenceAnalysis <- function(analysisSettings,
                                     secondaryOutcomes = FALSE,
                                     threads = 1,
                                     alpha = 0.05,
                                     power = 0.8,
                                     twoSided = TRUE,
                                     modelType = "cox"){
  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  cluster <- ParallelLogger::makeCluster(
    threads
  )

  ParallelLogger::clusterRequire(
    cluster,
    c(
      "RiskStratifiedEstimation",
      "dplyr"
    )
  )


  res <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = predictOutcomes,
    fun = computeIncidenceCombined,
    analysisSettings = analysisSettings,
    alpha = alpha,
    power = power,
    twoSided = twoSided,
    modelType = modelType,
    secondaryOutcomes = secondaryOutcomes
  )

  ParallelLogger::stopCluster(cluster)

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  if (!dir.exists(saveDir)) {
    dir.create(
      saveDir,
      recursive = TRUE
    )
  }

  res %>%
    dplyr::bind_rows() %>%
    saveRDS(
      file.path(
        saveDir,
        "incidence.rds"
      )
    )
}



#' @importFrom dplyr %>%
#' @export

predictionPerformance <- function(outcomeId,
                                  analysisSettings) {

  recoverEvaluation <- function(cohort,
                                outcomeId,
                                analysisSettings) {

    analysisPath <- file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "Prediction",
      outcomeId,
      analysisSettings$analysisId,
      cohort
    )

    performance <- readRDS(
      file.path(
        analysisPath,
        "performanceEvaluation.rds"
      )
    )$evaluationStatistics

    performance <- as.data.frame(
      performance
    )
    rownames(performance) <- NULL

    performance %>%
      dplyr::mutate(
        Value = as.numeric(
          as.character(
            Value
          )
        )
      ) %>%
      dplyr::filter(
        Metric %in% c(
          "AUC.auc",
          "AUC.auc_lb95ci",
          "AUC.auc_ub95ci",
          "AUPRC",
          "BrierScore",
          "BrierScaled",
          "CalibrationIntercept",
          "CalibrationSlope",
          "CalibrationInLarge"
        )
      ) %>%
      dplyr::mutate(
        metric = c(
          "auc",
          "aucLower",
          "aucUpper",
          "auprc",
          "brierScore",
          "brierScaled",
          "calibrationIntercept",
          "calibrationSlope",
          "calibrationInLarge"
        )
      ) %>%
      dplyr::select(
        metric,
        Value
      ) %>%
      tidyr::spread(
        metric,
        Value
      ) %>%
      dplyr::mutate(
        cohort = cohort,
        analysisId = analysisSettings$analysisId,
        stratOutcome = outcomeId,
        treatmentId = analysisSettings$treatmentCohortId,
        comparatorId = analysisSettings$comparatorCohortId,
        database = analysisSettings$databaseName
      ) %>%
      return()


  }

  cohorts <- c(
    "Comparator",
    "EntirePopulation",
    "Matched",
    "Treatment"
  )

  lapply(
    cohorts,
    recoverEvaluation,
    analysisSettings = analysisSettings,
    outcomeId = outcomeId
  ) %>%
    dplyr::bind_rows() %>%
    return()


}


#' @importFrom dplyr %>%
#' @export

predictionPerformanceAnalysis <- function(analysisSettings,
                                          save = TRUE) {

  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  performance <- lapply(
    predictOutcomes,
    predictionPerformance,
    analysisSettings
  ) %>%
    dplyr::bind_rows()

  if (save) {

    saveDir <- file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "shiny"
    )

    if (!dir.exists(saveDir)) {
      dir.create(
        saveDir,
        recursive = TRUE
      )
    }

    saveRDS(performance,
            file.path(
              saveDir,
              "predictionPerformance.rds"
            )
    )
  }

  return(performance)
}
