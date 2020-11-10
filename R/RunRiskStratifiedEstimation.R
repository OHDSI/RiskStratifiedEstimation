# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of RiskStratifiedEstimation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# @author Observational Health Data Sciences and Informatics
# @author Alexandros Rekkas
# @author Peter Rijnbeek


#' Runs a risk stratified analysis
#'
#' Runs a risk stratified analysis in two stages. It first runs a prediction algorithm using
#' \code{PatientLevelPrediction} to derive baseline patient risks and then derives estimates
#' within risk strata incorporating functionality from \code{CohortMethod} package.
#'
#' @param connectionDetails          An R object of type \code{connectionDetails} created using the function
#'                                   \code{\link[DatabaseConnector]{createConnectionDetails}}.
#' @param analysisSettings           An R object of type \code{analysisSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createAnalysisSettings}}.
#' @param databaseSettings           An R object of type \code{databaseSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createDatabaseSettings}}.
#' @param getDataSettings            An R object of type \code{getDataSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createGetDataSettings}}.
#' @param covariateSettings          An R object of type \code{covariateSettings} created using the function
#'                                   \code{\link[FeatureExtraction]{createCovariateSettings}}.
#' @param populationSettings         An R object of type \code{populationSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createPopulationSettings}}.
#' @param runSettings                An R object of type \code{runSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createRunSettings}}.
#'
#'
#' @return                           The function saves all results based on \code{analysisSettings}. No
#'                                   results are returned.
#'
#' @import data.table
#'
#' @export

runRiskStratifiedEstimation <- function(
  connectionDetails,
  analysisSettings,
  databaseSettings,
  getDataSettings,
  covariateSettings,
  populationSettings,
  runSettings
)
{

  if (is.null(analysisSettings$verbosity))
  {
    analysisSettings$verbosity <- "INFO"
  }
  else
  {
    if (!analysisSettings$verbosity %in% c("DEBUG","TRACE","INFO","WARN","FATAL","ERROR")) {
      stop(
        'Incorrect verbosity string'
      )
    }
  }
  start.all <- Sys.time()
  if (is.null(analysisSettings$analysisId))
  {
    analysisSettings$analysisId <- paste(
      gsub(
        ':',
        '',
        gsub(
          '-',
          '',
          gsub(
            ' ',
            '',
            start.all
          )
        )
      )
    )
  }

  if (is.null(analysisSettings$saveDirectory))
  {
    analysisSettings$saveDirectory <- file.path(
      getwd(),
      'RSEE'
    )
  }

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  if (!dir.exists(analysisPath))
  {
    dir.create(
      analysisPath,
      recursive = TRUE
    )
  }

  logFileName = paste0(
    analysisPath,
    '/logRSEE.txt'
  )

  logger <- ParallelLogger::createLogger(
    name = "RSEE Main Log",
    threshold = analysisSettings$verbosity,
    appenders = list(
      ParallelLogger::createFileAppender(
        layout = ParallelLogger::layoutParallel,
        fileName = logFileName
      )
    )
  )

  ParallelLogger::registerLogger(
    logger
  )

  logSep <- paste(
    rep(
      '*',
      96
    ),
    collapse = ''
  )

  predictOutcomes <- analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]


  #######################
  # Overall results step
  #######################
  ParallelLogger::logInfo(
    "Merging the treatment and comparator cohorts"
  )



  if (is.null(getDataSettings$plpDataFolder)) {
    prepareForPlpData(
      treatmentCohortId = analysisSettings$treatmentCohortId,
      comparatorCohortId = analysisSettings$comparatorCohortId,
      targetCohortId = databaseSettings$targetCohortId,
      cohortDatabaseSchema = databaseSettings$cohortDatabaseSchema,
      cohortTable = databaseSettings$cohortTable,
      resultsDatabaseSchema = databaseSettings$resultsDatabaseSchema,
      mergedCohortTable = databaseSettings$mergedCohortTable,
      connectionDetails = connectionDetails
    )
    ParallelLogger::logInfo(
      "Constructing the plpData object"
    )

    dataPath <- file.path(
      analysisPath,
      "Data"
    )

    if (!dir.exists(dataPath))
    {
      dir.create(
        dataPath,
        recursive = TRUE
      )
    }

    plpData <- PatientLevelPrediction::getPlpData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = databaseSettings$cdmDatabaseSchema,
      cohortId = databaseSettings$targetCohortId,
      outcomeIds = predictOutcomes,
      cohortDatabaseSchema = databaseSettings$resultsDatabaseSchema,
      cohortTable = databaseSettings$mergedCohortTable,
      outcomeDatabaseSchema = databaseSettings$outcomeDatabaseSchema,
      outcomeTable = databaseSettings$outcomeTable,
      studyStartDate = getDataSettings$getPlpDataSettings$studyStartDate,
      studyEndDate = getDataSettings$getPlpDataSettings$studyEndDate,
      cdmVersion = databaseSettings$cdmVersion,
      firstExposureOnly = getDataSettings$getPlpDataSettings$firstExposureOnly,
      washoutPeriod = getDataSettings$getPlpDataSettings$washoutPeriod,
      excludeDrugsFromCovariates = getDataSettings$getPlpDataSettings$excludeDrugsFromCovariates,
      covariateSettings = covariateSettings$covariateSettingsPlp
    )
    PatientLevelPrediction::savePlpData(
      plpData,
      file = file.path(
        dataPath,
        "plpData"
      )
    )
    getDataSettings$plpDataFolder <- file.path(
      analysisPath,
      "Data",
      "plpData"
    )
  } else {
    plpData <- PatientLevelPrediction::loadPlpData(
      getDataSettings$plpDataFolder
    )
  }

  if (is.null(getDataSettings$cohortMethodDataFolder)) {

    dataPath <- file.path(
      analysisPath,
      "Data"
    )

    if (!dir.exists(dataPath)) {
      dir.create(
        dataPath,
        recursive = TRUE
      )
    }

    cohortMethodData <- CohortMethod::getDbCohortMethodData(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = databaseSettings$cdmDatabaseSchema,
      targetId = analysisSettings$treatmentCohortId,
      comparatorId = analysisSettings$comparatorCohortId,
      outcomeIds = analysisSettings$outcomeIds,
      studyStartDate = getDataSettings$getCmDataSettings$studyStartDate,
      studyEndDate = getDataSettings$getCmDataSettings$studyEndDate,
      exposureDatabaseSchema = databaseSettings$exposureDatabaseSchema,
      exposureTable = databaseSettings$exposureTable,
      outcomeDatabaseSchema = databaseSettings$outcomeDatabaseSchema,
      outcomeTable = databaseSettings$outcomeTable,
      cdmVersion = databaseSettings$cdmVersion,
      excludeDrugsFromCovariates = getDataSettings$getCmDataSettings$excludeDrugsFromCovariates,
      firstExposureOnly = getDataSettings$getCmDataSettings$firstExposureOnly,
      removeDuplicateSubjects = getDataSettings$getCmDataSettings$removeDuplicateSubjects,
      restrictToCommonPeriod = getDataSettings$getCmDataSettings$restrictToCommonPeriod,
      washoutPeriod = getDataSettings$getCmDataSettings$washoutPeriod,
      maxCohortSize = getDataSettings$getCmDataSettings$maxCohortSize,
      covariateSettings = covariateSettings$covariateSettingsCm
    )

    CohortMethod::saveCohortMethodData(
      cohortMethodData,
      file.path(
        analysisPath,
        "Data",
        "cmData"
      )
    )
    getDataSettings$cohortMethodDataFolder <- file.path(
      analysisPath,
      "Data",
      "cmData"
    )
  } else {
    cohortMethodData <- CohortMethod::loadCohortMethodData(
      getDataSettings$cohortMethodDataFolder
    )
  }

  ParallelLogger::logInfo(
    "Done loading data"
  )


  cluster <- ParallelLogger::makeCluster(
    runSettings$runCmSettings[[1]]$createPsThreads
  )

  ParallelLogger::clusterRequire(
    cluster,
    "RiskStratifiedEstimation"
  )
  ParallelLogger::clusterRequire(
    cluster,
    "CohortMethod"
  )

  dummy <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = predictOutcomes,
    fun = fitPsModelOverall,
    getDataSettings = getDataSettings,
    populationSettings = populationSettings,
    analysisSettings = analysisSettings,
    runCmSettings = runSettings$runCmSettings[[1]]
  )
  ParallelLogger::stopCluster(cluster)

  ParallelLogger::logInfo(
    "Done estimating propensity scores"
  )

  #######################
  # Prediction step
  #######################
  ParallelLogger::logInfo(
    logSep
  )

  ParallelLogger::logInfo(
    "****Starting prediction step****"
  )

  ParallelLogger::registerLogger(
    logger
  )

  for (id in predictOutcomes) {
    ps <- readRDS(
      file.path(
        analysisSettings$saveDirectory,
        analysisSettings$analysisId,
        "Estimation",
        id,
        "psFull.rds"
      )
    )
    pop <- CohortMethod::matchOnPs(
      ps
    ) %>%
      dplyr::mutate(
        cohortStartDate = lubridate::as_date(
          cohortStartDate
        )
      )
    cohorts <- plpData$cohorts %>%
      dplyr::mutate(
        cohortStartDate = lubridate::as_date(
          cohortStartDate
        )
      )

    startingPop <- pop %>%
      dplyr::left_join(
        plpData$cohorts
      ) %>%
      dplyr::select(
        -"daysToEvent"
      )

    # startingPop <- cohorts %>%
    #   dplyr::filter(
    #     subjectId %in% !!pop$subjectId
    #   )

    attr(startingPop, "metaData")$attrition <- NULL

    population <- PatientLevelPrediction::createStudyPopulation(
      plpData = plpData,
      outcomeId = id,
      population = startingPop,
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

    attr(population, "metaData")$cohortId <- 1

    predictionResults <- PatientLevelPrediction::runPlp(
      population = population,
      plpData = plpData,
      modelSettings = runSettings$runPlpSettings$modelSettings,
      minCovariateFraction = runSettings$runPlpSettings$minCovariateFraction,
      normalizeData = runSettings$runPlpSettings$normalizeData,
      testSplit = runSettings$runPlpSettings$testSplit,
      testFraction = runSettings$runPlpSettings$testFraction,
      trainFraction = runSettings$runPlpSettings$trainFraction,
      nfold = runSettings$runPlpSettings$nfold,
      indexes = runSettings$runPlpSettings$indexes,
      savePlpData = runSettings$runPlpSettings$savePlpData,
      savePlpResult = TRUE,
      savePlpPlots = FALSE,
      saveEvaluation = runSettings$runPlpSettings$saveEvaluation,
      verbosity = runSettings$runPlpSettings$verbosity,
      timeStamp = runSettings$runPlpSettings$timeStamp,
      analysisId = analysisSettings$analysisId,
      saveDirectory = file.path(
        analysisPath,
        "Prediction",
        id
      )
    )
  }

  ParallelLogger::logInfo(
    paste(
      "Estimated prediction models for outcomes",
      predictOutcomes
    )
  )

  runSettings$runPlpSettings$plpResults <- data.frame(
    outcomeId = predictOutcomes,
    directory = file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "Prediction",
      predictOutcomes,
      analysisSettings$analysisId
    )
  ) %>%
    dplyr::bind_rows(
      runSettings$runPlpSettings$plpResults
    ) %>%
    dplyr::mutate(
      existed = ifelse(
        outcomeId %in% runSettings$runPlpSettings$plpResults$outcomeId,
        yes = 1,
        no = 0
      )
    )


  #######################
  # Estimation step
  #######################
  ParallelLogger::logInfo(
    "****Starting estimation step****"
  )

  ParallelLogger::logInfo(
    "Starting propensity score estimation for main outcomes"
  )

  cluster <- ParallelLogger::makeCluster(
    runSettings$runCmSettings[[1]]$createPsThreads
  )

  ParallelLogger::clusterRequire(
    cluster,
    c(
      "RiskStratifiedEstimation",
      "CohortMethod"
    )
  )


  dummy <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = predictOutcomes,
    fun = fitPsModel,
    analysisSettings = analysisSettings,
    getDataSettings = getDataSettings,
    populationSettings = populationSettings,
    runSettings = runSettings
  )

  ParallelLogger::stopCluster(
    cluster
  )

  ParallelLogger::logInfo(
    "Starting propensity score estimation for secondary outcomes"
  )

  for (predictOutcome in predictOutcomes) {
    predLoc <- which(analysisSettings$outcomeIds == predictOutcome)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- sort(
      compareOutcomes[compareOutcomes != predictOutcome]
    )

    if (length(compareOutcomes) == 0)
    {
      compareOutcomes <- NULL
    }

    if (!is.null(compareOutcomes))
    {
      cluster <- ParallelLogger::makeCluster(
        runSettings$runCmSettings[[1]]$createPsThreads
      )

      ParallelLogger::clusterRequire(
        cluster,
        c(
          "RiskStratifiedEstimation",
          "CohortMethod"
        )
      )

      dummy <- tryCatch(
        {
          ParallelLogger::clusterApply(
            cluster = cluster,
            x = compareOutcomes,
            fun = fitPsModelSwitch,
            predictOutcome = predictOutcome,
            analysisSettings = analysisSettings,
            getDataSettings = getDataSettings,
            populationSettings = populationSettings,
            runSettings = runSettings
          )
        },
        error = function(e)
        {
          e$message
        }
      )

      ParallelLogger::stopCluster(
        cluster
      )
    }
  }

  ParallelLogger::logInfo(
    "Done estimating propensity scores"
  )

  ParallelLogger::logInfo(
    "Starting estimation of results for main outcomes"
  )

  analysisLabels <- unlist(
    rlist::list.map(                     # extract the second element of a
      runSettings$runCmSettings,         # list of lists (here the label)
      .[2]
    )
  )

  names(analysisLabels) <- NULL
  analysisSettings$analysisLabels <- analysisLabels

  mergeMultipleTempFiles <- function(
    pathToPs,
    outcomeId,
    mergeTempFiles,
    fileNames
  ) {
    lapply(
      fileNames,
      mergeTempFiles,
      pathToPs = pathToPs,
      outcomeId = outcomeId
    )
  }

  cluster <- ParallelLogger::makeCluster(
    runSettings$runCmSetting[[1]]$fitOutcomeModelsThreads      # only the first is used
  )                                                            # for the cluster

  ParallelLogger::clusterRequire(
    cluster,
    c(
      "RiskStratifiedEstimation",
      "CohortMethod"
    )
  )

  for (i in seq_along(analysisLabels)) {

    ParallelLogger::logInfo(
      paste(
        "Estimating results for the analysis:",
        analysisLabels[i]
      )
    )
    settings <- runSettings$runCmSettings[[i]]

    pathToPs <- file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "Estimation"
    )

    dummy <- tryCatch(
      {
        ParallelLogger::clusterApply(
          cluster = cluster,
          x = predictOutcomes,
          fun = fitOutcomeModels,
          getDataSettings = getDataSettings,
          pathToPs = pathToPs,
          runCmSettings = settings
        )
      },
      error = function(e)
      {
        e$message
      }
    )


    ParallelLogger::logInfo(
      "Starting estimation of results for secondary outcomes"
    )

    for (predictOutcome in predictOutcomes) {
      predLoc <- which(analysisSettings$outcomeIds == predictOutcome)
      compLoc <- analysisSettings$analysisMatrix[, predLoc]
      compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
      compareOutcomes <- sort(
        compareOutcomes[compareOutcomes != predictOutcome]
      )

      if (length(compareOutcomes) == 0) {
        compareOutcomes <- NULL
      }

      if (!is.null(compareOutcomes)) {

        pathToPs <- file.path(
          analysisSettings$saveDirectory,
          analysisSettings$analysisId,
          "Estimation",
          predictOutcome
        )

        dummy <- tryCatch(
          {
            ParallelLogger::clusterApply(
              cluster = cluster,
              x = compareOutcomes,
              fun = fitOutcomeModels,
              getDataSettings = getDataSettings,
              pathToPs = pathToPs,
              runCmSettings = settings
            )
          },
          error = function(e)
          {
            e$message
          }
        )
      }
    }
  }

  ParallelLogger::logInfo(
    "Merging temporary files"
  )

  pathToPs <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "Estimation"
  )

  dummy <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = predictOutcomes,
    fun = mergeMultipleTempFiles,
    fileNames = list(
      "relativeRiskReduction",
      "absoluteRiskReduction",
      "cases"
    ),
    mergeTempFiles = mergeTempFiles,
    pathToPs = pathToPs
  )

  for (predictOutcome in predictOutcomes) {
    predLoc <- which(analysisSettings$outcomeIds == predictOutcome)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- sort(
      compareOutcomes[compareOutcomes != predictOutcome]
    )

    if (length(compareOutcomes) == 0) {
      compareOutcomes <- NULL
    }

    if (!is.null(compareOutcomes)) {
      pathToPs <- file.path(
        analysisSettings$saveDirectory,
        analysisSettings$analysisId,
        "Estimation",
        predictOutcome
      )

      dummy <- ParallelLogger::clusterApply(
        cluster = cluster,
        x = compareOutcomes,
        fun = mergeMultipleTempFiles,
        fileNames = list(
          "relativeRiskReduction",
          "absoluteRiskReduction",
          "cases"
        ),
        mergeTempFiles = mergeTempFiles,
        pathToPs = pathToPs
      )
    }
  }

  ParallelLogger::stopCluster(
    cluster
  )

  ParallelLogger::logInfo(
    "Evaluating prediction models"
  )

  nThreads <- ifelse(
    runSettings$runCmSetting[[1]]$fitOutcomeModelsThreads > runSettings$runCmSettings[[1]]$riskStrata,
    yes = runSettings$runCmSettings[[1]]$riskStrata,
    no = runSettings$runCmSettings[[1]]$createPsThreads
  )

  cluster <- ParallelLogger::makeCluster(
    nThreads
  )

  ParallelLogger::clusterRequire(
    cluster,
    "RiskStratifiedEstimation"
  )

  dummy <- tryCatch(
    {
      ParallelLogger::clusterApply(
        cluster = cluster,
        x = predictOutcomes,
        fun = evaluatePrediction,
        analysisSettings = analysisSettings,
        getDataSettings = getDataSettings,
        populationSettings = populationSettings
      )
    },
    error = function(e)
    {
      e$message
    }
  )

  ParallelLogger::stopCluster(
    cluster
  )

  ParallelLogger::logInfo(
    "Saving prediction model performance"
  )

  predictionPerformanceAnalysis(
    analysisSettings = analysisSettings,
    save = TRUE
  )

  ParallelLogger::logInfo(
    "Computing and saving incidence"
  )

  for (i in seq_along(analysisLabels)) {
    computeIncidenceAnalysis(
      analysisSettings = analysisSettings,
      analysisType = analysisLabels[i],
      secondaryOutcomes = TRUE,
      threads = nThreads
    )
  }

  ParallelLogger::logInfo(
    "Merging..."
  )
  mergeTempFiles(
    file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "shiny"
    ),
    outcomeId = "",
    fileName = "incidence"
  )

  ParallelLogger::logInfo(
    "Computing and saving propensity score density"
  )

  for (i in seq_along(analysisLabels)) {
    subsetRunCmSettings <- runSettings$runCmSettings[[i]]
    psDensityAnalysis(
      analysisSettings = analysisSettings,
      secondaryOutcomes = TRUE,
      threads = nThreads,
      runCmSettings = subsetRunCmSettings
    )
  }

  ParallelLogger::logInfo(
    "Computing and saving covariate balance. This may take a while..."
  )

  for (i in seq_along(analysisLabels)) {
    subsetRunCmSettings <- runSettings$runCmSettings[[i]]
    computeCovariateBalanceAnalysis2(
      analysisSettings = analysisSettings,
      runCmSettings = subsetRunCmSettings,
      getDataSettings = getDataSettings,
      balanceThreads = analysisSettings$balanceThreads
    )
  }

  ParallelLogger::logInfo(
    "Creating and saving overall results"
  )

  createOverallResults(
    analysisSettings
  )

  settings <- list(
    analysisSettings = analysisSettings,
    getDataSettings = getDataSettings,
    databaseSettings = databaseSettings,
    covariateSettings = covariateSettings,
    populationSettings = populationSettings,
    runSettings = runSettings
  )

  for (i in seq_along(analysisLabels)) {
    includeOverallResults(
      analysisSettings = analysisSettings,
      getDataSettings = getDataSettings,
      runCmSettings = runSettings$runCmSettings[[i]]
    )
  }

  ParallelLogger::logInfo(
    "Merging..."
  )

  lapply(
    c("mappedOverallResults", "incidenceOverall"),
    mergeTempFiles,
    pathToPs = file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "shiny"
    ),
    outcomeId = ""
  )

  saveRDS(
    settings,
    file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "settings.rds"
    )
  )


  ParallelLogger::logInfo(
    'Run finished successfully'
  )

  # stop logger
  ParallelLogger::clearLoggers()

  logger <- ParallelLogger::createLogger(
    name = "SIMPLE",
    threshold = "INFO",
    appenders = list(
      ParallelLogger::createConsoleAppender(
        layout = ParallelLogger::layoutTimestamp
      )
    )
  )

  return(analysisSettings)
}
