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
) {

  if (is.null(analysisSettings$verbosity)) {
    analysisSettings$verbosity <- "INFO"
  } else {
    if (!analysisSettings$verbosity %in% c("DEBUG","TRACE","INFO","WARN","FATAL","ERROR")) {
      stop(
        'Incorrect verbosity string'
      )
    }
  }

  start.all <- Sys.time()

  if (is.null(analysisSettings$analysisId)) {
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

  if (is.null(analysisSettings$saveDirectory)) {
    analysisSettings$saveDirectory <- file.path(
      getwd(),
      'RSEE'
    )
  }

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  shinyDir <- file.path(
    analysisPath,
    "shiny"
  )

  if (!dir.exists(shinyDir)) {
    dir.create(
      shinyDir,
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

  #-----------------------------------------------------------------------------
  # extract the second element of a list of lists (here the stratificationOutcome)
  #-----------------------------------------------------------------------------
 predictOutcomes <- unlist(
    rlist::list.map(
      runSettings$runCmSettings$analyses,
      stratificationOutcome
    )
  )
  analysisLabels <- unlist(
    rlist::list.map(
      runSettings$runCmSettings$analyses,
      label
    )
  )


  names(analysisLabels) <- names(predictOutcomes) <- NULL
  names(runSettings$runCmSettings$analyses) <- analysisLabels
  analysisSettings$analysisLabels <- analysisLabels


  #-----------------------------------------------------------------------------
  # Overall results step
  #-----------------------------------------------------------------------------
  ParallelLogger::logInfo(
    "Merging the treatment and comparator cohorts"
  )

  if (is.null(getDataSettings$plpDataFolder)) {
    prepareForPlpData(
      treatmentCohortId     = analysisSettings$treatmentCohortId,
      comparatorCohortId    = analysisSettings$comparatorCohortId,
      targetCohortId        = databaseSettings$targetCohortId,
      cohortDatabaseSchema  = databaseSettings$cohortDatabaseSchema,
      cohortTable           = databaseSettings$cohortTable,
      resultsDatabaseSchema = databaseSettings$resultsDatabaseSchema,
      mergedCohortTable     = databaseSettings$mergedCohortTable,
      connectionDetails     = connectionDetails
    )
    ParallelLogger::logInfo(
      "Constructing the plpData object"
    )

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

    plpData <- PatientLevelPrediction::getPlpData(
      databaseDetails = PatientLevelPrediction::createDatabaseDetails(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = databaseSettings$cdmDatabaseSchema,
        cdmDatabaseName = databaseSettings$databaseName,
        tempEmulationSchema = databaseSettings$tempEmulationSchema,
        cohortDatabaseSchema = databaseSettings$resultsDatabaseSchema,
        cohortTable = databaseSettings$mergedCohortTable,
        outcomeDatabaseSchema = databaseSettings$outcomeDatabaseSchema,
        outcomeTable = databaseSettings$outcomeTable,
        targetId = databaseSettings$targetCohortId,
        outcomeIds = predictOutcomes,
        cdmVersion = databaseSettings$cdmVersion
      ),
      covariateSettings = covariateSettings$covariateSettingsPlp,
      restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(
        studyStartDate = getDataSettings$getPlpDataSettings$studyStartDate,
        studyEndDate = getDataSettings$getPlpDataSettings$studyEndDate,
        firstExposureOnly = getDataSettings$getPlpDataSettings$firstExposureOnly,
        washoutPeriod = getDataSettings$getPlpDataSettings$washoutPeriod
      )
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
    plpData <- PatientLevelPrediction::loadPlpData(
      getDataSettings$plpDataFolder
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

    getCmDataSettings <- getDataSettings$getCmDataSettings
    cohortMethodData <- CohortMethod::getDbCohortMethodData(
      connectionDetails          = connectionDetails,
      cdmDatabaseSchema          = databaseSettings$cdmDatabaseSchema,
      targetId                   = analysisSettings$treatmentCohortId,
      comparatorId               = analysisSettings$comparatorCohortId,
      outcomeIds                 = c(
        analysisSettings$outcomeIds,
        analysisSettings$negativeControlOutcomes
      ),
      studyStartDate             = getCmDataSettings$studyStartDate,
      studyEndDate               = getCmDataSettings$studyEndDate,
      exposureDatabaseSchema     = databaseSettings$exposureDatabaseSchema,
      exposureTable              = databaseSettings$exposureTable,
      outcomeDatabaseSchema      = databaseSettings$outcomeDatabaseSchema,
      outcomeTable               = databaseSettings$outcomeTable,
      cdmVersion                 = databaseSettings$cdmVersion,
      firstExposureOnly          = getCmDataSettings$firstExposureOnly,
      removeDuplicateSubjects    = getCmDataSettings$removeDuplicateSubjects,
      restrictToCommonPeriod     = getCmDataSettings$restrictToCommonPeriod,
      washoutPeriod              = getCmDataSettings$washoutPeriod,
      maxCohortSize              = getCmDataSettings$maxCohortSize,
      covariateSettings          = covariateSettings$covariateSettingsCm
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
    cohortMethodData <- CohortMethod::loadCohortMethodData(
      getDataSettings$cohortMethodDataFolder
    )
  } else {
    cohortMethodData <- CohortMethod::loadCohortMethodData(
      getDataSettings$cohortMethodDataFolder
    )
  }

  ParallelLogger::logInfo(
    "Done loading data"
  )

  ParallelLogger::logInfo(
    "Creating initial population"
  )

  cohortsCm <- cohortMethodData$cohorts %>%
    dplyr::collect() %>%
    dplyr::mutate(
      cohortStartDate = lubridate::as_date(cohortStartDate)
    )

  cohortsPlp <- plpData$cohorts %>% dplyr::tibble()

  initialPopulation <- cohortsCm %>%
    dplyr::left_join(
      cohortsPlp,
      by = c(
        "rowId",
        "cohortStartDate",
        "daysFromObsStart",
        "daysToCohortEnd",
        "daysToObsEnd"
      )
    )

  if (!is.function(populationSettings$postProcessing)) {
    if (populationSettings$postProcessing != "none") {
      ParallelLogger::logWarn("Post processing is not a function!")
      ParallelLogger::logWarn("Skipping post processing")
    }
  } else {
    ParallelLogger::logInfo("Post processing initial population")
    initialPopulation <- populationSettings$postProcessing(initialPopulation)
  }

  ParallelLogger::logInfo("Fitting overall propensity score models")

  cluster <- ParallelLogger::makeCluster(
    runSettings$runCmSettings$createPsThreads
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
    cluster            = cluster,
    x                  = predictOutcomes,
    fun                = fitPsModelOverall,
    initialPopulation  = initialPopulation,
    getDataSettings    = getDataSettings,
    populationSettings = populationSettings,
    analysisSettings   = analysisSettings,
    runCmSettings      = runSettings$runCmSettings
  )
  ParallelLogger::stopCluster(cluster)

  ParallelLogger::logInfo(
    "Done estimating propensity scores"
  )

  #-----------------------------------------------------------------------------
  # Prediction step
  #-----------------------------------------------------------------------------
  ParallelLogger::logInfo(
    logSep
  )

  ParallelLogger::logInfo(
    "****Starting prediction step****"
  )

  ParallelLogger::registerLogger(
    logger
  )

  cohorts <- plpData$cohorts %>%
    dplyr::mutate(
      cohortStartDate = lubridate::as_date(
        cohortStartDate
      )
    )

  runPlpSettings <- runSettings$runPlpSettings
  predictionResultSaveDirectories <- rep("", length(predictOutcomes))
  timepoints <- existed <- rep(0, length(predictOutcomes))
  for (i in seq_along(predictOutcomes)) {
    predictionSettings <- NULL
    currentPlpSettings <- pullPlpSettings(runPlpSettings, predictOutcomes[i])
    if (class(currentPlpSettings) == "runExistingPlpSettingsArgs") {
      plpResult <- PatientLevelPrediction::loadPlpResult(
        dirPath = currentPlpSettings$plpResultDirectory
      )
      predictionResultSaveDirectories[i] <- currentPlpSettings$plpResultDirectory
      timepoints[i] <- ifelse(
        is.null(currentPlpSettings$predictionSettings$timepoint),
        yes = -1,
        no = currentPlpSettings$predictionSettings$timepoint
      )
      existed[i] <- 1
    } else {
      if (is.null(currentPlpSettings)) {
        predictionSettings <- runPlpSettings$defaultSettings
      } else if (class(currentPlpSettings) == "runPlpAnalysesArgs") {
        predictionSettings <- currentPlpSettings
      }
      predictionResultSaveDirectories[i] = file.path(
        analysisSettings$saveDirectory,
        analysisSettings$analysisId,
        "Prediction",
        predictOutcomes[i],
        analysisSettings$analysisId
      )
      timepoints[i] <- currentPlpSettings$timepoint
    }

    if (!is.null(predictionSettings)) {
      ps <- readRDS(
        file.path(
          analysisSettings$saveDirectory,
          analysisSettings$analysisId,
          "Prediction",
          predictOutcomes[i],
          "psFull.rds"
        )
      )
      pop <- CohortMethod::matchOnPs(
        population = ps,
        caliper    = predictionSettings$matchingSettings$caliper,
        maxRatio   = predictionSettings$matchingSettings$maxRatio
      ) %>%
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

      # ------------------------------------
      # Assign the matched population as
      # the initial prediction population
      # ------------------------------------
      plpData$cohorts <- plpData$cohorts[plpData$cohorts$rowId %in% startingPop$rowId, ]

      attr(startingPop, "metaData")$attrition <- NULL

      populationPlpSettings <- populationSettings$populationPlpSettings
      predictionResults <- PatientLevelPrediction::runPlp(
        analysisId = analysisSettings$analysisId,
        plpData = plpData,
        outcomeId = predictOutcomes[i],
        populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
          binary                         = populationPlpSettings$binary,
          includeAllOutcomes             = populationPlpSettings$includeAllOutcomes,
          firstExposureOnly              = populationPlpSettings$firstExposureOnly,
          washoutPeriod                  = populationPlpSettings$washoutPeriod,
          removeSubjectsWithPriorOutcome = populationPlpSettings$removeSubjectsWithPriorOutcome,
          priorOutcomeLookback           = populationPlpSettings$priorOutcomeLookback,
          requireTimeAtRisk              = populationPlpSettings$requireTimeAtRisk,
          minTimeAtRisk                  = populationPlpSettings$minTimeAtRisk,
          riskWindowStart                = populationPlpSettings$riskWindowStart,
          startAnchor                    = populationPlpSettings$startAnchor,
          riskWindowEnd                  = populationPlpSettings$riskWindowEnd,
          endAnchor                      = populationPlpSettings$endAnchor,
          restrictTarToCohortEnd         = populationPlpSettings$restrictTarToCohortEnd
        ),
        splitSettings = predictionSettings$splitSettings,
        sampleSettings = predictionSettings$sampleSettings,
        featureEngineeringSettings = predictionSettings$featureEngineeringSettings,
        preprocessSettings = predictionSettings$preprocessSettings,
        modelSettings = predictionSettings$modelSettings,
        logSettings = predictionSettings$logSettings,
        executeSettings = predictionSettings$executeSettings,
        saveDirectory = file.path(
          analysisPath,
          "Prediction",
          predictOutcomes[i]
        )
      )
    }
  }

  runSettings$runPlpSettings$plpResults <- data.frame(
    outcomeId = predictOutcomes,
    directory = predictionResultSaveDirectories,
    timepoint = timepoints,
    existed   = existed
  )

  ParallelLogger::logInfo(
    "Evaluating prediction models"
  )

  nThreads <- analysisSettings$balanceThreads

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
        populationSettings = populationSettings,
        runPlpSettings = runSettings$runPlpSettings
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


  #-----------------------------------------------------------------------------
  # Estimation step
  #-----------------------------------------------------------------------------
  ParallelLogger::logInfo(
    "****Starting estimation step****"
  )

  #-----------------------------------------------------------------------------
  # Propensity score estimation main outcomes
  #-----------------------------------------------------------------------------
  ParallelLogger::logInfo(
    "Starting propensity score estimation for main outcomes"
  )

  cluster <- ParallelLogger::makeCluster(
    runSettings$runCmSettings$createPsThreads
  )

  ParallelLogger::clusterRequire(
    cluster,
    "RiskStratifiedEstimation"
  )
  ParallelLogger::clusterRequire(
    cluster,
    "CohortMethod"
  )
  ParallelLogger::clusterRequire(
    cluster,
    "dplyr"
  )

  dummy <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = seq_along(runSettings$runCmSettings$analyses),
    fun = fitPsModel,
    initialPopulation = initialPopulation,
    analysisSettings = analysisSettings,
    getDataSettings = getDataSettings,
    populationSettings = populationSettings,
    runSettings = runSettings
  )

  ParallelLogger::stopCluster(
    cluster
  )

  #-----------------------------------------------------------------------------
  # Propensity score estimation secondary outcomes
  #-----------------------------------------------------------------------------
  ParallelLogger::logInfo(
    "Starting propensity score estimation for secondary outcomes"
  )

  for (i in seq_along(runSettings$runCmSettings$analyses)) {

    currentRunCmAnalysis <- runSettings$runCmSettings$analyses[[i]]
    ParallelLogger::logInfo(
      paste(
        "Fitting propensity scores for analysis:",
        currentRunCmAnalysis$label
      )
    )

    stratificationOutcome <- currentRunCmAnalysis$stratificationOutcome
    compareOutcomes <- currentRunCmAnalysis$estimationOutcomes
    compareOutcomes <- compareOutcomes[compareOutcomes != stratificationOutcome]

    if (length(compareOutcomes) == 0) {
      next
    } else {
      cluster <- ParallelLogger::makeCluster(
        runSettings$runCmSettings$createPsThreads
      )

      ParallelLogger::clusterRequire(
        cluster,
        "RiskStratifiedEstimation"
      )
      ParallelLogger::clusterRequire(
        cluster,
        "CohortMethod"
      )
      ParallelLogger::clusterRequire(
        cluster,
        "dplyr"
      )

      dummy <- tryCatch(
        {
          ParallelLogger::clusterApply(
            cluster = cluster,
            x = compareOutcomes,
            fun = fitPsModelSwitch,
            analysis = currentRunCmAnalysis,
            predictOutcome = stratificationOutcome,
            initialPopulation = initialPopulation,
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

  mergeMultipleTempFiles <- function(
    path,
    outcomeId,
    mergeTempFiles,
    fileNames
  ) {
    lapply(
      fileNames,
      mergeTempFiles,
      path = path,
      outcomeId = outcomeId
    )
  }

  cluster <- ParallelLogger::makeCluster(
    runSettings$runCmSettings$fitOutcomeModelsThreads
  )

  ParallelLogger::clusterRequire(
    cluster,
    "RiskStratifiedEstimation"
  )

  ParallelLogger::clusterRequire(
    cluster,
    "CohortMethod"
  )

  for (i in seq_along(analysisLabels)) {
    ParallelLogger::logInfo(
      paste(
        "Estimating results for the analysis:",
        analysisLabels[i]
      )
    )
    pathToPs <- file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "Estimation",
      analysisLabels[i],
      runSettings$runCmSettings$analyses[[i]]$stratificationOutcome
    )
    outcomeIds <- as.numeric(
      list.dirs(
        path       = pathToPs,
        recursive  = FALSE,
        full.names = FALSE
      )
    ) %>% sort()
    tmpSettings <- runSettings$runCmSettings$analyses[[i]]
    dummy <- tryCatch(
      {
        ParallelLogger::clusterApply(
          cluster = cluster,
          x = outcomeIds,
          fun = fitOutcomeModels,
          getDataSettings = getDataSettings,
          pathToPs = pathToPs,
          analysis = tmpSettings,
          analysisId = analysisSettings$analysisId
        )
      },
      error = function(e) {
        e$message
      }
    )
  }

  ParallelLogger::logInfo(
    "Computing and saving incidence"
  )

  computeRseeIncidence(analysisSettings)

  ParallelLogger::logInfo(
    "Merging..."
  )
  mergeTempFiles(
    shinyDir,
    paste("incidence", analysisSettings$analysisId, sep = "_")
  )

  ParallelLogger::logInfo(
    "Computing and saving propensity score density"
  )

  computeRseePsDensity(analysisSettings)

  ParallelLogger::logInfo(
    "Computing and saving covariate balance. This may take a while..."
  )

  computeRseeCovariateBalance(
    analysisSettings = analysisSettings,
    getDataSettings  = getDataSettings
  )


  #-----------------------------------------------------------------------------
  # Inclusion of overall results
  #-----------------------------------------------------------------------------
  ParallelLogger::logInfo(
    crayon::blue(
      "Generating overall results for all analyses..."
    )
  )
  includeOverallResults(
    analysisSettings = analysisSettings,
    getDataSettings = getDataSettings,
    runSettings = runSettings
  )

  #=============================================================================
  # Negative controls
  #=============================================================================
  if (!is.null(analysisSettings$negativeControlOutcomes)) {
    ParallelLogger::logInfo("Starting negative control analyses")
    cluster <- ParallelLogger::makeCluster(
      runSettings$runCmSettings$negativeControlThreads
    )

    ParallelLogger::clusterRequire(
      cluster,
      "RiskStratifiedEstimation"
    )
    ParallelLogger::clusterRequire(
      cluster,
      "CohortMethod"
    )
    negativeControlIds <- analysisSettings$negativeControlOutcomes

    ParallelLogger::logInfo("Fitting overall propensity score models")
    dummy <- ParallelLogger::clusterApply(
      cluster            = cluster,
      x                  = negativeControlIds,
      fun                = fitPsModelOverall,
      initialPopulation  = initialPopulation,
      getDataSettings    = getDataSettings,
      populationSettings = populationSettings,
      analysisSettings   = analysisSettings,
      runCmSettings      = runSettings$runCmSettings,
      isNegativeControl  = TRUE
    )
    ParallelLogger::logInfo("Fitting overall models for negative controls")
    includeOverallResultsNegativeControls(
      analysisSettings  = analysisSettings,
      getDataSettings   = getDataSettings,
      runSettings       = runSettings
    )

    if (runSettings$runCmSettings$runRiskStratifiedNcs) {
      for (i in seq_along(runSettings$runCmSettings$analyses)) {

        currentRunCmAnalysis <- runSettings$runCmSettings$analyses[[i]]
        ParallelLogger::logInfo(
          paste(
            "Fitting PS models for negative controls within risk strata of:",
            currentRunCmAnalysis$label
          )
        )

        stratificationOutcome <- currentRunCmAnalysis$stratificationOutcome

        dummy <- ParallelLogger::clusterApply(
          cluster = cluster,
          x = negativeControlIds,
          fun = fitPsModelSwitch,
          analysis = currentRunCmAnalysis,
          predictOutcome = stratificationOutcome,
          initialPopulation = initialPopulation,
          analysisSettings = analysisSettings,
          getDataSettings = getDataSettings,
          populationSettings = populationSettings,
          runSettings = runSettings
        )

        ParallelLogger::logInfo(
          paste(
            "Fitting outcome models for negative controls for analysis: ",
            currentRunCmAnalysis$label
          )
        )
        analysis <- runSettings$runCmSettings$analyses[[i]]
        pathToPs <- file.path(
          analysisSettings$saveDirectory,
          analysisSettings$analysisId,
          "Estimation",
          currentRunCmAnalysis$label,
          stratificationOutcome
        )

        dummy <- tryCatch({
          ParallelLogger::clusterApply(
            cluster = cluster,
            x = negativeControlIds,
            fun = fitOutcomeModels,
            getDataSettings = getDataSettings,
            pathToPs = pathToPs,
            analysis = analysis,
            analysisId = analysisSettings$analysisId
          )},
          error = function(e) {
            e$message
          }
        )
      }
    }
  }

  ParallelLogger::logInfo(
    "Creating and saving overall results"
  )

  createOverallResults(
    analysisSettings = analysisSettings,
    runSettings = runSettings
  )
  ParallelLogger::logInfo("Saving analysis settings")

  settings <- list(
    analysisSettings   = analysisSettings,
    getDataSettings    = getDataSettings,
    databaseSettings   = databaseSettings,
    covariateSettings  = covariateSettings,
    populationSettings = populationSettings,
    runSettings        = runSettings
  )

  saveRDS(
    settings,
    file.path(
      analysisSettings$saveDirectory,
      analysisSettings$analysisId,
      "settings.rds"
    )
  )

  ParallelLogger::logInfo("Run finished successfully")

  # stop logger
  ParallelLogger::clearLoggers()

  logger <- ParallelLogger::createLogger(
    name      = "SIMPLE",
    threshold = "INFO",
    appenders = list(
      ParallelLogger::createConsoleAppender(
        layout = ParallelLogger::layoutTimestamp
      )
    )
  )

  return(analysisSettings)
}
