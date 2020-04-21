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
#' @return                           The function saves all results based on \code{analysisSettings}. No
#'                                   results are returned.
#'
#'
#' @export

runRiskStratifiedEstimation <- function(connectionDetails,
                                        analysisSettings,
                                        databaseSettings,
                                        getDataSettings,
                                        covariateSettings,
                                        populationSettings,
                                        runSettings){

  if(is.null(analysisSettings$verbosity)){
    analysisSettings$verbosity <- "INFO"
  } else{
    if(!analysisSettings$verbosity %in% c("DEBUG","TRACE","INFO","WARN","FATAL","ERROR")){
      stop('Incorrect verbosity string')
    }
  }

  ExecutionDateTime <- Sys.time()

  start.all <- Sys.time()
  if(is.null(analysisSettings$analysisId))
    analysisSettings$analysisId <- paste(gsub(':','',gsub('-','',gsub(' ','',start.all))))

  if(is.null(analysisSettings$saveDirectory)) analysisSettings$saveDirectory <- file.path(getwd(),'RSEE')


  analysisPath <- file.path(analysisSettings$saveDirectory, analysisSettings$analysisId)
  if(!dir.exists(analysisPath)){dir.create(analysisPath, recursive=T)}
  logFileName = paste0(analysisPath,'/logRSEE.txt')

  logger <-
    ParallelLogger::createLogger(name = "RSEE Main Log",
                                 threshold = analysisSettings$verbosity,
                                 appenders =
                                   list(ParallelLogger::createFileAppender(layout = ParallelLogger::layoutParallel,
                                                                           fileName = logFileName)))
  ParallelLogger::registerLogger(logger)
  logSep <- paste(rep('*', 96), collapse = '')

  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]


  #######################
  # Overall results step
  #######################
  ParallelLogger::logInfo("Merging the treatment and comparator cohorts")



  if(is.null(getDataSettings$plpDataFolder)){

    prepareForPlpData(
      treatmentCohortId = analysisSettings$treatmentCohortId,
      comparatorCohortId = analysisSettings$comparatorCohortId,
      targetCohortId = databaseSettings$targetCohortId,
      cohortDatabaseSchema = databaseSettings$cohortDatabaseSchema,
      cohortTable = databaseSettings$cohortTable,
      resultsDatabaseSchema = databaseSettings$resultsDatabaseSchema,
      mergedCohortTable = databaseSettings$mergedCohortTable,
      connectionDetails = connectionDetails)

    ParallelLogger::logInfo("Constructing the plpData object")
    plpData <-
      PatientLevelPrediction::getPlpData(
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
        excludeDrugsFromCovariates =getDataSettings$getPlpDataSettings$excludeDrugsFromCovariates,
        covariateSettings = covariateSettings$covariateSettingsPlp)


    PatientLevelPrediction::savePlpData(plpData, file = file.path(analysisPath, "Data", "plpData"))
    getDataSettings$plpDataFolder <- file.path(analysisPath, "Data", "plpData")
  }
  else{
    plpData <- PatientLevelPrediction::loadPlpData(getDataSettings$plpDataFolder)
  }

  if(is.null(getDataSettings$cohortMethodDataFolder)){
    cohortMethodDataOutcomes <- c(analysisSettings$outcomeIds,
                                  analysisSettings$negativeControlOutcomes)
    cohortMethodData <-
      CohortMethod::getDbCohortMethodData(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = databaseSettings$cdmDatabaseSchema,
        targetId = analysisSettings$treatmentCohortId,
        comparatorId = analysisSettings$comparatorCohortId,
        outcomeIds = cohortMethodDataOutcomes,
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
        covariateSettings = covariateSettings$covariateSettingsCm)

    CohortMethod::saveCohortMethodData(cohortMethodData, file.path(analysisPath, "Data", "cmData"))
    getDataSettings$cohortMethodDataFolder <- file.path(analysisPath, "Data", "cmData")
  }
  else{
    cohortMethodData <- CohortMethod::loadCohortMethodData(getDataSettings$cohortMethodDataFolder)
  }

  ParallelLogger::logInfo("Done loading data")
  ParallelLogger::logInfo("Starting estimation of overall propensity scores")


  cluster <- ParallelLogger::makeCluster(runSettings$runCmSettings$createPsThreads)
  ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation", "CohortMethod"))
  dummy <-
    ParallelLogger::clusterApply(
      cluster = cluster,
      x = predictOutcomes,
      fun = fitPsModelOverall,
      getDataSettings = getDataSettings,
      populationSettings = populationSettings,
      analysisSettings = analysisSettings,
      runCmSettings = runSettings$runCmSettings)

  ParallelLogger::stopCluster(cluster)

  ParallelLogger::logInfo("Done estimating propensity scores")

  #######################
  # Prediction step
  #######################
  ParallelLogger::logInfo(logSep)
  ParallelLogger::logInfo("****Starting prediction step****")


  ParallelLogger::logInfo("Done")
  ParallelLogger::logInfo("Creating covariate settings")

  ParallelLogger::registerLogger(logger)

  for(id in predictOutcomes){
    ps <- readRDS(file.path(analysisSettings$saveDirectory,
                            analysisSettings$analysisId,
                            "Estimation",
                            id,
                            "psFull.rds"))
    pop <- CohortMethod::matchOnPs(ps)
    cohorts <- plpData$cohorts
    population <- cohorts %>%
      dplyr::filter(subjectId %in% pop$subjectId)
    population <-
      PatientLevelPrediction::createStudyPopulation(
        plpData = plpData,
        outcomeId = id,
        population = population,
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
        verbosity = populationSettings$populationPlpSettings$verbosity)

    predictionResults <-
      PatientLevelPrediction::runPlp(
        population = population,
        plpData = plpData,
        modelSettings = runSettings$runPlpSettings$modelSettings,
        saveDirectory = file.path(analysisPath, "Prediction", id),
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
        analysisId = analysisSettings$analysisId)
  }

  ParallelLogger::logInfo("Estimated prediction models for all outcomes")

  #######################
  # Estimation step
  #######################
  ParallelLogger::logInfo("****Starting estimation step****")
  ParallelLogger::logInfo("Starting propensity score estimation for main outcomes")

  cluster <- ParallelLogger::makeCluster(runSettings$runCmSettings$createPsThreads)
  ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation", "CohortMethod"))


  dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                        x = predictOutcomes,
                                        fun = fitPsModel,
                                        analysisSettings = analysisSettings,
                                        getDataSettings = getDataSettings,
                                        populationSettings = populationSettings,
                                        runSettings = runSettings)

  ParallelLogger::stopCluster(cluster)

  ParallelLogger::logInfo("Starting propensity score estimation for secondary outcomes")

  for(predictOutcome in predictOutcomes){
    predLoc <- which(analysisSettings$outcomeIds == predictOutcome)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- compareOutcomes[compareOutcomes != predictOutcome]

    if(length(compareOutcomes) == 0)
      compareOutcomes <- NULL

    if(!is.null(compareOutcomes)){

      cluster <- ParallelLogger::makeCluster(runSettings$runCmSettings$createPsThreads)
      ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation", "CohortMethod"))


      dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                            x = compareOutcomes,
                                            fun = fitPsModelSwitch,
                                            predictOutcome = predictOutcome,
                                            analysisSettings = analysisSettings,
                                            getDataSettings = getDataSettings,
                                            populationSettings = populationSettings,
                                            runSettings = runSettings)

      ParallelLogger::stopCluster(cluster)
    }
  }



  ParallelLogger::logInfo("Done estimating propensity scores")

  ParallelLogger::logInfo("Starting estimation of results for main outcomes")

  cluster <- ParallelLogger::makeCluster(runSettings$runCmSettings$createPsThreads)
  ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation", "CohortMethod"))

  pathToPs <- file.path(analysisSettings$saveDirectory,
                            analysisSettings$analysisId,
                            "Estimation")
  dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                        x = predictOutcomes,
                                        fun = fitOutcomeModels,
                                        getDataSettings = getDataSettings,
                                        pathToPs = pathToPs,
                                        runSettings = runSettings)

  ParallelLogger::stopCluster(cluster)

  ParallelLogger::logInfo("Starting estimation of results for secondary outcomes")

  for(predictOutcome in predictOutcomes){
    predLoc <- which(analysisSettings$outcomeIds == predictOutcome)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- compareOutcomes[compareOutcomes != predictOutcome]

    if(length(compareOutcomes) == 0)
      compareOutcomes <- NULL

    if(!is.null(compareOutcomes)){

      cluster <- ParallelLogger::makeCluster(runSettings$runCmSettings$createPsThreads)
      ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation", "CohortMethod"))


      pathToPs <- file.path(analysisSettings$saveDirectory,
                            analysisSettings$analysisId,
                            "Estimation",
                            predictOutcome)
      dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                            x = compareOutcomes,
                                            fun = fitOutcomeModels,
                                            getDataSettings = getDataSettings,
                                            pathToPs = pathToPs,
                                            runSettings = runSettings)

      ParallelLogger::stopCluster(cluster)
    }
  }

  if(!is.null(analysisSettings$negativeControlOutcomes)){

    negativeControlIds <- analysisSettings$negativeControlOutcomes
    analysisSettingsForNegativeControls <- analysisSettings
    analysisSettingsForNegativeControls$analysisId <-
      file.path(analysisSettings$analysisId,
                "NegativeControls")

    ParallelLogger::logInfo("Starting estimation for negative control outcomes")


    for(predictOutcome in predictOutcomes){
      cluster <- ParallelLogger::makeCluster(runSettings$runCmSettings$createPsThreadsNegativeControls)
      ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation", "CohortMethod"))
      pathToPs <- file.path(analysisSettings$saveDirectory,
                            analysisSettings$analysisId,
                            "Estimation",
                            predictOutcome)
      ParallelLogger::logInfo("Fitting negative control propensity score models")
      dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                            x = negativeControlIds,
                                            fun = fitPsModelSwitch,
                                            predictOutcome = predictOutcome,
                                            analysisSettings = analysisSettings,
                                            getDataSettings = getDataSettings,
                                            populationSettings = populationSettings,
                                            runSettings = runSettings)

      ParallelLogger::logInfo("Fitting negative control outcome models")


      dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                            x = negativeControlIds,
                                            fun = fitOutcomeModels,
                                            getDataSettings = getDataSettings,
                                            pathToPs = pathToPs,
                                            runSettings = runSettings)

      ParallelLogger::stopCluster(cluster)
    }
  }


  createOverallResults(analysisSettings,
                       runSettings)
  saveRDS(analysisSettings,
          file.path(analysisSettings$saveDirectory,
                    analysisSettings$analysisId,
                    "analysisSettings.rds"))



  ParallelLogger::logInfo('Created reference table')
  ParallelLogger::logInfo('Run finished successfully')

  # stop logger
  ParallelLogger::clearLoggers()
  logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                         threshold = "INFO",
                                         appenders = list(ParallelLogger::createConsoleAppender(
                                           layout = ParallelLogger::layoutTimestamp)))
  ParallelLogger::registerLogger(logger)

  return(analysisSettings)
}
