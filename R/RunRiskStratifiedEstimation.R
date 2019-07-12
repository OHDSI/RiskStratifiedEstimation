#' Runs a risk stratified analysis
#'
#' Runs a risk stratified analysis in two stages. It first runs a prediction algorithm using \code{PatientLevelPrediction} to derive baseline
#' patient risks and then derives estimates within risk strata using \code{CohortMethod} package.
#'
#' @param plpDataFolder                      Folder where the \code{plpData} object is stored. If \code{NULL}, it will be constructed within the function.
#' @param cohortMethodDataFolder             Folder where the \code{cohortMethodData} object is stored. If \code{NULL}, it will be constructed within the function.
#' @param cdmDatabaseSchema                  The name of the database schema that contains the vocabulary files.
#'                                           Requires read permissions to this database. On SQL Server, this should specifiy both the database and the schema,
#'                                           so for example 'cdm_instance.dbo'.
#' @param cohortDatabaseSchema               The name of the database schema that contains the treatment and comaparator cohorts. Requires read permissions
#'                                           to this database.
#' @param outcomeDatabaseSchema              The name of the database schema that contains the outcome cohorts. Requires read permissions to this database.
#' @param resultsDatabaseSchema              The name of the database schema with write permissions.
#' @param cohortTable                        The name of the table holding the treatment and comparator cohorts.
#' @param outcomeTable                       The name of the table hodling the outcome cohorts.
#' @param mergedCohortTable                  The name of the table where the merged treatment and comparator cohorts will be stored.
#' @param attributeDefinitionTable           The table where the definition of the treatment covariate will be stored.
#' @param cohortAttributeTable               The table where the covariate values with regard to treatment will be stored.
#' @param treatmentCohortId                  The cohort definition id of the treatment cohort in the cohortTable.
#' @param comparatorCohortId                 The cohort definition id of the comparator cohort in the cohortTable.
#' @param outcomeIds                         A list of cohort definition ids used to define the outcomes in the outcome table.
#' @param targetCohortId                     The cohrt definition id of of the merged cohort in the mergedCohortTable.
#' @param connectionDetails                  An R object of type \code{connectionDetails} created using function
#'                                           \code{\link[DatabaseConnector]{createConnectionDetails}}.
#'                                           Either the \code{connection} or the \code{connectionDetails} argument should be specified.
#' @param cdmVersion                         Define the OMOP CDM version used: currently supported is "5".
#' @param getDbCohortMethodDataArgs          A parameter object for the function \code{\link[CohortMethod]{getDbCohortMethodData}}. Can be generated from function
#'                                           \code{\link[CohortMethod]{createGetDbCohortMethodDataArgs}}.
#' @param covariateSettingsCm                An object of type \code{covariateSettings} as created using the
#'                                           \code{\link[FeatureExtraction]{createCovariateSettings}} to be used for the definition of the
#'                                           \code{cohortMethodData} object.
#' @param exposureDatabaseSchema             Input of function \code{\link[CohortMethod]{getDbCohortMethodData}}: The name of the database schema that is the
#'                                           location where the exposure data used to define the exposure cohorts is available.
#' @param exposureTable                      Input of function \code{\link[CohortMethod]{getDbCohortMethodData}}: The tablename that contains the exposure cohorts.
#' @param psControl                          An object of the type \code{cyclopsControl} generated from \code{\link[Cyclops]{createControl}}.
#' @param psPrior                            An object of the type \code{cyclopsPrior} generated from \code{\link[Cyclops]{createPrior}}.
#' @param psMethod                           Select the propensity score method for the estimation of treatment effects within risk strata. It can be "matchOnPs",
#'                                           "stratifyByPs" or "inversePtWeighted".
#' @param createPsThreads                    The number of threads for the calculation of the propensity scores.
#' @param modelSettings                      An object of the class \code{modelSettings} to be used as input for \code{\link[PatientLevelPrediction]{runPlp}}.
#' @param getPlpDataArgs                     A parameter object for the function \code{\link[PatientLevelPrediction]{getPlpData}}. It can be generated from function
#'                                           \code{\link[RiskStratifiedEstimation]{createGetPlpDataArgs}}.
#' @param populationCmSettings               A parameter object for the function \code{\link[CohortMethod]{createStudyPopulation}}. Can be generated from
#'                                           function \code{createStudyPopulationCmSettings}.
#' @param covariateSettingsPlp               An object of type \code{covariateSettings} as created using the
#'                                           \code{\link[FeatureExtraction]{createCovariateSettings}} to be used for the definition of the
#'                                           \code{plpData} object. note that a covariate indicating treatment will be added.
#' @param populationPlpSettings              A parameter object for the function \code{\link[PatientLevelPrediction]{createStudyPopulation}}. Can be generated from
#'                                           function \code{\link[PatientLevelPrediction]{createStudyPopulationSettings}}.
#' @param runPlpArgs                         A parameter object for the function \code{\link[PatientLevelPrediction]{runPlp}}. Can be generated from function
#'                                           \code{\link[RiskStratifiedEstimation]{createRunPlpArgs}}.
#' @param riskStrata                         The number of risk strata to divide the study population.
#' @param weightsType                        Only required if \code{weightsType} is "inversePtWeighted". The type of weights for the balancing of covariates.
#'                                           Should be either 'ATE' or 'ATT'
#' @param useStabilizedWeights               Only required if \code{weightsType} is "inversePtWeighted". Should stabilized weights be used?
#' @param truncationLevels                   Only required if \code{weightsType} is "inversePtWeighted". The level of truncation expressed in percentiles of the propensity score.
#' @param timePoint                          The time point of interest for the calculation of the absolute risk reduction.
#' @param compareAllOutcomes                 Should all the outcomes be analyzed within all stratifications?
#' @param predictionThreads                  The number of threads to be used to run the predictions.
#' @param saveResults                        Should the results of the entire analysis be saved?
#' @param saveDirectory                      The file path to the directory where the results of the analysis will be saved.
#' @param fftempdir                          The directory where the temporary \code{ff} files will be saved.
#' @param fitOutcomeModelsThreads            The number of threadss to be used for the calculation of the risk stratified results.
#' @param saveMapMatrix                      Should the map matrix with the risk sratum allocations be saved?
#' @param savePs                             Should the propensity scores be saved?
#' @param verbosity                          Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold,
#'                                           a message will print. The levels are:
#'                                           \itemize{
#'                                               \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                               \item{TRACE}{Showing information about start and end of steps}
#'                                               \item{INFO}{Show informative information (Default)}
#'                                               \item{WARN}{Show warning messages}
#'                                               \item{ERROR}{Show error messages}
#'                                               \item{FATAL}{Be silent except for fatal errors}}.
#' @param analysisId                         The identifier of the analysis.
#'
#' @return                                   An object containing two large lists:
#'                                           \itemize{
#'                                               \item The propensity scores within risk strata for each outcome in \code{outcomeIds}
#'                                               \item The results of the risk stratified analyis within risk strata for all outcomes in \code{outcomeIds}:
#'                                                   \itemize{
#'                                                        \item Weighted Kaplan-Meier estimates.
#'                                                        \item Relative risk reduction.
#'                                                        \item Absolute risk reduction.
#'                                                        \item Number of cases.
#'                                                   }
#'                                            }
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @export

runRiskStratifiedEstimation <-
  function(plpDataFolder = NULL, cohortMethodDataFolder = NULL, cdmDatabaseSchema, cohortDatabaseSchema, outcomeDatabaseSchema,
           resultsDatabaseSchema, cohortTable, outcomeTable, mergedCohortTable, attributeDefinitionTable, cohortAttributeTable,
           treatmentCohortId, comparatorCohortId, outcomeIds, targetCohortId, connectionDetails, getDbCohortMethodDataArgs,
           covariateSettingsCm, populationCmSettings, exposureTable, psControl = NULL, psPrior = NULL, psMethod = "matchOnPs",
           createPsThreads = 1, exposureDatabaseSchema, getPlpDataArgs, covariateSettingsPlp, modelSettings, populationPlpSettings,
           cdmVersion = "5", runPlpArgs, riskStrata = 4, weightsType = "ATE", useStabilizedWeights = TRUE,
           truncationLevels = c(.01, .99), timePoint, compareAllOutcomes = TRUE, predictionThreads = 1, saveResults, saveDirectory = NULL,
           fftempdir, fitOutcomeModelsThreads = 1, saveMapMatrix = TRUE, savePs = TRUE, verbosity = "INFO", analysisId = NULL){


    if(missing(verbosity)){
      verbosity <- "INFO"
    } else{
      if(!verbosity%in%c("DEBUG","TRACE","INFO","WARN","FATAL","ERROR")){
        stop('Incorrect verbosity string')
      }
    }

    # log the start time:
    ExecutionDateTime <- Sys.time()

    # create an analysisid and folder to save the results
    start.all <- Sys.time()
    if(is.null(analysisId))
      analysisId <- paste(gsub(':','',gsub('-','',gsub(' ','',start.all))))

    if(is.null(saveDirectory)) saveDirectory <- file.path(getwd(),'RSEE') # if NULL save to wd


    analysisPath <- file.path(saveDirectory, analysisId)
    if(!dir.exists(analysisPath)){dir.create(analysisPath, recursive=T)}
    logFileName = paste0(analysisPath,'/logRSEE.txt')

    logger <- ParallelLogger::createLogger(name = "RSEE Main Log",
                                           threshold = verbosity,
                                           appenders = list(ParallelLogger::createFileAppender(layout = ParallelLogger::layoutParallel,
                                                                                               fileName = logFileName)))
    ParallelLogger::registerLogger(logger)
    logSep <- paste(rep('*', 96), collapse = '')

    ParallelLogger::logInfo(paste0('Risk Stratified Effect Estimation Package version ', utils::packageVersion("RiskStratifiedEstimation")))
    ParallelLogger::logInfo(logSep)

    ParallelLogger::logInfo(sprintf('%-20s%s', 'AnalysisID: ',analysisId))
    ParallelLogger::logInfo(sprintf('%-20s%s', 'treatmentId: ', treatmentCohortId))
    ParallelLogger::logInfo(sprintf('%-20s%s', 'comparatorId', comparatorCohortId))
    ParallelLogger::logInfo(sprintf('%-20s%s', 'OutcomeID: ', outcomeIds))

    #######################
    # Prediction step
    #######################
    ParallelLogger::logInfo(logSep)
    ParallelLogger::logInfo("****Starting prediction step****")
    ParallelLogger::logInfo("Merging the treatment and comparator cohorts")

    prepareForPlpData(treatmentCohortId = treatmentCohortId,
                      comparatorCohortId = comparatorCohortId,
                      targetCohortId = targetCohortId,
                      cohortDatabaseSchema = cohortDatabaseSchema,
                      cohortTable = cohortTable,
                      resultsDatabaseSchema = resultsDatabaseSchema,
                      mergedCohortTable = mergedCohortTable,
                      attributeDefinitionTable = attributeDefinitionTable,
                      cohortAttributeTable = cohortAttributeTable,
                      connectionDetails = connectionDetails)
    ParallelLogger::logInfo("Done")
    ParallelLogger::logInfo("Creating covariate settings")
    covariateSettingsTreatment <-
      FeatureExtraction::createCohortAttrCovariateSettings(attrDatabaseSchema = resultsDatabaseSchema,
                                                           attrDefinitionTable = attributeDefinitionTable,
                                                           cohortAttrTable = cohortAttributeTable)
    covariateSettingsList <- list(covariateSettingsPlp,
                                  covariateSettingsTreatment)

    ParallelLogger::logInfo("Constructing the plpData object")

    if(is.null(plpDataFolder)){
      plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                                    cdmDatabaseSchema = cdmDatabaseSchema,
                                                    cohortId = targetCohortId,
                                                    outcomeIds = outcomeIds,
                                                    cohortDatabaseSchema = resultsDatabaseSchema,
                                                    cohortTable = mergedCohortTable,
                                                    outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                    outcomeTable = outcomeTable,
                                                    studyStartDate = getPlpDataArgs$studyStartDate,
                                                    studyEndDate = getPlpDataArgs$studyEndDate,
                                                    cdmVersion = cdmVersion,
                                                    firstExposureOnly = getPlpDataArgs$firstExposureOnly,
                                                    washoutPeriod = getPlpDataArgs$washoutPeriod,
                                                    excludeDrugsFromCovariates = getPlpDataArgs$excludeDrugsFromCovariates,
                                                    covariateSettings = covariateSettingsList)

      PatientLevelPrediction::savePlpData(plpData, file = file.path(saveDirectory, "Prediction", "plpData"))
      plpDataFolder <- file.path(saveDirectory, "Prediction", "plpData")
    }
    else{
      plpData <- PatientLevelPrediction::loadPlpData(plpDataFolder)
    }




    runPrediction <- function(x,
                              populationPlpSettings,
                              modelSettings,
                              plpDataFolder,
                              outcomeIds,
                              testSplit,
                              testFraction,
                              nfold,
                              analysisId,
                              saveDirectory){

      ParallelLogger::registerLogger(logger)


      plpData <- PatientLevelPrediction::loadPlpData(file = plpDataFolder)

      populationPlp <-
        PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                      outcomeId = outcomeIds[x],
                                                      binary = populationPlpSettings$binary,
                                                      includeAllOutcomes = populationPlpSettings$includeAllOutcomes,
                                                      firstExposureOnly = populationPlpSettings$firstExposureOnly,
                                                      washoutPeriod = populationPlpSettings$washoutPeriod,
                                                      removeSubjectsWithPriorOutcome = populationPlpSettings$removeSubjectsWithPriorOutcome,
                                                      priorOutcomeLookback = populationPlpSettings$priorOutcomeLookback,
                                                      requireTimeAtRisk = populationPlpSettings$requireTimeAtRisk,
                                                      minTimeAtRisk = populationPlpSettings$minTimeAtRisk,
                                                      riskWindowStart = populationPlpSettings$riskWindowStart,
                                                      addExposureDaysToStart = populationPlpSettings$addExposureDaysToStart,
                                                      riskWindowEnd = populationPlpSettings$riskWindowEnd,
                                                      addExposureDaysToEnd = populationPlpSettings$addExposureDaysToEnd,
                                                      verbosity = populationPlpSettings$verbosity)


      predictionResult <-
        PatientLevelPrediction::runPlp(population = populationPlp,
                                       plpData = plpData,
                                       modelSettings = modelSettings,
                                       saveDirectory = file.path(saveDirectory, "Prediction", outcomeIds[x]),
                                       minCovariateFraction = runPlpArgs$minCovariateFraction,
                                       normalizeData = runPlpArgs$normalizeData ,
                                       testSplit = runPlpArgs$testSplit ,
                                       testFraction = runPlpArgs$testFraction ,
                                       trainFraction = runPlpArgs$trainFraction ,
                                       nfold = runPlpArgs$nfold ,
                                       indexes = runPlpArgs$indexes ,
                                       savePlpData = FALSE ,
                                       savePlpResult = TRUE ,
                                       savePlpPlots = runPlpArgs$savePlpPlots ,
                                       saveEvaluation = runPlpArgs$saveEvaluation ,
                                       verbosity = runPlpArgs$verbosity ,
                                       timeStamp = runPlpArgs$timeStamp ,
                                       analysisId = analysisId)


      return(NULL)

    }

    predictionList <- list()
    lengthOutcomes <- length(outcomeIds)
    cl <- ParallelLogger::makeCluster(numberOfThreads = predictionThreads)
    predictionList <-  ParallelLogger::clusterApply(cluster = cl,
                                                    fun = runPrediction,
                                                    x = 1:lengthOutcomes,
                                                    plpDataFolder = plpDataFolder,
                                                    populationPlpSettings = populationPlpSettings,
                                                    modelSettings = modelSettings,
                                                    testSplit = testSplit,
                                                    outcomeIds = outcomeIds,
                                                    testFraction = testFraction,
                                                    nfold = nfold,
                                                    analysisId = analysisId,
                                                    saveDirectory = saveDirectory)
    ParallelLogger::stopCluster(cl)

    predictionOutcomes <- numeric()

    ParallelLogger::registerLogger(logger)
    ParallelLogger::logInfo("Estimated prediction models for all outcomes")


    #######################
    # Estimation step
    #######################
    ParallelLogger::logInfo("****Starting estimation step****")
    ParallelLogger::logInfo("Constructing plpData object excluding treatment")
    plpData <- removeTreatment(plpData = plpData,
                               treatmentCovariateId = 1)

    ParallelLogger::logInfo("Done")

    if(is.null(cohortMethodDataFolder)){

      cohortMethodData <-
        CohortMethod::getDbCohortMethodData(connectionDetails = connectionDetails,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            targetId = treatmentCohortId,
                                            comparatorId = comparatorCohortId,
                                            outcomeIds = outcomeIds,
                                            studyStartDate = getDbCohortMethodDataArgs$studyStartDate,
                                            studyEndDate = getDbCohortMethodDataArgs$studyEndDate,
                                            exposureDatabaseSchema = exposureDatabaseSchema,
                                            exposureTable = exposureTable,
                                            outcomeDatabaseSchema = outcomeDatabaseSchema,
                                            outcomeTable = outcomeTable,
                                            cdmVersion = cdmVersion,
                                            excludeDrugsFromCovariates = getDbCohortMethodDataArgs$excludeDrugsFromCovariates,
                                            firstExposureOnly = getDbCohortMethodDataArgs$firstExposureOnly,
                                            removeDuplicateSubjects = getDbCohortMethodDataArgs$removeDuplicateSubjects,
                                            restrictToCommonPeriod = getDbCohortMethodDataArgs$restrictToCommonPeriod,
                                            washoutPeriod = getDbCohortMethodDataArgs$washoutPeriod,
                                            maxCohortSize = getDbCohortMethodDataArgs$maxCohortSize,
                                            covariateSettings = getDbCohortMethodDataArgs$covariateSettings)

      CohortMethod::saveCohortMethodData(cohortMethodData, file.path(analysisPath, "cmData"))
      cohortMethodDataFolder <- file.path(analysisPath, "cmData")
    }
    else{
      cohortMethodData <- CohortMethod::loadCohortMethodData(cohortMethodDataFolder)
    }

    numberOfOutcomes <- length(outcomeIds)

    ParallelLogger::logInfo("Starting propensity score estimation")
    psOverAllOutcomes <- list()


    cluster <- ParallelLogger::makeCluster(createPsThreads)
    ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation", "CohortMethod"))


    dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                          x = outcomeIds,
                                          fun = fitPsModel,
                                          cohortMethodDataFolder = cohortMethodDataFolder,
                                          plpDataFolder = plpDataFolder,
                                          getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
                                          populationCmSettings = populationCmSettings,
                                          populationPlpSettings = populationPlpSettings,
                                          saveDirectory = saveDirectory,
                                          riskStrata = riskStrata,
                                          analysisPath = analysisPath,
                                          analysisId = analysisId)
    ParallelLogger::stopCluster(cluster)

    ParallelLogger::logInfo("Done estimating propensity scores")
    ParallelLogger::logInfo("Starting calculation of results")

    cluster <- ParallelLogger::makeCluster(fitOutcomeModelsThreads)
    ParallelLogger::clusterRequire(cluster, "RiskStratifiedEstimation")
    dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                          x = outcomeIds,
                                          fun = fitOutcomeModels,
                                          analysisPath = analysisPath,
                                          outcomeIdList = outcomeIds,
                                          cohortMethodDataFolder = cohortMethodDataFolder,
                                          timePoint = timePoint,
                                          psMethod = psMethod,
                                          weightsType = weightsType,
                                          useStabilizedWeights = useStabilizedWeights,
                                          truncationLevels = truncationLevels,
                                          riskStrata = riskStrata,
                                          compareAllOutcomes = compareAllOutcomes,
                                          populationCmSettings = populationCmSettings)
    ParallelLogger::stopCluster(cluster)

    referenceTable <- list(analaysisId = analysisId,
                           targetId = targetCohortId,
                           comparatorId = comparatorCohortId,
                           outcomeIds = outcomeIds,
                           outputFolder = analysisPath)


    ParallelLogger::logInfo('Created reference table')
    ParallelLogger::logInfo('Run finished successfully')

    # stop logger
    ParallelLogger::clearLoggers()
    logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                           threshold = "INFO",
                                           appenders = list(ParallelLogger::createConsoleAppender(layout = ParallelLogger::layoutTimestamp)))
    ParallelLogger::registerLogger(logger)

    return(referenceTable)
  }
