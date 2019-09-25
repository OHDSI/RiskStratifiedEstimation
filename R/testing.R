#' Runs a risk stratified analysis
#'
#' Runs a risk stratified analysis in two stages. It first runs a prediction algorithm using
#' \code{PatientLevelPrediction} to derive baseline patient risks and then derives estimates within risk strata using
#' \code{CohortMethod} package.
#'
#' @param plpDataFolder                      Folder where the \code{plpData} object is stored. If \code{NULL}, it will
#'                                           be constructed within the function.
#' @param cohortMethodDataFolder             Folder where the \code{cohortMethodData} object is stored. If \code{NULL},
#'                                           it will be constructed within the function.
#' @param cdmDatabaseSchema                  The name of the database schema that contains the vocabulary files.
#'                                           Requires read permissions to this database. On SQL Server, this should
#'                                           specifiy both the database and the schema,
#'                                           so for example 'cdm_instance.dbo'.
#' @param cohortDatabaseSchema               The name of the database schema that contains the treatment and comaparator
#'                                           cohorts. Requires read permissions
#'                                           to this database.
#' @param outcomeDatabaseSchema              The name of the database schema that contains the outcome cohorts. Requires
#'                                           read permissions to this database.
#' @param resultsDatabaseSchema              The name of the database schema with write permissions.
#' @param cohortTable                        The name of the table holding the treatment and comparator cohorts.
#' @param outcomeTable                       The name of the table hodling the outcome cohorts.
#' @param mergedCohortTable                  The name of the table where the merged treatment and comparator cohorts
#'                                           will be stored.
#' @param attributeDefinitionTable           The table where the definition of the treatment covariate will be stored.
#' @param cohortAttributeTable               The table where the covariate values with regard to treatment will be
#'                                           stored.
#' @param treatmentCohortId                  The cohort definition id of the treatment cohort in the cohortTable.
#' @param comparatorCohortId                 The cohort definition id of the comparator cohort in the cohortTable.
#' @param targetCohortId                     The cohort definition id of of the merged cohort in the mergedCohortTable.
#' @param predictOutcomes                    The outcomes for which the risk stratification will be performed.
#' @param compareOutcomes                    The  outcomes for which risk stratified estimates need to be derived. If
#'                                           set to \code{NULL}, all \code{predictOutcomes} will be considered.
#' @param connectionDetails                  An R object of type \code{connectionDetails} created using function
#'                                           \code{\link[DatabaseConnector]{createConnectionDetails}}.
#'                                           Either the \code{connection} or the \code{connectionDetails} argument
#'                                           should be specified.
#' @param cdmVersion                         Define the OMOP CDM version used: currently supported is "5".
#' @param getDbCohortMethodDataArgs          A parameter object for the function
#'                                           \code{\link[CohortMethod]{getDbCohortMethodData}}. Can be generated from
#'                                           function
#'                                           \code{\link[CohortMethod]{createGetDbCohortMethodDataArgs}}.
#' @param covariateSettingsCm                An object of type \code{covariateSettings} as created using the
#'                                           \code{\link[FeatureExtraction]{createCovariateSettings}} to be used for the
#'                                            definition of the \code{cohortMethodData} object.
#' @param exposureDatabaseSchema             Input of function \code{\link[CohortMethod]{getDbCohortMethodData}}: The
#'                                           name of the database schema that is the
#'                                           location where the exposure data used to define the exposure cohorts is
#'                                           available.
#' @param exposureTable                      Input of function \code{\link[CohortMethod]{getDbCohortMethodData}}: The
#'                                           tablename that contains the exposure cohorts.
#' @param psControl                          An object of the type \code{cyclopsControl} generated from
#'                                           \code{\link[Cyclops]{createControl}}.
#' @param psPrior                            An object of the type \code{cyclopsPrior} generated from
#'                                           \code{\link[Cyclops]{createPrior}}.
#' @param psMethod                           Select the propensity score method for the estimation of treatment effects
#'                                           within risk strata. It can be "matchOnPs", "stratifyByPs" or
#'                                           "inversePtWeighted".
#' @param createPsThreads                    The number of threads for the calculation of the propensity scores.
#' @param modelSettings                      An object of the class \code{modelSettings} to be used as input for
#'                                           \code{\link[PatientLevelPrediction]{runPlp}}.
#' @param getPlpDataArgs                     A parameter object for the function
#'                                           \code{\link[PatientLevelPrediction]{getPlpData}}. It can be generated from
#'                                            function
#'                                           \code{\link[RiskStratifiedEstimation]{createGetPlpDataArgs}}.
#' @param populationCmSettings               A parameter object for the function
#'                                           \code{\link[CohortMethod]{createStudyPopulation}}. Can be generated from
#'                                           function \code{createStudyPopulationCmSettings}.
#' @param covariateSettingsPlp               An object of type \code{covariateSettings} as created using the
#'                                           \code{\link[FeatureExtraction]{createCovariateSettings}} to be used for
#'                                            the definition of the
#'                                           \code{plpData} object. note that a covariate indicating treatment will be
#'                                           added.
#' @param populationPlpSettings              A parameter object for the function
#'                                           \code{\link[PatientLevelPrediction]{createStudyPopulation}}. Can be
#'                                           generated from function
#'                                           \code{\link[PatientLevelPrediction]{createStudyPopulationSettings}}.
#' @param runPlpArgs                         A parameter object for the function
#'                                           \code{\link[PatientLevelPrediction]{runPlp}}. Can be generated from
#'                                           function
#'                                           \code{\link[RiskStratifiedEstimation]{createRunPlpArgs}}.
#' @param riskStrata                         The number of risk strata to divide the study population.
#' @param weightsType                        Only required if \code{weightsType} is "inversePtWeighted". The type of
#'                                           weights for the balancing of covariates.
#'                                           Should be either 'ATE' or 'ATT'
#' @param useStabilizedWeights               Only required if \code{weightsType} is "inversePtWeighted". Should
#'                                           stabilized weights be used?
#' @param truncationLevels                   Only required if \code{weightsType} is "inversePtWeighted". The level of
#'                                           truncation expressed in percentiles of the propensity score.
#' @param timePoint                          The time point of interest for the calculation of the absolute risk
#'                                           reduction.
#' @param predictionThreads                  The number of threads to be used to run the predictions.
#' @param saveResults                        Should the results of the entire analysis be saved?
#' @param saveDirectory                      The file path to the directory where the results of the analysis will be
#'                                           saved.
#' @param fftempdir                          The directory where the temporary \code{ff} files will be saved.
#' @param fitOutcomeModelsThreads            The number of threadss to be used for the calculation of the risk
#'                                           stratified results.
#' @param saveMapMatrix                      Should the map matrix with the risk sratum allocations be saved?
#' @param savePs                             Should the propensity scores be saved?
#' @param verbosity                          Sets the level of the verbosity. If the log level is at or higher in
#'                                           priority than the logger threshold,
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
#' @return                                   A reference list for the analaysis results:
#'                                           \itemize{
#'                                               \item analaysisId
#'                                               \item targetId
#'                                               \item comparatorId
#'                                               \item compareOutcomes
#'                                               \item predictOutcomes
#'                                               \item outputFolder
#'
#'                                            }
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @export

runRiskStratifiedEstimation1 <- function(connectionDetails,
                                         analysisSettings,
                                         databaseSettings,
                                         getDataSettings,
                                         covariateSettings,
                                         populationSettings,
                                         runSettings){


  if(missing(analysisSettings$verbosity)){
    analysisSettings$verbosity <- "INFO"
  } else{
    if(!analysisSettings$verbosity%in%c("DEBUG","TRACE","INFO","WARN","FATAL","ERROR")){
      stop('Incorrect verbosity string')
    }
  }

  # log the start time:
  ExecutionDateTime <- Sys.time()

  # create an analysisid and folder to save the results
  start.all <- Sys.time()
  if(is.null(analysisSettings$analysisId))
    analysisSettings$analysisId <- paste(gsub(':','',gsub('-','',gsub(' ','',start.all))))

  if(is.null(analysisSettings$saveDirectory))
    analysisSettings$saveDirectory <- file.path(getwd(),'RSEE') # if NULL save to wd


  analysisPath <- file.path(analysisSettings$saveDirectory, analysisSettings$analysisId)
  if(!dir.exists(analysisPath)){dir.create(analysisPath, recursive=T)}
  logFileName = paste0(analysisPath,'/logRSEE.txt')

  logger <- ParallelLogger::createLogger(name = "RSEE Main Log",
                                         threshold = verbosity,
                                         appenders =
                                           list(ParallelLogger::createFileAppender(
                                             layout = ParallelLogger::layoutParallel, fileName = logFileName)))
  ParallelLogger::registerLogger(logger)
  logSep <- paste(rep('*', 96), collapse = '')

  predictOutcomes <- unique(
    analysisSettings$outcomeIds[col(analysisSettings$analysisMatrix)[which(!analysisSettings$analysisMatrix == 0)]])
  compareOutcomes <- list()
  for(i in 1:length(predictOutcomes)){
    colNumber <- which(predictOutcomes == predictOutcomes[i])
    compareOutcomes[[i]] <- analysisSettings$outcomeIds[as.logical(analysisSettings$analysisMatrix[, colNumber])]
  }0

  #######################
  # Overall results step
  #######################
  if(is.null(getDataSettings$cohortMethodDataFolder)){

    cohortMethodData <-
      CohortMethod::getDbCohortMethodData(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = databaseSettings$cdmDatabaseSchema,
                                          targetId = analysisSettings$treatmentCohortId,
                                          comparatorId = analysisSettings$comparatorCohortId,
                                          outcomeIds = analysisSettings$outcomeIds,
                                          studyStartDate = getDataSettings$getCmDataArgs$studyStartDate,
                                          studyEndDate = getDataSettings$getCmDataArgs$studyEndDate,
                                          exposureDatabaseSchema = databaseSettings$exposureDatabaseSchema,
                                          exposureTable = databaseSettings$exposureTable,
                                          outcomeDatabaseSchema = databaseSettings$outcomeDatabaseSchema,
                                          outcomeTable = databaseSettings$outcomeTable,
                                          cdmVersion = databaseSettings$cdmVersion,
                                          excludeDrugsFromCovariates =
                                            getDataSettings$getCmDataArgs$excludeDrugsFromCovariates,
                                          firstExposureOnly = getDataSettings$getCmDataArgs$firstExposureOnly,
                                          removeDuplicateSubjects =
                                            getDataSettings$getCmDataArgs$removeDuplicateSubjects,
                                          restrictToCommonPeriod = getDataSettings$getCmDataArgs$restrictToCommonPeriod,
                                          washoutPeriod = getDataSettings$getCmDataArgs$washoutPeriod,
                                          maxCohortSize = getDataSettings$getCmDataArgs$maxCohortSize,
                                          covariateSettings = covariateSettings$covariateSettingsCm)

    CohortMethod::saveCohortMethodData(cohortMethodData, file.path(analysisPath, "Data", "cmData"))
    getDataSettings$cohortMethodDataFolder <- file.path(analysisPath, "Data", "cmData")
  }
  else{
    cohortMethodData <- CohortMethod::loadCohortMethodData(getDataSettings$cohortMethodDataFolder)
  }

  if(runSettings$runCmSettings$estimateOverallresults){

    cluster <- ParallelLogger::makeCluster(runSettings$runCmSettings$createPsThreads)
    ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation", "CohortMethod"))
    overallOutcomes <- unique(c(predictOutcomes, unlist(compareOutcomes)))

    dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                          x = overallOutcomes,
                                          fun = fitPsModelOverall,
                                          getDataSettings = getDataSettings,
                                          populationCmSettings = populationSettings$populationCmSettings,
                                          analysisSettings = analysisSettings,
                                          runCmSettings = runSettings$runCmSettings)

    ParallelLogger::stopCluster(cluster)

    ParallelLogger::logInfo("Done estimating propensity scores")
    ParallelLogger::logInfo("Starting calculation of results")

    cluster <- ParallelLogger::makeCluster(runSettings$runCmSettings$fitOutcomeModelsThreads)
    ParallelLogger::clusterRequire(cluster, "RiskStratifiedEstimation")

    # !!!! UpdatefitOutcomeModelsOverall funtion !!!!
    dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                          x = overallOutcomes,
                                          fun = fitOutcomeModelsOverall,
                                          analysisSettings = analysisSettings,
                                          getDataSettings = getDataSettings,
                                          runCmSettings = runSettings$runCmSettings)
    ParallelLogger::stopCluster(cluster)


  }


  #######################
  # Prediction step
  #######################
  ParallelLogger::logInfo(logSep)
  ParallelLogger::logInfo("****Starting prediction step****")
  ParallelLogger::logInfo("Merging the treatment and comparator cohorts")

  prepareForPlpData(treatmentCohortId = analysisSettings$treatmentCohortId,
                    comparatorCohortId = analysisSettings$comparatorCohortId,
                    targetCohortId = databaseSettings$targetCohortId,
                    cohortDatabaseSchema = databaseSettings$databaseSetitincohortDatabaseSchema,
                    cohortTable = databaseSettings$cohortTable,
                    resultsDatabaseSchema = databaseSettings$resultsDatabaseSchema,
                    mergedCohortTable = databaseSettings$mergedCohortTable,
                    attributeDefinitionTable = databaseSettings$attributeDefinitionTable,
                    cohortAttributeTable = databaseSettings$cohortAttributeTable,
                    connectionDetails = connectionDetails)
  ParallelLogger::logInfo("Done")
  ParallelLogger::logInfo("Creating covariate settings")
  covariateSettingsTreatment <-
    FeatureExtraction::createCohortAttrCovariateSettings(attrDatabaseSchema = resultsDatabaseSchema,
                                                         attrDefinitionTable = attributeDefinitionTable,
                                                         cohortAttrTable = cohortAttributeTable)
  covariateSettingsList <- list(covariateSettings$covariateSettingsPlp,
                                covariateSettingsTreatment)

  ParallelLogger::logInfo("Constructing the plpData object")

  if(is.null(getDataSettings$plpDataFolder)){
    plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                                  cdmDatabaseSchema = databaseSettings$cdmDatabaseSchema,
                                                  cohortId = databaseSettings$targetCohortId,
                                                  outcomeIds = analysisSettings$outcomeIds,
                                                  cohortDatabaseSchema = databaseSettings$resultsDatabaseSchema,
                                                  cohortTable = databaseSettings$mergedCohortTable,
                                                  outcomeDatabaseSchema = databaseSettings$outcomeDatabaseSchema,
                                                  outcomeTable = databaseSettings$outcomeTable,
                                                  studyStartDate = getDataSettings$getPlpDataSettings$studyStartDate,
                                                  studyEndDate = getDataSettings$getPlpDataSettings$studyEndDate,
                                                  cdmVersion = databaseSettings$cdmVersion,
                                                  firstExposureOnly =
                                                    getDataSettings$getPlpDataSettings$firstExposureOnly,
                                                  washoutPeriod = getDataSettings$getPlpDataSettings$washoutPeriod,
                                                  excludeDrugsFromCovariates =
                                                    getDataSettings$getPlpDataSettings$excludeDrugsFromCovariates,
                                                  covariateSettings = covariateSettingsList)

    PatientLevelPrediction::savePlpData(plpData, file = file.path(analysisPath, "Data", "plpData"))
    plpDataFolder <- file.path(analysisPath, "Data", "plpData")
  }
  else{
    plpData <- PatientLevelPrediction::loadPlpData(plpDataFolder)
  }

  runPrediction <- function(x,
                            populationPlpSettings,
                            modelSettings,
                            plpDataFolder,
                            predictOutcomes,
                            testSplit,
                            testFraction,
                            nfold,
                            analysisId,
                            analysisPath){

    ParallelLogger::registerLogger(logger)


    plpData <- PatientLevelPrediction::loadPlpData(file = plpDataFolder)

    populationPlp <-
      PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                    outcomeId = predictOutcomes[x],
                                                    binary = populationPlpSettings$binary,
                                                    includeAllOutcomes = populationPlpSettings$includeAllOutcomes,
                                                    firstExposureOnly = populationPlpSettings$firstExposureOnly,
                                                    washoutPeriod = populationPlpSettings$washoutPeriod,
                                                    removeSubjectsWithPriorOutcome =
                                                      populationPlpSettings$removeSubjectsWithPriorOutcome,
                                                    priorOutcomeLookback = populationPlpSettings$priorOutcomeLookback,
                                                    requireTimeAtRisk = populationPlpSettings$requireTimeAtRisk,
                                                    minTimeAtRisk = populationPlpSettings$minTimeAtRisk,
                                                    riskWindowStart = populationPlpSettings$riskWindowStart,
                                                    addExposureDaysToStart =
                                                      populationPlpSettings$addExposureDaysToStart,
                                                    riskWindowEnd = populationPlpSettings$riskWindowEnd,
                                                    addExposureDaysToEnd = populationPlpSettings$addExposureDaysToEnd,
                                                    verbosity = populationPlpSettings$verbosity)


    predictionResult <-
      PatientLevelPrediction::runPlp(population = populationPlp,
                                     plpData = plpData,
                                     modelSettings = modelSettings,
                                     saveDirectory = file.path(analysisPath, "Prediction", predictOutcomes[x]),
                                     minCovariateFraction = runPlpArgs$minCovariateFraction,
                                     normalizeData = runPlpArgs$normalizeData ,
                                     testSplit = runPlpArgs$testSplit ,
                                     testFraction = runPlpArgs$testFraction ,
                                     trainFraction = runPlpArgs$trainFraction ,
                                     nfold = runPlpArgs$nfold ,
                                     indexes = runPlpArgs$indexes ,
                                     savePlpData = FALSE, # Maybe change that???
                                     savePlpResult = TRUE ,
                                     savePlpPlots = runPlpArgs$savePlpPlots ,
                                     saveEvaluation = runPlpArgs$saveEvaluation ,
                                     verbosity = runPlpArgs$verbosity ,
                                     timeStamp = runPlpArgs$timeStamp ,
                                     analysisId = analysisId)


    return(NULL)

  }

  predictionList <- list()
  lengthOutcomes <- length(predictOutcomes)
  cl <- ParallelLogger::makeCluster(numberOfThreads = predictionThreads)
  predictionList <-  ParallelLogger::clusterApply(cluster = cl,
                                                  fun = runPrediction,
                                                  x = 1:lengthOutcomes,
                                                  plpDataFolder = plpDataFolder,
                                                  populationPlpSettings = populationPlpSettings,
                                                  modelSettings = modelSettings,
                                                  testSplit = testSplit,
                                                  predictOutcomes = predictOutcomes,
                                                  testFraction = testFraction,
                                                  nfold = nfold,
                                                  analysisId = analysisId,
                                                  analysisPath = analysisPath)
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

  ParallelLogger::logInfo("Starting propensity score estimation")

  cluster <- ParallelLogger::makeCluster(createPsThreads)
  ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation",
                                            "CohortMethod"))


  dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                        x = predictOutcomes,
                                        fun = fitPsModel,
                                        cohortMethodDataFolder = cohortMethodDataFolder,
                                        plpDataFolder = plpDataFolder,
                                        populationCmSettings = populationCmSettings,
                                        populationPlpSettings = populationPlpSettings,
                                        riskStrata = riskStrata,
                                        analysisPath = analysisPath,
                                        analysisId = analysisId,
                                        psControl = psControl,
                                        psPrior = psPrior)

  ParallelLogger::stopCluster(cluster)

  ParallelLogger::logInfo("Done estimating propensity scores")
  ParallelLogger::logInfo("Starting calculation of results")

  cluster <- ParallelLogger::makeCluster(fitOutcomeModelsThreads)
  ParallelLogger::clusterRequire(cluster, "RiskStratifiedEstimation")
  dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                        x = predictOutcomes,
                                        fun = fitOutcomeModels1,
                                        analysisPath = analysisPath,
                                        analysisRef = analysisRef,
                                        cohortMethodDataFolder = cohortMethodDataFolder,
                                        timePoint = timePoint,
                                        psMethod = psMethod,
                                        weightsType = weightsType,
                                        useStabilizedWeights = useStabilizedWeights,
                                        truncationLevels = truncationLevels,
                                        populationCmSettings = populationCmSettings)
  ParallelLogger::stopCluster(cluster)

  referenceTable <- list(analaysisId = analysisId,
                         targetId = targetCohortId,
                         comparatorId = comparatorCohortId,
                         compareOutcomes = compareOutcomes,
                         predictOutcomes = predictOutcomes,
                         outputFolder = analysisPath)


  ParallelLogger::logInfo('Created reference table')
  ParallelLogger::logInfo('Run finished successfully')

  # stop logger
  ParallelLogger::clearLoggers()
  logger <- ParallelLogger::createLogger(name = "SIMPLE",
                                         threshold = "INFO",
                                         appenders = list(
                                           ParallelLogger::createConsoleAppender(
                                             layout = ParallelLogger::layoutTimestamp)))
  ParallelLogger::registerLogger(logger)

  return(referenceTable)



}




#' Fit outcome models
#'
#' Fits outcome models within risk strata, estimating relative and absolute differences. Designed to be performed within
#' a parellelized analysis.
#'
#' @param outcomeId               The outcome of interest for which the esitmation is performed. That is the outcome for which risk stratification is performed.
#' @param analysisPath            The path to the \code{RSEE} analysis results.
#' @param cohortMethodDataFolder  The directory where the \code{cohortMethodData} object is stored.
#' @param compareOutcomes         The  outcomes for which risk stratified estimates need to be derived.
#' @param timePoint               The time point at which absolute risk differences will be calculated.
#' @param psMethod                Select the propensity score method for the estimation of treatment effects within risk strata. It can be "matchOnPs",
#'                                "stratifyByPs" or "inversePtWeighted".
#' @param weightsType             Only required if \code{weightsType} is "inversePtWeighted". The type of weights for the balancing of covariates.
#'                                Should be either 'ATE' or 'ATT'
#' @param useStabilizedWeights    Only required if \code{weightsType} is "inversePtWeighted". Should stabilized weights be used?
#' @param truncationLevels        Only required if \code{weightsType} is "inversePtWeighted". The level of truncation expressed in percentiles of the propensity score.
#' @param populationCmSettings    A parameter object for the function \code{\link[CohortMethod]{createStudyPopulation}}. Can be generated from
#'                                function \code{createStudyPopulationCmSettings}.
#'
#' @return                        \code{NULL}. The results are all saved.
#'
#' @export

fitOutcomeModels1 <- function(outcomeId,
                              analysisPath,
                              cohortMethodDataFolder,
                              analysisRef,
                              timePoint,
                              psMethod,
                              weightsType,
                              useStabilizedWeights,
                              truncationLevels,
                              populationCmSettings){

  ParallelLogger::logInfo(paste("Calculating main results for outcome:", outcomeId))

  ps <- readRDS(file.path(analysisPath, "Estimation", outcomeId, "ps.rds"))
  cohortMethodData <- CohortMethod::loadCohortMethodData(file = cohortMethodDataFolder)

  if(psMethod == "matchOnPs"){

    matchedPop <- lapply(ps, CohortMethod::matchOnPs)
    models <- lapply(matchedPop,
                     CohortMethod::fitOutcomeModel, stratified = TRUE, modelType = "cox")

    cases <- do.call(rbind, lapply(matchedPop, getCounts, timePoint = timePoint, psMethod = psMethod))
    colnames(cases) <- c("comparator", "treatment")
    cases <- as.data.frame(cases)
    riskStrata <- length(ps)
    cases$riskStratum <- paste0("Q", 1:riskStrata)

    arr <- do.call(rbind, lapply(matchedPop, absoluteRiskReduction, timePoint = timePoint, psMethod = psMethod))
    colnames(arr) <- c("ARR", "lower", "upper")
    arr <- as.data.frame(arr)
    arr$riskStratum <- paste0("Q", 1:riskStrata)
    rrr <- do.call(rbind, lapply(models, relativeRiskReduction))
    colnames(rrr) <- c("HR", "lower", "upper")
    rrr <- as.data.frame(rrr)
    rrr$riskStratum <- paste0("Q", 1:riskStrata)

  }
  else if(psMethod == "stratifyByPs"){ # Need to fix the cases variable for stratification on ps!!!!

    stratifiedPop <- lapply(ps, CohortMethod::stratifyByPs) # Add stratification settings
    cases <- do.call(rbind, lapply(stratifiedPop, getCounts, timePoint = timePoint, psMethod = psMethod))
    colnames(cases) <- c("comparator", "treatment")
    cases <- as.data.frame(cases)
    riskStrata <- length(ps)
    cases$riskStratum <- paste0("Q", 1:riskStrata)
    models <- lapply(stratifiedPop,
                     CohortMethod::fitOutcomeModel, stratified = TRUE, modelType = "cox")
    arr <- do.call(rbind, lapply(stratifiedPop, absoluteRiskReduction, timePoint = timePoint, psMethod = "stratifyByPS"))
    colnames(arr) <- c("ARR", "lower", "upper")
    arr <- as.data.frame(arr)
    arr$riskStratum <- paste0("Q", 1:riskStrata)
    rrr <- do.call(rbind, lapply(models, relativeRiskReduction))
    colnames(rrr) <- c("HR", "lower", "upper")
    rrr <- as.data.frame(rrr)
    rrr$riskStratum <- paste0("Q", 1:riskStrata)

  }
  else if(psMethod == "inversePtWeighted"){

    ps <- lapply(ps,
                 createIPW,
                 weightsType = weightsType,
                 useStabilizedWeights = useStabilizedWeights,
                 truncationLevels = truncationLevels)
    models <- lapply(ps, outcomeModelWeighted, calculateWeights = FALSE)

    cases <- do.call(rbind, lapply(ps, getCounts, timePoint = timePoint, psMethod = "inversePtWeighted"))
    colnames(cases) <- c("comparator", "treatment")
    cases <- as.data.frame(cases)
    riskStrata <- length(ps)
    cases$riskStratum <- paste0("Q", 1:riskStrata)

    arr <- do.call(rbind, lapply(ps, absoluteRiskReduction, timePoint = timePoint, psMethod = "inversePtWeighted"))
    colnames(arr) <- c("ARR", "lower", "upper")
    arr <- as.data.frame(arr)
    arr$riskStratum <- paste0("Q", 1:riskStrata)
    rrr <- do.call(rbind, lapply(models, relativeRiskReduction))
    colnames(rrr) <- c("HR", "lower", "upper")
    rrr <- as.data.frame(rrr)
    rrr$riskStratum <- paste0("Q", 1:riskStrata)

  }

  saveDir <- paste(analysisPath, "Estimation", outcomeId, sep = "/")
  saveRDS(rrr, file = file.path(saveDir, 'relativeRiskReduction.rds'))
  saveRDS(arr, file = file.path(saveDir, 'absoluteRiskReduction.rds'))
  saveRDS(models, file = file.path(saveDir, 'models.rds'))
  saveRDS(cases, file = file.path(saveDir, 'cases.rds'))

  ParallelLogger::logInfo('Saved main the results')

  predLoc <- which(analysisRef$outcomeIds == outcomeId)
  compLoc <- analysisRef$analysisMatrix[, predLoc]
  compareOutcomes <- analysisRef$outcomeIds[as.logical(compLoc)]

  if(length(compareOutcomes[compareOutcomes!=outcomeId]) == 0)
    compareOutcomes <- NULL

  if(!is.null(compareOutcomes)){

    ParallelLogger::logInfo('Generating results for the other outcomes')
    compareOutcomes <- compareOutcomes[compareOutcomes!=outcomeId]
    numberOfComparisons <- length(compareOutcomes)
    resSwitched <- list()
    modelsSwitched <- list()
    kaplanMeierSwitched <- list()
    rseeSwitched <- list()

    for(j in 1:numberOfComparisons){

      ParallelLogger::logInfo(paste("Stratification outcome", outcomeId, "results outcome:", compareOutcomes[j]))
      ParallelLogger::logInfo("Generating population with switched outcome")

      populationCm <-
        CohortMethod::createStudyPopulation(cohortMethodData = cohortMethodData,
                                            outcomeId = compareOutcomes[j],
                                            firstExposureOnly = populationCmSettings$firstExposureOnly,
                                            restrictToCommonPeriod = populationCmSettings$restrictToCommonPeriod,
                                            washoutPeriod = populationCmSettings$washoutPeriod,
                                            removeDuplicateSubjects = TRUE, # needs to be TRUE otherwise there is an error
                                            removeSubjectsWithPriorOutcome = populationCmSettings$removeSubjectsWithPriorOutcome,
                                            priorOutcomeLookback = populationCmSettings$priorOutcomeLookback,
                                            minDaysAtRisk = populationCmSettings$minDaysAtRisk,
                                            riskWindowStart = populationCmSettings$riskWindowStart,
                                            addExposureDaysToStart = populationCmSettings$addExposureDaysToStart,
                                            riskWindowEnd = populationCmSettings$riskWindowEnd,
                                            addExposureDaysToEnd = populationCmSettings$addExposureDaysToEnd,
                                            censorAtNewRiskWindow = populationCmSettings$censorAtNewRiskWindow)

      psSwitchedOutcome <- lapply(ps, switchOutcome, populationCm = populationCm)

      if(psMethod == "matchOnPs"){

        matchedPop <- lapply(psSwitchedOutcome, CohortMethod::matchOnPs)
        models <- lapply(matchedPop,
                         CohortMethod::fitOutcomeModel, stratified = TRUE, modelType = "cox")

        cases <- do.call(rbind, lapply(psSwitchedOutcome, getCounts, timePoint = timePoint, psMethod = "matchOnPs"))
        colnames(cases) <- c("comparator", "treatment")
        cases <- as.data.frame(cases)
        cases$riskStratum <- paste0("Q", 1:riskStrata)

        arr <- do.call(rbind, lapply(matchedPop, absoluteRiskReduction, timePoint = timePoint, psMethod = psMethod))
        colnames(arr) <- c("ARR", "lower", "upper")
        arr <- as.data.frame(arr)
        arr$riskStratum <- paste0("Q", 1:riskStrata)
        rrr <- do.call(rbind, lapply(models, relativeRiskReduction))
        colnames(rrr) <- c("HR", "lower", "upper")
        rrr <- as.data.frame(rrr)
        rrr$riskStratum <- paste0("Q", 1:riskStrata)

      }
      else if(psMethod == "stratifyByPs"){ # Need to fix the cases variable for stratification on ps !!!!

        stratifiedPop <- lapply(psSwitchedOutcome, CohortMethod::stratifyByPs)

        models <- lapply(stratifiedPop,
                         CohortMethod::fitOutcomeModel, stratified = TRUE, modelType = "cox")

        cases <- do.call(rbind, lapply(stratifiedPop, getCounts, timePoint = timePoint, psMethod = psMethod))
        colnames(cases) <- c("comparator", "treatment")
        cases <- as.data.frame(cases)
        riskStrata <- length(ps)
        cases$riskStratum <- paste0("Q", 1:riskStrata)

        arr <- do.call(rbind, lapply(stratifiedPop, absoluteRiskReduction, timePoint = timePoint, psMethod = "stratifyByPS"))
        colnames(arr) <- c("ARR", "lower", "upper")
        arr <- as.data.frame(arr)
        arr$riskStratum <- paste0("Q", 1:riskStrata)

        rrr <- do.call(rbind, lapply(models, relativeRiskReduction))
        colnames(rrr) <- c("HR", "lower", "upper")
        rrr <- as.data.frame(rrr)
        rrr$riskStratum <- paste0("Q", 1:riskStrata)

      }
      else if(psMethod == "inversePtWeighted"){

        psSwitchedOutcome <- lapply(psSwitchedOutcome,
                                    createIPW,
                                    weightsType = weightsType,
                                    useStabilizedWeights = useStabilizedWeights,
                                    truncationLevels = truncationLevels)
        models <- lapply(psSwitchedOutcome, outcomeModelWeighted, calculateWeights = FALSE)

        cases <- do.call(rbind, lapply(psSwitchedOutcome, getCounts, timePoint = timePoint, psMethod = psMethod))
        colnames(cases) <- c("comparator", "treatment")
        cases <- as.data.frame(cases)
        cases$riskStratum <- paste0("Q", 1:riskStrata)

        arr <- do.call(rbind, lapply(psSwitchedOutcome, absoluteRiskReduction, timePoint = timePoint, psMethod = "inversePtWeighted"))
        colnames(arr) <- c("ARR", "lower", "upper")
        arr <- as.data.frame(arr)
        arr$riskStratum <- paste0("Q", 1:riskStrata)
        rrr <- do.call(rbind, lapply(models, relativeRiskReduction))
        colnames(rrr) <- c("HR", "lower", "upper")
        rrr <- as.data.frame(rrr)
        rrr$riskStratum <- paste0("Q", 1:riskStrata)
      }

      saveDir <- paste(analysisPath, "Estimation", outcomeId, compareOutcomes[j], sep = "/")
      if(!dir.exists(saveDir)){dir.create(saveDir, recursive = T)}
      saveRDS(rrr, file = file.path(saveDir, 'relativeRiskReduction.rds'))
      saveRDS(arr, file = file.path(saveDir, 'absoluteRiskReduction.rds'))
      saveRDS(models, file = file.path(saveDir, 'models.rds'))
      saveRDS(cases, file = file.path(saveDir, 'cases.rds'))

    }
  }
  return(NULL)
}

#' Calculate propensity scores for a specific outcome
#'
#' Fits a large-scale regularized regression model to estimate propensity scores within predicted risk strata. Designed
#' to be applied in a parallelized analysis.
#'
#' @param cohortMethodDataFolder               The directory where the \code{cohortMethodData} object is stored.
#' @param outcomeId                            The outcome of interest for which the risk stratification is performed.
#' @param populationCmSettings                 A parameter object for the function \code{\link[CohortMethod]{createStudyPopulation}}.
#'                                             Can be generated from function \code{createStudyPopulationCmSettings}.
#'                                             Can be generated from unction \code{\link[PatientLevelPrediction]{createStudyPopulationSettings}}.
#' @param analysisId                           The analysis ID of the prediction model used to stratify the population.
#' @param analysisPath                         The directory where the propensity scores will be stored.
#' @param psControl                            An object of the type \code{cyclopsControl} generated from \code{\link[Cyclops]{createControl}}.
#' @param psPrior                              An object of the type \code{cyclopsPrior} generated from \code{\link[Cyclops]{createPrior}}.
#'
#' @return                                     \code{NULL}. The results are all saved.
#'
#' @export

fitPsModelOverall <- function(outcomeId,
                              getDataSettings,
                              populationCmSettings,
                              analysisSettings,
                              runCmSettings){

  cohortMethodData <- CohortMethod::loadCohortMethodData(file = getDataSettings$cohortMethodDataFolder)

  populationCm <-
    CohortMethod::createStudyPopulation(cohortMethodData = cohortMethodData,
                                        outcomeId = outcomeId,
                                        firstExposureOnly = populationCmSettings$firstExposureOnly,
                                        restrictToCommonPeriod = populationCmSettings$restrictToCommonPeriod,
                                        washoutPeriod = populationCmSettings$washoutPeriod,
                                        removeDuplicateSubjects = populationCmSettings$removeDuplicateSubjects,
                                        removeSubjectsWithPriorOutcome =
                                          populationCmSettings$removeSubjectsWithPriorOutcome,
                                        priorOutcomeLookback = populationCmSettings$priorOutcomeLookback,
                                        minDaysAtRisk = populationCmSettings$minDaysAtRisk,
                                        riskWindowStart = populationCmSettings$riskWindowStart,
                                        addExposureDaysToStart = populationCmSettings$addExposureDaysToStart,
                                        riskWindowEnd = populationCmSettings$riskWindowEnd,
                                        addExposureDaysToEnd = populationCmSettings$addExposureDaysToEnd,
                                        censorAtNewRiskWindow = populationCmSettings$censorAtNewRiskWindow)

  ps <- CohortMethod::createPs(cohortMethodData = cohortMethodData,
                               population = populationCm,
                               includeCovariateIds = runCmSettings$psSettings$includeCovariateIds,
                               maxCohortSizeForFitting = runCmSettings$psSettings$maxCohortSizeForFitting,
                               errorOnHighCorrelation = runCmSettings$psSettings$errorOnHighCorrelation,
                               stopOnError = runCmSettings$psSettings$stopOnError,
                               control = runCmSettings$psSettings$control,
                               prior = runCmSettings$psSettings$control)


  saveDir <- file.path(analysisSettings$saveDirectory, analysisSettings$analysisId, "Estimation", outcomeId)
  dir.create(saveDir, recursive = TRUE)
  saveRDS(ps,
          file.path(saveDir, "psFull.rds"))


  ParallelLogger::logInfo(paste("Calculated overall propensity scores for outcome", outcomeId))

  return(NULL)
}


#' Fit overall outcome model
#'
#' Fits outcome models within risk strata, estimating relative and absolute differences. Designed to be performed within
#' a parellelized analysis.
#'
#' @param outcomeId               The outcome of interest for which the esitmation is performed. That is the outcome for which risk stratification is performed.
#' @param analysisPath            The path to the \code{RSEE} analysis results.
#' @param cohortMethodDataFolder  The directory where the \code{cohortMethodData} object is stored.
#' @param timePoint               The time point at which absolute risk differences will be calculated.
#' @param psMethod                Select the propensity score method for the estimation of treatment effects within risk strata. It can be "matchOnPs",
#'                                "stratifyByPs" or "inversePtWeighted".
#' @param weightsType             Only required if \code{weightsType} is "inversePtWeighted". The type of weights for the balancing of covariates.
#'                                Should be either 'ATE' or 'ATT'
#' @param useStabilizedWeights    Only required if \code{weightsType} is "inversePtWeighted". Should stabilized weights be used?
#' @param truncationLevels        Only required if \code{weightsType} is "inversePtWeighted". The level of truncation expressed in percentiles of the propensity score.
#' @param populationCmSettings    A parameter object for the function \code{\link[CohortMethod]{createStudyPopulation}}. Can be generated from
#'                                function \code{createStudyPopulationCmSettings}.
#'
#' @return                        \code{NULL}. The results are all saved.
#'
#' @export

fitOutcomeModelsOverall <- function(outcomeId,
                                    analysisSettings,
                                    getDataSettings,
                                    runCmSettings){

  ParallelLogger::logInfo(paste("Calculating main results for outcome:", outcomeId))

  ps <- readRDS(file.path(analysisSettings$saveDirectory, analysisSettings$analysisId, "Estimation", outcomeId,
                          "psFull.rds"))
  cohortMethodData <- CohortMethod::loadCohortMethodData(file = getDataSettings$cohortMethodDataFolder)

  if(runCmSettings$psMethod == "matchOnPs"){

    matchedPop <-  CohortMethod::matchOnPs(ps,
                                           caliper = runCmSettings$effectEstimationSettings$caliper,
                                           caliperScale = runCmSettings$effectEstimationSettings$caliperScale,
                                           maxRatio = runCmSettings$effectEstimationSettings$maxRatio,
                                           stratificationColumns =
                                             runCmSettings$effectEstimationSettings$stratificationColumns)

    outcomeModel <- CohortMethod::fitOutcomeModel(matchedPop,
                                                  stratified = TRUE,
                                                  modelType = "cox")

    arr <- absoluteRiskReduction(matchedPop,
                                 timePoint = runCmSettings$timePoint,
                                 psMethod = "matchOnPs")

    rrr <- relativeRiskReduction(outcomeModel)

  }
  else if(runCmSettings$psMethod == "stratifyByPs"){

    stratifiedPop <- CohortMethod::stratifyByPs(ps,
                                                numberOfStrata = runCmSettings$effectEstimationSettings$numberOfStrata,
                                                stratificationColumns =
                                                  runCmSettings$effectEstimationSettings$stratificationColumns,
                                                baseSelection = runCmSettings$effectEstimationSettings$baseSelection)

    outcomeModel <- CohortMethod::fitOutcomeModel(stratifiedPop,
                                                  stratified = TRUE,
                                                  modelType = "cox")

    arr <- absoluteRiskReduction(stratifiedPop,
                                 timePoint = runCmSettings$timePoint,
                                 psMethod = "stratifyByPS")
    rrr <- relativeRiskReduction(outcomeModel)

  }
  else if(psMethod == "inversePtWeighted"){

    ps <- createIPW(ps,
                    weightsType = runCmSettings$effectEstimationSettings$weightsType,
                    useStabilizedWeights = runCmSettings$effectEstimationSettings$useStabilizedWeights,
                    truncationLevels = runCmSettings$effectEstimationSettings$truncationLevels)

    outcomeModel <- outcomeModelWeighted(ps,
                                         calculateWeights = FALSE)

    arr <- absoluteRiskReduction(ps,
                                 timePoint = runCmSettings$timePoint,
                                 psMethod = "inversePtWeighted")

    rrr <- relativeRiskReduction(outcomeModel)

  }

  overallResult <- list(absoluteRiskReduction = arr,
                        relativeRiskReduction = rrr,
                        outcomeModel = outcomeModel)

  saveDir <- paste(analysisPath, "Estimation", outcomeId, sep = "/")
  saveRDS(overallResult, file = file.path(saveDir, 'overallResult.rds'))

  ParallelLogger::logInfo('Saved the overall  results')

  return(NULL)
}
