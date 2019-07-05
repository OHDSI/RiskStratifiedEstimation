#' Runs a risk stratified analysis
#'
#' Runs a risk stratified analysis in two stages. It first runs a prediction algorithm using \code{PatientLevelPrediction} to derive baseline
#' patient risks and then derives estimates within risk strata using \code{CohortMethod} package.
#'
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
#' @param psThreads                          The number of threads to be used for the estimation of the propensity scores.
#' @param predictionThreads                  The number of threads to be used to run the predictions.
#' @param saveResults                        Should the results of the entire analysis be saved?
#' @param saveDirectory                      The file path to the directory where the results of the analysis will be saved.
#' @param fftempdir                          The directory where the temporary \code{ff} files will be saved.
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
  function(cdmDatabaseSchema, cohortDatabaseSchema, outcomeDatabaseSchema, resultsDatabaseSchema,
           cohortTable, outcomeTable, mergedCohortTable, attributeDefinitionTable, cohortAttributeTable,
           treatmentCohortId, comparatorCohortId, outcomeIds, targetCohortId, connectionDetails,
           getDbCohortMethodDataArgs, covariateSettingsCm, populationCmSettings, exposureTable,
           psControl = NULL, psPrior = NULL, psMethod = "matchOnPs", exposureDatabaseSchema, getPlpDataArgs,
           covariateSettingsPlp, modelSettings, populationPlpSettings, cdmVersion = "5",
           runPlpArgs, riskStrata = 4, weightsType = "ATE", useStabilizedWeights = TRUE,
           truncationLevels = c(.01, .99), timePoint, compareAllOutcomes = TRUE,
           psThreads = 1, predictionThreads = 1, saveResults, saveDirectory = NULL, fftempdir,
           saveMapMatrix = TRUE, savePs = TRUE, verbosity = "INFO", analysisId = NULL){

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
      analysisId <- paste(gsub(':','',gsub('-','',gsub(' ','',start.all))), 'RSEE')

    if(is.null(saveDirectory)) saveDirectory <- file.path(getwd(),'RSEE') # if NULL save to wd


    analysisPath = file.path(saveDirectory, analysisId)
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

    ParallelLogger::logInfo("Constructing the plpData object")

    runPrediction <- function(x,
                              populationPlpSettings,
                              modelSettings,
                              plpData,
                              outcomeIds,
                              testSplit,
                              testFraction,
                              nfold,
                              saveDirectory){

      ParallelLogger::registerLogger(logger)

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
                                       saveDirectory = paste(saveDirectory, "Prediction", outcomeIds[x], sep = "/"),
                                       minCovariateFraction = runPlpArgs$minCovariateFraction,
                                       normalizeData = runPlpArgs$normalizeData ,
                                       testSplit = runPlpArgs$testSplit ,
                                       testFraction = runPlpArgs$testFraction ,
                                       trainFraction = runPlpArgs$trainFraction ,
                                       nfold = runPlpArgs$nfold ,
                                       indexes = runPlpArgs$indexes ,
                                       savePlpData = runPlpArgs$savePlpData ,
                                       savePlpResult = runPlpArgs$savePlpResult ,
                                       savePlpPlots = runPlpArgs$savePlpPlots ,
                                       saveEvaluation = runPlpArgs$saveEvaluation ,
                                       verbosity = runPlpArgs$verbosity ,
                                       timeStamp = runPlpArgs$timeStamp ,
                                       analysisId = runPlpArgs$analysisId)

      res <- list(populationPlp = populationPlp,
                  predict = predictionResult$model$predict)
      return(res)

    }



    predictionList <- list()
    lengthOutcomes <- length(outcomeIds)
    cl <- ParallelLogger::makeCluster(numberOfThreads = predictionThreads)
    predictionList <-  ParallelLogger::clusterApply(cluster = cl,
                                                    fun = runPrediction,
                                                    x = 1:lengthOutcomes,
                                                    plpData = plpData,
                                                    populationPlpSettings = populationPlpSettings,
                                                    modelSettings = modelSettings,
                                                    testSplit = testSplit,
                                                    outcomeIds = outcomeIds,
                                                    testFraction = testFraction,
                                                    nfold = nfold,
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
    ParallelLogger::logInfo("Done")

    if(is.null(psControl)){
      psControl <-  Cyclops::createControl(threads = -1, maxIterations = 1e4)}
    if(is.null(psPrior)){
      psPrior <- Cyclops::createPrior(priorType = "laplace",
                                      exclude = c(0),
                                      useCrossValidation = TRUE)}
    numberOfOutcomes <- length(outcomeIds)

    ParallelLogger::logInfo("Starting propensity score estimation")
    psOverAllOutcomes <- list()

    psOverAllOutcomes <-
      foreach::foreach(x = 1:numberOfOutcomes,
                       .combine = "list",
                       .multicombine = TRUE,
                       .packages = "foreach") %do%{

                         options(fftempdir = fftempdir)
                         populationCm <-
                           CohortMethod::createStudyPopulation(cohortMethodData = cohortMethodData,
                                                               outcomeId = outcomeIds[x],
                                                               firstExposureOnly = populationCmSettings$firstExposureOnly,
                                                               restrictToCommonPeriod = populationCmSettings$restrictToCommonPeriod,
                                                               washoutPeriod = populationCmSettings$washoutPeriod,
                                                               removeDuplicateSubjects = populationCmSettings$removeDuplicateSubjects,
                                                               removeSubjectsWithPriorOutcome = populationCmSettings$removeSubjectsWithPriorOutcome,
                                                               priorOutcomeLookback = populationCmSettings$priorOutcomeLookback,
                                                               minDaysAtRisk = populationCmSettings$minDaysAtRisk,
                                                               riskWindowStart = populationCmSettings$riskWindowStart,
                                                               addExposureDaysToStart = populationCmSettings$addExposureDaysToStart,
                                                               riskWindowEnd = populationCmSettings$riskWindowEnd,
                                                               addExposureDaysToEnd = populationCmSettings$addExposureDaysToEnd,
                                                               censorAtNewRiskWindow = populationCmSettings$censorAtNewRiskWindow)

                         populationCmMetaData <- attr(populationCm, "metaData")
                         attr(populationCm, "metaData") <- attr(predictionList[[x]]$populationPlp, "metaData")
                         riskPredictions <- predictionList[[x]]$predict(plpData = plpData,
                                                                        population = populationCm)
                         riskPredictions <- subset(riskPredictions,
                                                   select = c(rowId, subjectId, value))

                         attr(populationCm, "metaData") <- populationCmMetaData
                         ParallelLogger::logInfo("Stratifying estimation population")
                         mapMatrix <- riskPredictions
                         mapMatrix <- dplyr::mutate(mapMatrix, riskStratum = dplyr::ntile(riskPredictions$value,
                                                                                          riskStrata))
                         ps <- list()
                         for(i in 1:riskStrata){
                           population <- populationCm[populationCm$subjectId %in% mapMatrix[mapMatrix$riskStratum == i,]$subjectId, ]
                           ps[[i]] <- ff::as.ffdf(CohortMethod::createPs(cohortMethodData = cohortMethodData,
                                                                         population = population,
                                                                         excludeCovariateIds = getDbCohortMethodDataArgs$covariateSettings$excludedCovariateConceptIds,
                                                                         control = psControl,
                                                                         prior = psPrior))
                         }


                         if(savePs){

                           saveDir <- paste(analysisPath, "Estimation", outcomeIds[x], sep = "/")
                           dir.create(saveDir, recursive = TRUE)
                           saveRDS(lapply(ps, as.data.frame),
                                   paste(saveDir, "ps.rds", sep = "/"))

                         }

                         if(saveMapMatrix){
                           saveRDS(mapMatrix, paste(analysisPath, "Estimation", outcomeIds[x], 'mapMatrix.rds', sep = "/"))
                           ParallelLogger::logInfo("Saved the map matrix")
                         }

                         return(list(ps = ps,
                                     mapMatrix = mapMatrix))

                       }



    ParallelLogger::logInfo("Done estimating propensity scores")
    ParallelLogger::logInfo("Starting calculation of results")

    resultsOverAllOutcomes <-
      foreach::foreach(i = 1:length(outcomeIds),
                       .combine = list,
                       .multicombine = TRUE,
                       .packages = "RiskStratifiedEstimation")%do%{

                         ParallelLogger::logInfo(paste("Calculating main results for outcome:", outcomeIds[i]))
                         ps <- psOverAllOutcomes[[i]]$ps

                         cases <- do.call(rbind, lapply(ps, getCounts, timePoint = timePoint))
                         colnames(cases) <- c("comparator", "treatment")
                         cases <- as.data.frame(cases)
                         cases$riskStratum <- paste0("Q", 1:riskStrata)

                         if(psMethod == "matchOnPs"){

                           matchedPop <- lapply(lapply(ps, as.data.frame), CohortMethod::matchOnPs)
                           models <- lapply(matchedPop,
                                            CohortMethod::fitOutcomeModel, stratified = TRUE, modelType = "cox")
                           arr <- do.call(rbind, lapply(matchedPop, absoluteRiskReduction, timePoint = timePoint, psMethod = psMethod))
                           colnames(arr) <- c("ARR", "lower", "upper")
                           arr <- as.data.frame(arr)
                           arr$riskStratum <- paste0("Q", 1:riskStrata)
                           rrr <- do.call(rbind, lapply(models, relativeRiskReduction))
                           colnames(rrr) <- c("HR", "lower", "upper")
                           rrr <- as.data.frame(rrr)
                           rrr$riskStratum <- paste0("Q", 1:riskStrata)

                         }
                         else if(psMethod == "stratifyByPs"){

                           stratifiedPop <- lapply(lapply(ps, as.data.frame), CohortMethod::stratifyByPs)
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

                           ps <- lapply(lapply(ps, as.data.frame),
                                        createIPW,
                                        weightsType = weightsType,
                                        useStabilizedWeights = useStabilizedWeights,
                                        truncationLevels = truncationLevels)
                           models <- lapply(ps, outcomeModelWeighted, calculateWeights = FALSE)
                           arr <- do.call(rbind, lapply(ps, absoluteRiskReduction, timePoint = timePoint, psMethod = "inversePtWeighted"))
                           colnames(arr) <- c("ARR", "lower", "upper")
                           arr <- as.data.frame(arr)
                           arr$riskStratum <- paste0("Q", 1:riskStrata)
                           rrr <- do.call(rbind, lapply(models, relativeRiskReduction))
                           colnames(rrr) <- c("HR", "lower", "upper")
                           rrr <- as.data.frame(rrr)
                           rrr$riskStratum <- paste0("Q", 1:riskStrata)

                         }


                         rsee <- list(models = models,
                                      relative = rrr,
                                      absolute = arr,
                                      cases = cases)

                         ParallelLogger::logInfo('Done estimating results')

                         if(saveResults){
                           saveDir <- paste(analysisPath, "Estimation", outcomeIds[i], sep = "/")
                           saveRDS(rsee$relative, file = file.path(saveDir, 'relativeRiskReduction.rds'))
                           saveRDS(rsee$absolute, file = file.path(saveDir, 'absoluteRiskReduction.rds'))
                           saveRDS(rsee$models, file = file.path(saveDir, 'models.rds'))
                           saveRDS(rsee$cases, file = file.path(saveDir, 'cases.rds'))

                           ParallelLogger::logInfo('Saved main the results')
                         }
                         ##################
                         ParallelLogger::logInfo('Generating results for the other outcomes')

                         if(compareAllOutcomes){

                           numberOfComparisons <- length(outcomeIds) - 1
                           comparisonOutcomes <- outcomeIds[outcomeIds!=outcomeIds[i]]
                           resSwitched <- list()
                           modelsSwitched <- list()
                           kaplanMeierSwitched <- list()
                           rseeSwitched <- list()

                           for(j in 1:numberOfComparisons){

                             ParallelLogger::logInfo(paste("Stratification outcome", outcomeIds[i], "results outcome:", comparisonOutcomes[j]))
                             ParallelLogger::logInfo("Generating population with switched outcome")

                             populationCm <-
                               CohortMethod::createStudyPopulation(cohortMethodData = cohortMethodData,
                                                                   outcomeId = comparisonOutcomes[j],
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
                             cases <- do.call(rbind, lapply(psSwitchedOutcome, getCounts, timePoint = timePoint))
                             colnames(cases) <- c("comparator", "treatment")
                             cases <- as.data.frame(cases)
                             cases$riskStratum <- paste0("Q", 1:riskStrata)

                             if(psMethod == "matchOnPs"){

                               matchedPop <- lapply(lapply(psSwitchedOutcome, as.data.frame), CohortMethod::matchOnPs)
                               models <- lapply(matchedPop,
                                                CohortMethod::fitOutcomeModel, stratified = TRUE, modelType = "cox")
                               arr <- do.call(rbind, lapply(matchedPop, absoluteRiskReduction, timePoint = timePoint, psMethod = psMethod))
                               colnames(arr) <- c("ARR", "lower", "upper")
                               arr <- as.data.frame(arr)
                               arr$riskStratum <- paste0("Q", 1:riskStrata)
                               rrr <- do.call(rbind, lapply(models, relativeRiskReduction))
                               colnames(rrr) <- c("HR", "lower", "upper")
                               rrr <- as.data.frame(rrr)
                               rrr$riskStratum <- paste0("Q", 1:riskStrata)

                             }
                             else if(psMethod == "stratifyByPs"){

                               stratifiedPop <- lapply(lapply(psSwitchedOutcome, as.data.frame), CohortMethod::stratifyByPs)
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

                               psSwitchedOutcome <- lapply(lapply(psSwitchedOutcome, as.data.frame),
                                                           createIPW,
                                                           weightsType = weightsType,
                                                           useStabilizedWeights = useStabilizedWeights,
                                                           truncationLevels = truncationLevels)
                               models <- lapply(psSwitchedOutcome, outcomeModelWeighted, calculateWeights = FALSE)
                               arr <- do.call(rbind, lapply(psSwitchedOutcome, absoluteRiskReduction, timePoint = timePoint, psMethod = "inversePtWeighted"))
                               colnames(arr) <- c("ARR", "lower", "upper")
                               arr <- as.data.frame(arr)
                               arr$riskStratum <- paste0("Q", 1:riskStrata)
                               rrr <- do.call(rbind, lapply(models, relativeRiskReduction))
                               colnames(rrr) <- c("HR", "lower", "upper")
                               rrr <- as.data.frame(rrr)
                               rrr$riskStratum <- paste0("Q", 1:riskStrata)

                             }

                             rseeSwitched[[j]] <- list(models = models,
                                                       relative = rrr,
                                                       absolute = arr,
                                                       cases = cases)
                             names(rseeSwitched)[j] <- paste("outcome", comparisonOutcomes[j], sep  ="_")

                             if(saveResults){
                               saveDir <- paste(analysisPath, "Estimation", outcomeIds[i], comparisonOutcomes[j], sep = "/")
                               if(!dir.exists(saveDir)){dir.create(saveDir, recursive=T)}
                               saveRDS(rseeSwitched[[j]]$relative, file = file.path(saveDir, 'relativeRiskReduction.rds'))
                               saveRDS(rseeSwitched[[j]]$absolute, file = file.path(saveDir, 'absoluteRiskReduction.rds'))
                               saveRDS(rseeSwitched[[j]]$models, file = file.path(saveDir, 'models.rds'))
                               saveRDS(rseeSwitched[[j]]$cases, file = file.path(saveDir, 'cases.rds'))

                               ParallelLogger::logInfo('Saved the main results')
                             }
                           }
                         }



                         list(mainOutcome = rsee,
                              otherOutcomes = rseeSwitched)
                       }

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
