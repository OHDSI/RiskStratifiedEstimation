#' Runs a risk stratified analysis
#'
#' @param cohortMethodData A cohortMethodData object
#' @param population The study population to perform the analysis
#' @param modelSettings The model settings for the prediction step
#' @param save The save directory
#' @param testSplit The type of split for the cross validation. Should be either 'person' or 'time'
#' @param testFraction The size of the test set
#' @param nfold The number of folds for cross validation
#' @param riskStrata The number of risk strata on which to perform the analysis
#' @param weightsType The type of weights for the balancing of covariates. Should be either 'ATE' or 'ATT'
#' @param useStabilizedWeights Should stabilized weights be used?
#' @param extremeWeights The way to assess extreme weights. Possible options are 'unadjusted, 'cvLikeTruncation', 'crumpTrimming' and 'fixedTruncation'
#' @param truncationLevels The level of truncation expressed in percentiles of the propensity score. If extremeWeights is 'fixedTruncation' then the weights will be truncated at the levels defined here. If extremeWeights is 'cvLikeTruncation' then the data adaptive procedure will only assess truncation up to the levels defined here
#' @param cvLikeRepetitions The number of times to repeat the 2-fold cross-validations
#' @param stepTruncationLevels The steps for the grid of possible truncation levels
#' @param timePoint The time point of interest for the calculation of the absolute risk reduction
#' @param excludeCovariateIds Covariate Ids to be excluded from calculation of propensity scores
#' @param binary Forces the outcomeCount to be 0 or 1 in the prediction step
#' @param includeAllOutcomes (binary) indicating whether to include people with outcomes who are not observed for the whole at risk period
#' @param requireTimeAtRisk Should subjects without time at risk be removed at the prediction step?
#' @param savePlpPlots (binary) Should plots for the prediction step be generated?
#' @param psThreads The number of cores to use for the estimation of the propensity score. If 1 then serial approach is implemented
#' @param analysisId Identifier of the analysis
#' @param priorType The prior for the propensity score model
#' @param verbosity Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                     \itemize{
#'                     \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                         \item{TRACE}{Showing information about start and end of steps}
#'                                         \item{INFO}{Show informative information (Default)}
#'                                         \item{WARN}{Show warning messages}
#'                                         \item{ERROR}{Show error messages}
#'                                         \item{FATAL}{Be silent except for fatal errors}}

#' @return
#' \item{ps}{The propensity scores within risk strata along with patient weights}
#' \item{mapMatrix}{The matrix that maps the patients to risk strata}
#' \item{dataKM}{The weighted Kaplan-Meier estimates within risk strata}
#' \item{absoluteRiskReduction}{The absolute risk reduction within risk strata}
#' \item{relativeRiskReduction}{The relative risk reduction within risk strata}
#'
#' @export
#'

runRiskStratifiedEstimation <- function(cohortMethodData, population, modelSettings, save,
                                        testSplit = 'person', testFraction = .3, nfold = 10,
                                        riskStrata = 4, weightsType = 'ATE',
                                        useStabilizedWeights = FALSE, extremeWeights = 'fixedTruncation', truncationLevels,
                                        cvLikeRepetitions  = 50, stepTruncationLevels,
                                        timePoint, excludeCovariateIds = NULL, binary = TRUE, includeAllOutcomes = TRUE,
                                        requireTimeAtRisk = TRUE, savePlpPlots = FALSE, psThreads = 1, priorType = 'laplace',
                                        verbosity = 'INFO', analysisId = NULL){

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

  if(is.null(save)) save <- file.path(getwd(),'RSEE') #if NULL save to wd


  analysisPath = file.path(save, analysisId)
  if(!dir.exists(analysisPath)){dir.create(analysisPath, recursive=T)}
  logFileName = paste0(analysisPath,'/logRSEE.txt')

  logger <- OhdsiRTools::createLogger(name = "RSEE Main Log",
                                      threshold = verbosity,
                                      appenders = list(OhdsiRTools::createFileAppender(layout = OhdsiRTools::layoutParallel,
                                                                                       fileName = logFileName)))
  OhdsiRTools::registerLogger(logger)
  logSep <- paste(rep('*', 96), collapse = '')
  OhdsiRTools::logInfo(logSep)

  OhdsiRTools::logInfo(paste0('Risk Stratified Effect Estimation Package version ', utils::packageVersion("RiskStratifiedEstimation")))
  OhdsiRTools::logInfo(logSep)
  # get ids
  targetId <- attr(population, "metaData")$targetId
  comparatorId <- attr(population, "metaData")$comparatorId
  outcomeId <- attr(population, 'metaData')$call$outcomeId

  OhdsiRTools::logInfo(sprintf('%-20s%s', 'AnalysisID: ',analysisId))
  OhdsiRTools::logInfo(sprintf('%-20s%s', 'targetId: ', targetId))
  OhdsiRTools::logInfo(sprintf('%-20s%s', 'comparatorId', comparatorId))
  OhdsiRTools::logInfo(sprintf('%-20s%s', 'OutcomeID: ', outcomeId))
  OhdsiRTools::logInfo(sprintf('%-20s%s', 'Cohort size: ', nrow(cohortMethodData$cohorts)))
  OhdsiRTools::logInfo(sprintf('%-20s%s', 'Covariates: ', nrow(cohortMethodData$covariateRef)))
  OhdsiRTools::logInfo(sprintf('%-20s%s', 'Population size: ', nrow(population)))
  OhdsiRTools::logInfo(sprintf('%-20s%s', 'Cases: ', sum(population$outcomeCount>0)))
  OhdsiRTools::logInfo(sprintf('%-20s%s', 'Risk strata: ', riskStrata))


  #########################################
  # PREDICTION
  #########################################

  OhdsiRTools::logTrace('Converting plpData from coohrtMethodData')
  plpData <- cmToPlpData(cohortMethodData)


  populationCall <- attr(population, 'metaData')$call

  OhdsiRTools::logTrace('Generating prediction study population')
  populationPlp <-
    PatientLevelPrediction::createStudyPopulation(plpData = plpData,
                                                  outcomeId = populationCall$outcomeId,
                                                  firstExposureOnly = populationCall$firstExposureOnly,
                                                  washoutPeriod = populationCall$washoutPeriod,
                                                  removeSubjectsWithPriorOutcome = populationCall$removeSubjectsWithPriorOutcome,
                                                  priorOutcomeLookback = populationCall$priorOutcomeLookback,
                                                  minTimeAtRisk = populationCall$minDaysAtRisk,
                                                  riskWindowStart = populationCall$riskWindowStart,
                                                  addExposureDaysToStart = populationCall$addExposureDaysToStart,
                                                  riskWindowEnd = populationCall$riskWindowEnd,
                                                  addExposureDaysToEnd = populationCall$addExposureDaysToEnd,
                                                  binary = binary,
                                                  includeAllOutcomes = includeAllOutcomes,
                                                  requireTimeAtRisk = requireTimeAtRisk)
  OhdsiRTools::logInfo(logSep)
  OhdsiRTools::logInfo('Generated plpData object and prediction study population')
  OhdsiRTools::logInfo('Starting prediction step')
  # Run the prediction model ----
  resultsPrediction <- PatientLevelPrediction::runPlp(
    population = populationPlp,
    plpData = plpData,
    modelSettings = modelSettings,
    testSplit = testSplit,
    testFraction = testFraction,
    nfold = nfold,
    savePlpPlots = savePlpPlots,
    saveDirectory = save,
    savePlpResult = TRUE
  )

  #########################################
  # RISK STRATIFICATION
  #########################################

  # creates new column with stratum numbers -> stores the stratum number and the subjectId to mapMatrix
  mapMatrix <- dplyr::mutate(resultsPrediction$prediction,
                             riskStratum = dplyr::ntile(resultsPrediction$prediction$value, riskStrata))
  mapMatrix <- subset(mapMatrix, select = c('subjectId', 'riskStratum'))
  saveRDS(mapMatrix, file.path(analysisPath, 'mapMatrix.rds', fsep = '\\'))


  #########################################
  # RISK STRATIFIED ANALYSIS
  #########################################
  OhdsiRTools::logInfo(logSep)
  OhdsiRTools::logInfo('Estimating propensity scores')
  tt <- Sys.time()

  ps <- list()
  psEstimationParallel <- function(k){

    populationRiskStratified <- population[population$subjectId %in% mapMatrix$subjectId[mapMatrix$riskStratum == k], ]
    ps[[k]] <- CohortMethod::createPs(cohortMethodData = cohortMethodData,
                                      population = populationRiskStratified,
                                      excludeCovariateIds = excludeCovariateIds,
                                      control = Cyclops::createControl(threads = -1,
                                                                       tolerance = 2e-07,
                                                                       cvRepetitions = 10,
                                                                       startingVariance = .01),
                                      prior = Cyclops::createPrior(priorType = priorType,
                                                                   exclude = c(0),
                                                                   useCrossValidation = TRUE))
  }

  cl <- OhdsiRTools::makeCluster(psThreads)


  ps <- OhdsiRTools::clusterApply(cl, 1:psThreads, psEstimationParallel)

  OhdsiRTools::stopCluster(cl)

  OhdsiRTools::logInfo(paste('Propensity score estimation took', round(Sys.time() - tt, 2), 'sec'))

  saveRDS(ps, file.path(analysisPath, 'ps.rds'))
  OhdsiRTools::logInfo(paste('Saved propensity score estimates in', save))

  for(i in 1:length(ps))
    ps[[i]] <- createIPW(ps[[i]],
                         weightsType = weightsType,
                         useStabilizedWeights = useStabilizedWeights,
                         extremeWeights = extremeWeights,
                         truncationLevels = truncationLevels,
                         cvLikeRepetitions = cvLikeRepetitions,
                         stepTruncationLevels = stepTruncationLevels)
  OhdsiRTools::logInfo(paste('Generated', weightsType, 'weights within risk strata'))




  #########################################
  # Weighted K-M estimates
  #########################################

  dataKM <- list()
  for(i in 1:riskStrata){

    dataKM[[i]] <- weightedKM(ps[[i]],
                              calculateWeights = FALSE,
                              weightsType = weightsType,
                              useStabilizedWeights = useStabilizedWeights,
                              extremeWeights = extremeWeights,
                              truncationLevels = truncationLevels,
                              cvLikeRepetitions = cvLikeRepetitions,
                              stepTruncationLevels = stepTruncationLevels)
  }

  OhdsiRTools::logInfo('Generated weighted Kaplan-Meier estimates within risk strata')

  saveRDS(dataKM, file = file.path(analysisPath, 'dataKM.rds'))

  #########################################
  # Absolute/Relative risk reduction
  #########################################
  AbsoluteRiskReduction <- absoluteRiskReduction(dataKM,
                                                 timePoint)
  saveRDS(AbsoluteRiskReduction, file = file.path(analysisPath, 'absoluteRiskReduction.rds'))

  OhdsiRTools::logInfo('Estimated absolute risk reduction within risk strata')

  RelativeRiskReduction <- relativeRiskReduction(ps,
                                                 calculateWeights = FALSE,
                                                 weightsType = weightsType,
                                                 useStabilizedWeights = useStabilizedWeights,
                                                 extremeWeights = extremeWeights,
                                                 truncationLevels = truncationLevels,
                                                 cvLikeRepetitions = cvLikeRepetitions,
                                                 stepTruncationLevels = stepTruncationLevels)

  saveRDS(RelativeRiskReduction, file = file.path(analysisPath, 'relativeRiskReduction.rds'))

  treatedCases <- data.frame(riskStratum = numeric(),
                             outcomeRate = numeric())
  comparatorCases <- data.frame(riskStratum = numeric(),
                                outcomeRate = numeric())
  OhdsiRTools::logInfo('Estimated hazard ratios within risk strata')


  for(i in 1:riskStrata){

    treatmentEvents <- subset(dataKM[[i]], eventTime == 1 & cohort == 'treatment')
    sortTimes <- sort(c(timePoint, treatmentEvents$time))
    if(sum(sortTimes == timePoint) == 1){
      positionTreatment <- which(sortTimes == timePoint)
      survivalTreatment <- 1 - treatmentEvents$S[positionTreatment - 1]
    }else{
      positionTreatment <- which(treatmentEvents$time == timePoint)
      survivalTreatment <- 1 - treatmentEvents$S[positionTreatment]
    }

    comparatorEvents <- subset(dataKM[[i]], eventTime == 1 & cohort == 'comparator')
    sortTimes <- sort(c(timePoint, comparatorEvents$time))
    if(sum(sortTimes == timePoint) == 1){
      positionComparator <- which(sortTimes == timePoint)
      survivalComparator <- 1 - comparatorEvents$S[positionComparator - 1]
    }else{
      positionComparator <- which(comparatorEvents$time == timePoint)
      survivalComparator <- 1 - comparatorEvents$S[positionComparator]
    }

    treatedCases[i, ] <- c(i, survivalTreatment)
    comparatorCases[i, ] <- c(i, survivalComparator)

  }

  cases <- dplyr::bind_rows(data = treatedCases, comparatorCases, .id = 'cohort')
  cases$cohort <- factor(cases$cohort, levels = 1:2, labels = c('treatment', 'comparator'))
  cases$riskStratum <- paste('Q', cases$riskStratum, sep = '')
  OhdsiRTools::logInfo('Calculated outcome rates within risk strata')


  results <- list(ps = ps,
                  mapMatrix = mapMatrix,
                  dataKM = dataKM,
                  absoluteRiskReduction = AbsoluteRiskReduction,
                  relativeRiskReduction = RelativeRiskReduction,
                  cases = cases,
                  predictionResult = resultsPrediction)
  OhdsiRTools::logInfo('Run finished successfully')

  # stop logger
  OhdsiRTools::clearLoggers()
  logger <- OhdsiRTools::createLogger(name = "SIMPLE",
                                      threshold = "INFO",
                                      appenders = list(OhdsiRTools::createConsoleAppender(layout = OhdsiRTools::layoutTimestamp)))
  OhdsiRTools::registerLogger(logger)

  return(results)
}
