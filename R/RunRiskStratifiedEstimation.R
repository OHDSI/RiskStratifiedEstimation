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
#' @param fixedTruncationLevels The levels for fixed truncation weighting
#' @param useStabilizedWeights Should stabilized weights be used?
#' @param extremeWeights The way to assess extreme weights. Possible options are 'unadjusted, 'cvLikeTruncation', 'crumpTrimming'
#' @param truncationLevels The level of truncation expressed in percentiles of the propensity score. Only symmetric truncation is available. E.g. truncationLevels =.01 will assess truncation up to the .99th percentile of ps
#' @param cvLikeRepetitions The number of times to repeat the 2-fold cross-validations
#' @param stepTruncationLevels The steps for the grid of possible truncation levels
#' @param timePoint The time point of interest for the calculation of the absolute risk reduction
#' @param excludeCovariateIds Covariate Ids to be excluded from calculation of propensity scores
#' @param binary Forces the outcomeCount to be 0 or 1 in the prediction step
#' @param includeAllOutcomes (binary) indicating whether to include people with outcomes who are not observed for the whole at risk period
#' @param requireTimeAtRisk Should subjects without time at risk be removed at the prediction step?
#' @param plpPlot (binary) Should plots for the prediction step be generated?
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
                                        useStabilizedWeights = FALSE, extremeWeights, truncationLevels,
                                        cvLikeRepetitions  = 50, stepTruncationLevels, fixedTruncationLevels = c(.01, .99),
                                        timePoint, excludeCovariateIds = NULL, binary = TRUE, includeAllOutcomes = TRUE,
                                        requireTimeAtRisk = TRUE, plpPlot = FALSE, psThreads = 1, priorType = 'laplace',
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
  outcomeId <- attr(populationCm, 'metaData')$call$outcomeId

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
    save = save,
    saveModel = TRUE
  )


  # PatientLevelPrediction::savePlpResult(resultsPrediction, dirPath = outputFolder)

  # add plots and document to output folder
  if(plpPlot)
    PatientLevelPrediction::plotPlp(resultsPrediction,
      file.path(save, 'Plots', resultsPrediction$analysisRef$analysisId, fsep = '\\'))


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

  cl <- parallel::makePSOCKcluster(psThreads)
  doSNOW::registerDoSNOW(cl)

  pb <- txtProgressBar(max = riskStrata, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  ps <- list()
  tt <- Sys.time()

  ps <- foreach::foreach(i = 1:4, .options.snow = opts) %dopar%{

    populationRiskStratified <- population[population$subjectId %in% mapMatrix$subjectId[mapMatrix$riskStratum == i], ]
    CohortMethod::createPs(cohortMethodData = cohortMethodData,
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
  parallel::stopCluster(cl)
  close(pb)

  saveRDS(ps, file.path(analysisPath, 'ps.rds'))

  for(i in 1:length(ps))
    ps[[i]] <- createIPW(ps[[i]],
                         weightsType = weightsType,
                         useStabilizedWeights = useStabilizedWeights,
                         extremeWeights = extremeWeights,
                         fixedTruncationLevels = fixedTruncationLevels,
                         truncationLevels = truncationLevels,
                         cvLikeRepetitions = cvLikeRepetitions,
                         stepTruncationLevels = stepTruncationLevels)




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
                              fixedTruncationLevels = fixedTruncationLevels,
                              truncationLevels = truncationLevels,
                              cvLikeRepetitions = cvLikeRepetitions,
                              stepTruncationLevels = stepTruncationLevels)
  }

  saveRDS(dataKM, file = file.path(analysisPath, 'dataKM.rds'))

  #########################################
  # Absolute/Relative risk reduction
  #########################################
  AbsoluteRiskReduction <- absoluteRiskReduction(dataKM,
                                                 timePoint)
  saveRDS(AbsoluteRiskReduction, file = file.path(analysisPath, 'absoluteRiskReduction.rds'))

  RelativeRiskReduction <- relativeRiskReduction(ps,
                                                 calculateWeights = FALSE,
                                                 weightsType = weightsType,
                                                 useStabilizedWeights = useStabilizedWeights,
                                                 truncatedWeights = truncatedWeights,
                                                 truncationQuantiles = truncationQuantiles)
  saveRDS(RelativeRiskReduction, file = file.path(analysisPath, 'relativeRiskReduction.rds'))


  results <- list(ps = ps,
                  mapMatrix = mapMatrix,
                  dataKM = dataKM,
                  absoluteRiskReduction = AbsoluteRiskReduction,
                  relativeRiskReduction = RelativeRiskReduction)
  return(results)
}
