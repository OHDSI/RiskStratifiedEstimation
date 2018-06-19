runRiskStratifiedEstimation <- function(cohortMethodData, population, modelSettings, save,
                                        testSplit = 'person', testFraction = .3, nfold = 10,
                                        riskStrata = 4, weightsType = 'ATE', truncatedWeights = TRUE,
                                        useStabilizedWeights = FALSE, truncationQuantiles = c(.01, .99),
                                        timePoint, binary = TRUE, includeAllOutcomes = TRUE,
                                        requireTimeAtRisk = TRUE, plpPlot = TRUE, psThreads = 1,
                                        treatmentLabel = NULL, comparatorLabel = NULL){

  #########################################
  # PREDICTION
  #########################################

  plpData <- cmToPlpData(cohortMethodData)

  subFolder <- modelSettings$name
  outputFolder <- file.path(save, subFolder, fsep = '\\')
  dir.create(outputFolder, recursive = TRUE)
  populationCall <- attr(population, 'metaData')$call

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

  # Run the prediction model ----
  resultsPrediction <- PatientLevelPrediction::runPlp(
    population = populationPlp,
    plpData = plpData,
    modelSettings = modelSettings,
    testSplit = testSplit,
    testFraction = testFraction,
    nfold = nfold
  )

  PatientLevelPrediction::savePlpResult(resultsPrediction, dirPath = outputFolder)

  # add plots and document to output folder
  if(plpPlot)
    PatientLevelPrediction::plotPlp(resultsPrediction,
      file.path(outputFolder, 'Plots', resultsPrediction$analysisRef$analysisId, fsep = '\\'))


  #########################################
  # RISK STRATIFICATION
  #########################################

  # creates new column with stratum numbers -> stores the stratum number and the subjectId to mapMatrix
  mapMatrix <- dplyr::mutate(resultsPrediction$prediction,
                             riskStratum = dplyr::ntile(resultsPrediction$prediction$value, riskStrata))
  mapMatrix <- subset(mapMatrix, select = c('subjectId', 'riskStratum'))
  saveRDS(mapMatrix, file.path(save, 'mapMatrix.rds', fsep = '\\'))


  #########################################
  # RISK STRATIFIED ANALYSIS
  #########################################

  cl <- makeSOCKcluster(psThreads)
  registerDoSNOW(cl)

  pb <- txtProgressBar(max = riskStrata, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)

  ps <- list()
  tt <- Sys.time()

  ps <- foreach(i = 1:4, .options.snow = opts) %dopar%{

    populationRiskStratified <- population[population$subjectId %in% mapMatrix$subjectId[mapMatrix$riskStratum == i], ]
    CohortMethod::createPs(cohortMethodData = cohortMethodData,
                           population = populationRiskStratified,
                           control = Cyclops::createControl(threads = -1,
                                                            tolerance = 2e-07,
                                                            cvRepetitions = 10,
                                                            startingVariance = .01))

  }
  stopCluster(cl)
  close(pb)

  saveRDS(ps, file.path(save, 'psList.rds', fsep = '\\'))


  #########################################
  # Weighted K-M estimates
  #########################################

  dataKM <- list()
  for(i in 1:riskStrata){

    dataKM[[i]] <- weightedKM(ps[[i]],
                              weightsType = weightsType,
                              truncatedWeights = truncatedWeights,
                              useStabilizedWeights = useStabilizedWeights,
                              truncationQuantiles = truncationQuantiles)
  }

  saveRDS(dataKM, file = file.path(save, 'dataKM.rds'))

  results <- list(ps = ps,
                  dataKM = dataKM)
  return(results)
}


#   nModels <- length(subFolder)
#   dataARR <- data.frame(ARR = numeric(),
#                         lower95 = numeric(),
#                         upper95 = numeric(),
#                         riskStratum = numeric())
#   dataHR <- data.frame(HR = numeric(),
#                        lower95 = numeric(),
#                        upper95 = numeric(),
#                        riskStratum = numeric())
#   for(i in 1:nModels){
#
#     directory <- file.path(save, subFolder[[i]], 'psList.rds', fsep = '\\')
#     psList <- readRDS(directory)
#     dataARR <- rbind(dataARR, absoluteRiskReductionDataFrame(psList = psList,
#                                                              timePoint = timePoint,
#                                                              useSW = F,
#                                                              truncatedWeights = TRUE,
#                                                              truncationQuantiles = c(.01, .99)))
#     dataHR <- rbind(dataHR, hazardRatioDataFrame(psList = psList,
#                                                  useSW = TRUE,
#                                                  truncatedWeights = TRUE,
#                                                  truncationQuantiles = c(.01, .99)))
#
#   }
#   dataARR$riskModel <- factor(rep(1:nModels, each = nStrata))
#   levels(dataARR$riskModel) <- names(subFolder)
#   dataHR$riskModel <- factor(rep(1:nModels, each = nStrata))
#   levels(dataHR$riskModel) <- names(subFolder)
#
#   grid::grid.newpage()
#   comparisonPlotGeneral(dataARR,
#                         dataHR,
#                         ylimARR = c(-.1, .1),
#                         ylimHR = c(0, 1.5),
#                         legend.position = c(.05, .1))
#
#
#   pdf(file.path(mainFolder, 'comparisonPlotNew.pdf', fsep = '\\'), onefile = FALSE)
#   print(comparisonPlotGeneral(dataARR,
#                               dataHR,
#                               ylimARR = c(-.1, .1),
#                               ylimHR = c(0, 1.5),
#                               legend.position = c(.08, .15)))
#   dev.off()
#   result <- NUll
# }
