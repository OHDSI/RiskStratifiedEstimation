

runRiskStratifiedEstimation <- function(cohortMethodData,population, modelSettings, save,
                                        testSplit = 'person', testFraction = .3, nfold = 10,
                                        riskStrata = 4, excludeCovariateIds = NULL,
                                        useStabilizedWeights = F, truncationQuantiles = c(.025, .975),
                                        timePoint){

  #########################################
  # PREDICTION
  #########################################

  plpData <- cmToPlpData(cohortMethodData)

  subFolder <- 'TrainedModel'
  outputFolder <- file.path(save, subFolder, fsep = '\\')
  dir.create(outputFolder, recursive = TRUE)

  # Run the model ----
  results <- PatientLevelPrediction::runPlp(
    population = population, # Check if this will work using the same
    plpData = plpData,
    modelSettings = modelSettings,
    testSplit = testSplit,
    testFraction = testFraction,
    nfold = nfold
  )

  PatientLevelPrediction::savePlpModel(results$model, dirPath = file.path(outputFolder, "model", fsep = '\\'))
  PatientLevelPrediction::savePlpResult(results, dirPath = file.path(outputFolder, 'lr', fsep = '\\'))

  # add plots and document to output folder
  PatientLevelPrediction::plotPlp(
    results,
    file.path(outputFolder, 'plpmodels', results$analysisRef$analysisId, fsep = '\\')
  )


  #########################################
  # RISK STRATIFICATION
  #########################################

  # creates new column with stratum numbers -> stores the stratum number and the subjectId to mapMatrix
  mapMatrix <- dplyr::mutate(results$prediction, riskStratum = dplyr::ntile(results$prediction$value, riskStrata))[, c(2, 14)]
  saveRDS(mapMatrix, file.path(save, subFolder, 'mapMatrix.rds', fsep = '\\'))


  #########################################
  # RISK STRATIFIED ANALYSIS
  #########################################

  #library(tictoc)
  #library(doParallel)
  #library(foreach)

  cl <- makeCluster(riskStrata)
  registerDoParallel(cl)

  ps <- list()
  tic()
  ps <- foreach(i = 1:riskStrata) %dopar%{

    cohortMethodData <- CohortMethod::loadCohortMethodData(file.path(save, 'Data\\cmData', fsep = '\\'))
    mapMatrix <- readRDS(file.path(save, 'TrainedModel\\mapMatrix.rds', fsep = '\\'))
    populationCm <- readRDS(file.path(save, 'populationCm.rds', fsep = '\\'))
    populationRiskStratified <- populationCm[populationCm$subjectId %in% mapMatrix$subjectId[mapMatrix$riskStratum == i], ]
    CohortMethod::createPs(cohortMethodData = cohortMethodData, population = populationRiskStratified,
                           excludeCovariateIds = excludeCovariateIds)

  }
  toc()
  stopCluster(cl)
  saveRDS(ps, file.path(save, subFolder, 'psList.rds', fsep = '\\'))

  #########################################
  # Weighted K-M curves/combined K-M plot
  #########################################

  psList <- readRDS(file.path(save, subFolder, 'psList.rds', fsep = '\\'))

  subFolder <- file.path('TrainedModel', 'RiskStratifiedAnalysis', fsep = '\\')
  directory <- file.path(mainFolder, subFolder,fsep = '\\')
  dir.create(directory, recursive = TRUE)

  plotsKM <- list()
  for(i in 1:riskStrata){

    ps <- psList[[i]]

    plotsKM[[i]] <- weightedKM(ps,
                               useSW = useStabilizedWeights,
                               truncationQuantiles = truncationQuantiles,
                               title = paste0('Risk Stratum ', i),
                               legend.position = c(.2, .14))$plot

    # pdf(file.path(directory, paste0('KMIPW_mine', i, '.pdf'), fsep = '\\'), onefile = FALSE)
    # print(plotsKM[[i]])
    # dev.off()

    print(i)

  }


  pdf(file.path(directory, 'combinedKM.pdf', fsep = '\\'), onefile = FALSE)
  print(gridExtra::grid.arrange(plotsKM[[1]],
                                plotsKM[[2]],
                                plotsKM[[3]],
                                plotsKM[[4]],
                                nrow = 2))
  dev.off() # Needed?

  pdf(file.path(directory, 'comparisonPlotHR.pdf', fsep = '\\'), onefile = FALSE)
  print(comparisonPlotHr(psList)$plot)
  dev.off()

  nModels <- length(subFolder)
  dataARR <- data.frame(ARR = numeric(),
                        lower95 = numeric(),
                        upper95 = numeric(),
                        riskStratum = numeric())
  dataHR <- data.frame(HR = numeric(),
                       lower95 = numeric(),
                       upper95 = numeric(),
                       riskStratum = numeric())
  for(i in 1:nModels){

    directory <- file.path(save, subFolder[[i]], 'psList.rds', fsep = '\\')
    psList <- readRDS(directory)
    dataARR <- rbind(dataARR, absoluteRiskReductionDataFrame(psList = psList,
                                                             timePoint = timePoint,
                                                             useSW = F,
                                                             truncatedWeights = TRUE,
                                                             truncationQuantiles = c(.01, .99)))
    dataHR <- rbind(dataHR, hazardRatioDataFrame(psList = psList,
                                                 useSW = TRUE,
                                                 truncatedWeights = TRUE,
                                                 truncationQuantiles = c(.01, .99)))

  }
  dataARR$riskModel <- factor(rep(1:nModels, each = nStrata))
  levels(dataARR$riskModel) <- names(subFolder)
  dataHR$riskModel <- factor(rep(1:nModels, each = nStrata))
  levels(dataHR$riskModel) <- names(subFolder)

  grid::grid.newpage()
  comparisonPlotGeneral(dataARR,
                        dataHR,
                        ylimARR = c(-.1, .1),
                        ylimHR = c(0, 1.5),
                        legend.position = c(.05, .1))


  pdf(file.path(mainFolder, 'comparisonPlotNew.pdf', fsep = '\\'), onefile = FALSE)
  print(comparisonPlotGeneral(dataARR,
                              dataHR,
                              ylimARR = c(-.1, .1),
                              ylimHR = c(0, 1.5),
                              legend.position = c(.08, .15)))
  dev.off()
  result <- NUll
}
