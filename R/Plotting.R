#' @export
plotPsOverall <- function(ps,
                          analysisSettings,
                          runSettings){
  plotList <-
    lapply(
      ps,
      CohortMethod::plotPs,
      comparatorLabel = unlist(
        analysisSettings$mapTreatments %>%
          dplyr::filter(
            idNumber %in% analysisSettings$comparatorCohortId
          ) %>%
          dplyr::select(label)
      ),
      targetLabel = unlist(
        analysisSettings$mapTreatments %>%
          dplyr::filter(
            idNumber %in% analysisSettings$treatmentCohortId
          ) %>%
          dplyr::select(label)
      )
    )

  for(i in 1:runSettings$runCmSettings$riskStrata){
    plotList[[i]] <- plotList[[i]] +
      ggplot2::ggtitle(
        paste("Risk stratum", i)
      )
  }

  return(plotList)
}

#' @importFrom dplyr %>%
#' @export
plotPsCombined <- function(outcomeId,
                           analysisSettings,
                           runSettings,
                           plotSecondaryOutcomes = FALSE){

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  ps <- readRDS(
    file.path(
      analysisPath,
      "Estimation",
      outcomeId,
      "ps.rds"
    )
  )

  plotList <- plotPsOverall(
    ps,
    analysisSettings = analysisSettings,
    runSettings = runSettings
  )

  saveDir <- file.path(
    analysisPath,
    "shiny",
    "data",
    "Estimation",
    outcomeId
  )

  if(!dir.exists(saveDir)){
    dir.create(
      saveDir,
      recursive = TRUE
    )
  }

  saveRDS(
    plotList,
    file.path(
      saveDir,
      paste0(
        paste("psPlotList",
              outcomeId,
              sep = "_"),
        ".rds"
      )
    )
  )

  if(plotSecondaryOutcomes){
    predLoc <- which(analysisSettings$outcomeIds == outcomeId)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- compareOutcomes[compareOutcomes != outcomeId]

    for(compareOutcome in compareOutcomes){
      dir <- file.path(
        analysisPath,
        "Estimation",
        outcomeId,
        compareOutcome
      )
      ps <- readRDS(
        file.path(
          dir,
          "ps.rds"
        )
      )

      plotList <- plotPsOverall(
        ps,
        analysisSettings = analysisSettings,
        runSettings = runSettings
      )
      saveRDS(
        plotList,
        file.path(
          saveDir,
          paste0(
            paste("psPlotList",
                  compareOutcome,
                  sep = "_"),
            ".rds"
          )
        )
      )
    }
  }
}

#' @export
plotPsAnalysis <- function(analysisSettings,
                           runSettings,
                           plotSecondaryOutcomes = FALSE,
                           threads = 1){
  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  cluster <- ParallelLogger::makeCluster(threads)
  ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation"))


  dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                        x = predictOutcomes,
                                        fun = plotPsCombined,
                                        analysisSettings = analysisSettings,
                                        runSettings = runSettings,
                                        plotSecondaryOutcomes = plotSecondaryOutcomes)

  ParallelLogger::stopCluster(cluster)
}






#' @importFrom dplyr %>%
#' @export
plotCovariateBalanceWeighted <- function(population,
                                         cohortMethodData){

  pops <- population %>%
    dplyr::group_by(treatment) %>%
    dplyr::summarise(n = n(),
                     nWeighted = sum(weights))

  covariates <- cohortMethodData$covariates[]
  tt <- covariates %>%
    dplyr::filter(rowId %in% population$rowId) %>%
    dplyr::left_join(population %>%
                       dplyr::select(c("rowId", "treatment", "weights")))

  test = tt %>%
    dplyr::group_by(treatment, covariateId) %>%
    dplyr::summarise(covariateSum = sum(covariateValue)) %>%
    dplyr::left_join(pops) %>%
    dplyr::mutate(unweighted = covariateSum/n,
                  weighted = covariateSum/nWeighted,
                  sdWeighted = weighted*(1 - weighted)/2,
                  sdUnweighted = unweighted*(1 - unweighted)/2) %>%
    as.data.frame()

  t1 <- test %>%
    dplyr::mutate(treatment = ifelse(treatment == 1, 1, -1)) %>%
    dplyr::group_by(covariateId) %>%
    dplyr::summarise(beforeWeighting = 100*abs(sum(unweighted*treatment))/sqrt(sum(sdUnweighted))) %>%
    as.data.frame()
  t2 <- test %>%
    dplyr::mutate(treatment = ifelse(treatment == 1, 1, -1)) %>%
    dplyr::group_by(covariateId) %>%
    dplyr::summarise(afterWeighting = 100*abs(sum(weighted*treatment))/sqrt(sum(sdWeighted))) %>%
    as.data.frame()
  t1 %>%
    dplyr::left_join(t2) %>%
    ggplot2::ggplot(ggplot2::aes(x = beforeWeighting, y = afterWeighting)) +
    ggplot2::geom_point(size = .5) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::geom_hline(yintercept = 10, linetype = 2, color = "red") +
    ggplot2::xlab(label = "Before weighting") +
    ggplot2::ylab(label = "After weighting")
}



#' @export
plotCovariateBalanceOverall <- function(ps,
                                        cohortMethodData,
                                        analysisSettings,
                                        runSettings){

  plotList <-
    lapply(
      ps,
      plotCovariateBalanceWeighted,
      cohortMethodData = cohortMethodData
    )

  for(i in 1:runSettings$runCmSettings$riskStrata){
    plotList[[i]] <- plotList[[i]] +
      ggplot2::ggtitle(
        paste("Risk stratum", i)
      )
  }

  return(plotList)
}


#' @export
plotCovariateBalanceCombined <- function(outcomeId,
                                         analysisSettings,
                                         runSettings,
                                         getDataSettings,
                                         plotSecondaryOutcomes = FALSE){

cohortMethodData <-
  CohortMethod::loadCohortMethodData(
    file = getDataSettings$cohortMethodDataFolder
  )
  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )
  ps <- readRDS(
    file.path(
      analysisPath,
      "Estimation",
      outcomeId,
      "ps.rds"
    )
  )
  ps <- lapply(
    ps,
    createIPW,
    weightsType = runSettings$runCmSettings$effectEstimationSettings$weightsType,
    useStabilizedWeights = runSettings$runCmSettings$effectEstimationSettings$useStabilizedWeights,
    truncationLevels = runSettings$runCmSettings$effectEstimationSettings$truncationLevels
  )
  plotList <-
    plotCovariateBalanceOverall(
      ps,
      cohortMethodData = cohortMethodData,
      analysisSettings = analysisSettings,
      runSettings = runSettings
    )

  saveDir <- file.path(
    analysisPath,
    "shiny",
    "data",
    "Estimation",
    outcomeId
  )

  if(!dir.exists(saveDir)){
    dir.create(
      saveDir,
      recursive = TRUE
    )
  }

  saveRDS(
    plotList,
    file.path(
      saveDir,
      paste0(
        paste("balancePlotList",
              outcomeId,
              sep = "_"),
        ".rds"
      )
    )
  )
  if (plotSecondaryOutcomes) {
    predLoc <- which(analysisSettings$outcomeIds == outcomeId)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- compareOutcomes[compareOutcomes != outcomeId]

    for (compareOutcome in compareOutcomes) {
      dir <- file.path(
        analysisPath,
        "Estimation",
        outcomeId,
        compareOutcome
      )
      ps <- readRDS(
        file.path(
          dir,
          "ps.rds"
        )
      )
      ps <- lapply(
        ps,
        createIPW,
        weightsType = runSettings$runCmSettings$effectEstimationSettings$weightsType,
        useStabilizedWeights = runSettings$runCmSettings$effectEstimationSettings$useStabilizedWeights,
        truncationLevels = runSettings$runCmSettings$effectEstimationSettings$truncationLevels
      )
      plotList <- plotCovariateBalanceOverall(
        ps,
        cohortMethodData = cohortMethodData,
        analysisSettings = analysisSettings,
        runSettings = runSettings
      )
      saveRDS(
        plotList,
        file.path(
          saveDir,
          paste0(
            paste(
              "balancePlotList",
              compareOutcome,
              sep = "_"
            ),
            ".rds"
          )
        )
      )
    }
  }
}



#' @export
plotCovariateBalanceAnalysis <- function(analysisSettings,
                                         runSettings,
                                         getDataSettings,
                                         plotSecondaryOutcomes = FALSE,
                                         threads = 1){
  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  cluster <- ParallelLogger::makeCluster(threads)
  ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation"))


  dummy <- ParallelLogger::clusterApply(cluster = cluster,
                                        x = predictOutcomes,
                                        fun = plotCovariateBalanceCombined,
                                        analysisSettings = analysisSettings,
                                        runSettings = runSettings,
                                        getDataSettings = getDataSettings,
                                        plotSecondaryOutcomes = plotSecondaryOutcomes)

  ParallelLogger::stopCluster(cluster)
}


