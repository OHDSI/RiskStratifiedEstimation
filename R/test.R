#' @importFrom dplyr %>%
#' @export
computeCovariateBalanceWeighted <- function(population,
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
    return()
}


#' @export
computeCovariateBalanceOverall <- function(ps,
                                           cohortMethodData,
                                           analysisSettings,
                                           runSettings){

  covariateBalanceList <-
    lapply(
      ps,
      computeCovariateBalanceWeighted,
      cohortMethodData = cohortMethodData
    )
  for (i in 1:length(covariateBalanceList)) {
    covariateBalanceList[[i]]$riskStratum <- paste0("Q", i)
  }

  covariateBalanceList %>%
    dplyr::bind_rows() %>%
    return()
}

#' @export
computeCovariateBalanceCombined <- function(outcomeId,
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
  covariateBalanceList <-
    computeCovariateBalanceOverall(
      ps,
      cohortMethodData = cohortMethodData,
      analysisSettings = analysisSettings,
      runSettings = runSettings
    ) %>%
    dplyr::mutate(
      database = analysisSettings$databaseName,
      analysisId = analysisSettings$analysisId,
      stratOutcome = outcomeId,
      estOutcome = outcomeId,
      treatmentId = analysisSettings$treatmentCohortId,
      comparatorId = analysisSettings$comparatorCohortId,
      analysisType = analysisSettings$analysisType
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

      covariateBalanceList <- computeCovariateBalanceOverall(
        ps,
        cohortMethodData = cohortMethodData,
        analysisSettings = analysisSettings,
        runSettings = runSettings
      ) %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          stratOutcome = outcomeId,
          estOutcome = compareOutcome,
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          analysisType = analysisSettings$analysisType
        ) %>%
        dplyr::bind_rows(covariateBalanceList)
    }
  }
  return(covariateBalanceList)
}



#' @export
computeCovariateBalanceAnalysis <- function(analysisSettings,
                                            runSettings,
                                            getDataSettings,
                                            plotSecondaryOutcomes = FALSE,
                                            threads = 1){
  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  cluster <- ParallelLogger::makeCluster(threads)
  ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation"))


  res <- ParallelLogger::clusterApply(cluster = cluster,
                                      x = predictOutcomes,
                                      fun = computeCovariateBalanceCombined,
                                      analysisSettings = analysisSettings,
                                      runSettings = runSettings,
                                      getDataSettings = getDataSettings,
                                      plotSecondaryOutcomes = plotSecondaryOutcomes)

  ParallelLogger::stopCluster(cluster)

  analysisPath <-  file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny",
    "data",
    "Estimation"
  )

  if (!dir.exists(analysisPath)) {
    dir.create(analysisPath, recursive = T)
  }

  do.call(dplyr::bind_rows, res) %>%
    saveRDS(
      file.path(
        analysisPath,
        "balance.rds"
      )
    )
  return(NULL)
}


#' @importFrom dplyr %>%
#' @export
psDensityOverall <- function(ps){

  lapply(ps,
         psDensity) %>%
    dplyr::bind_rows(.id = "riskStratum") %>%
    dplyr::mutate(
      riskStratum = paste0(
        "Q",
        riskStratum
      ) %>%
        return()
    )

}

#' @importFrom dplyr %>%
#' @export
psDensityCombined <- function(outcomeId,
                              analysisSettings,
                              secondaryOutcomes = FALSE){

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

  psDensityList <- psDensityOverall(ps) %>%
    dplyr::mutate(
      database = analysisSettings$databaseName,
      analysisId = analysisSettings$analysisId,
      stratOutcome = outcomeId,
      estOutcome = outcomeId,
      treatment = ifelse(
        treatment == 1,
        analysisSettings$treatmentCohortId,
        analysisSettings$comparatorCohortId
      ),
      treatmentId = analysisSettings$treatmentCohortId,
      comparatorId = analysisSettings$comparatorCohortId,
      analysisType = analysisSettings$analysisType
    )

  if (secondaryOutcomes) {
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

      psDensityList <- psDensityOverall(ps) %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          stratOutcome = outcomeId,
          estOutcome = compareOutcome,
          treatment = ifelse(
            treatment == 1,
            analysisSettings$treatmentCohortId,
            analysisSettings$comparatorCohortId
          ),
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          analysisType = analysisSettings$analysisType
        ) %>%
        dplyr::bind_rows(psDensityList)
    }
  }
  return(psDensityList)
}

#' @export
psDensityAnalysis <- function(analysisSettings,
                              secondaryOutcomes = FALSE,
                              threads = 1){
  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  cluster <- ParallelLogger::makeCluster(threads)
  ParallelLogger::clusterRequire(cluster, c("RiskStratifiedEstimation"))


  res <- ParallelLogger::clusterApply(cluster = cluster,
                                        x = predictOutcomes,
                                        fun = psDensityCombined,
                                        analysisSettings = analysisSettings,
                                        secondaryOutcomes = secondaryOutcomes)

  ParallelLogger::stopCluster(cluster)

  analysisPath <-  file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny",
    "data",
    "Estimation"
  )

  if (!dir.exists(analysisPath)) {
    dir.create(analysisPath, recursive = T)
  }
  do.call(dplyr::bind_rows, res) %>%
    saveRDS(
      file.path(
        analysisPath,
        "psDensity.rds"
      )
    )
  return(NULL)
}




#### Non-exports ####

psDensity <- function(population) {

  treatmentDensity <- density(
    population %>%
      dplyr::filter(treatment == 1) %>%
      dplyr::select(preferenceScore) %>%
      unlist()
  )
  comparatorDensity <- density(
    population %>%
      dplyr::filter(treatment == 0) %>%
      dplyr::select(preferenceScore) %>%
      unlist()
  )

  data.frame(
    x = treatmentDensity$x,
    y = treatmentDensity$y,
    treatment = 1
  ) %>%
    dplyr::bind_rows(
      data.frame(
        x = comparatorDensity$x,
        y = comparatorDensity$y,
        treatment = 0
      )
    ) %>%
    return()


}


# ggplot(data = pp, aes(x = x, y = y), fill = 1) +
#   geom_line() +
#   geom_area(mapping=aes(x = x), fill="#9898fb", alpha=1.)
