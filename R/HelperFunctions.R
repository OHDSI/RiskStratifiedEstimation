#' Prepares for the running the PatientLevelPrediction package
#'
#' Prepares for running the PatientLevelPrediction package by merging the treatment and comparator cohorts and defining a new covariate for treatment.
#'
#' @param treatmentCohortId The treatment cohort id
#' @param comparatorCohortId The comparator cohort id
#' @param targetCohortId The id of the merged cohorts
#' @param cohortDatabaseSchema The name of the database schema that is the location where the cohort data used to define the at risk cohort is available
#' @param cohortTable The table that contains the treatment and comparator cohorts.
#' @param resultsDatabaseSchema The name of the database schema to store the new tables. Need to have write access.
#' @param mergedCohortTable The table that will contain the merged cohorts.
#' @param connectionDetails The connection details required to connect to a database.
#'
#' @return Creates the tables resultsDatabaseSchema.mergedCohortTable, resultsDatabaseSchema.attributeDefinitionTable and resultsDatabaseSchema.cohortAttributeTable
#'
#' @export

prepareForPlpData <- function(
  treatmentCohortId,
  comparatorCohortId,
  targetCohortId,
  cohortDatabaseSchema,
  cohortTable,
  resultsDatabaseSchema,
  mergedCohortTable,
  connectionDetails
)
{

  addTable(
    connectionDetails,
    resultsDatabaseSchema = resultsDatabaseSchema,
    table = mergedCohortTable
  )

  connection <- DatabaseConnector::connect(connectionDetails)


  renderedSql <- SqlRender::loadRenderTranslateSql(
    "mergeCohorts.sql",
    packageName = "RiskStratifiedEstimation",
    result_database_schema = resultsDatabaseSchema,
    merged_cohort_table = mergedCohortTable,
    cohort_database_schema = cohortDatabaseSchema,
    cohort_table = cohortTable,
    target_cohort_id = targetCohortId,
    cohort_ids = c(treatmentCohortId, comparatorCohortId),
    dbms = connectionDetails$dbms
  )

  DatabaseConnector::executeSql(connection, renderedSql)
  DatabaseConnector::disconnect(connection)

}



addTable <- function(
  connectionDetails,
  resultsDatabaseSchema,
  table
)
{
  renderedSql <- SqlRender::loadRenderTranslateSql(
    "createTable.sql",
    packageName = "RiskStratifiedEstimation",
    result_database_schema = resultsDatabaseSchema,
    target_cohort_table = table,
    dbms = connectionDetails$dbms
  )

  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::executeSql(connection, renderedSql)
  DatabaseConnector::disconnect(connection)
}




#' @importFrom dplyr %>%
switchOutcome <- function(
  ps,
  populationCm
)
{

  result <- ps %>%
    dplyr::select(
      subjectId,
      propensityScore
    ) %>%
    dplyr::left_join(
      populationCm,
      by = "subjectId"
    ) %>%
    dplyr::filter(
      !is.na(
        survivalTime
      )
    )
  return(result)

}




#' Combines the overall results
#'
#' @param analysisSettings           An R object of type \code{analysisSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createAnalysisSettings}}.
#' @param runSettings                An R object of type \code{runSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createRunSettings}}.
#'
#' @return                          Stores the overall results along with the required data to lauch the shiny
#'                                   application in the `shiny` directory

#' @importFrom dplyr %>%
#' @export

createOverallResults <- function(analysisSettings){

  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  pathToResults <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "Estimation"
  )

  pathToPrediction <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "Prediction"
  )

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  if (!dir.exists(saveDir))
  {
    dir.create(saveDir, recursive = T)
  }

  absolute <- data.frame(
    estimate = numeric(),
    lower = numeric(),
    upper = numeric(),
    riskStratum = character(),
    stratOutcome = numeric(),
    estOutcome = numeric(),
    database = character(),
    analysisType = character(),
    treatment = numeric(),
    comparator = numeric()
  )
  relative <- data.frame(
    estimate = numeric(),
    lower = numeric(),
    upper = numeric(),
    riskStratum = character(),
    stratOutcome = numeric(),
    estOutcome = numeric(),
    database = character(),
    analysisType = character(),
    treatment = numeric(),
    comparator = numeric()
  )
  cases <- data.frame(
    riskStratum = character(),
    stratOutcome = numeric(),
    estOutcome = numeric(),
    database = character(),
    analysisType = character(),
    treatment = numeric(),
    comparator = numeric()
  )

  predictionPopulations <- c(
    "EntirePopulation",
    "Matched",
    "Treatment",
    "Comparator"
  )

  for (predictOutcome in predictOutcomes)
  {

    # failedAnalyses <- analysisSettings$failedAnalyses %>%
    #   dplyr::filter(.$stratOutcome == predictOutcome) %>%
    #   unlist()

    for (predictionPopulation in predictionPopulations)
    {
      prediction <- readRDS(
        file.path(
          pathToPrediction,
          predictOutcome,
          analysisSettings$analysisId,
          predictionPopulation,
          "prediction.rds"
        )
      )
      prediction <- prediction[order(-prediction$value), c("value", "outcomeCount")]
      prediction$sens <- cumsum(prediction$outcomeCount) / sum(prediction$outcomeCount)
      prediction$fpRate <- cumsum(prediction$outcomeCount == 0) / sum(prediction$outcomeCount == 0)
      data <- stats::aggregate(fpRate ~ sens, data = prediction, min)
      data <- stats::aggregate(sens ~ fpRate, data = data, min)
      data <- rbind(data, data.frame(fpRate = 1, sens = 1)) %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          stratOutcome = predictOutcome,
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          analysisType = analysisSettings$analysisType
        )
      saveRDS(
        data,
        file.path(
          saveDir,
         paste0(
           paste(
             "auc",
             predictionPopulation,
             analysisSettings$databaseName,
             analysisSettings$analysisType,
             analysisSettings$treatmentCohortId,
             analysisSettings$comparatorCohortId,
             predictOutcome,
             sep = "_"
           ),
           ".rds"
         )
        )
      )

      calibration <- readRDS(
        file.path(
          pathToPrediction,
          predictOutcome,
          analysisSettings$analysisId,
          predictionPopulation,
          "calibrationData.rds"
        )
      )

      calibration %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          lower = binom.test(
            x = observedIncidence*PersonCountAtRisk,
            PersonCountAtRisk
          )$conf.int[1],
          upper = binom.test(
            x = observedIncidence*PersonCountAtRisk,
            PersonCountAtRisk
          )$conf.int[2]
        ) %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          stratOutcome = predictOutcome,
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          analysisType = analysisSettings$analysisType
        ) %>%
        as.data.frame() %>%
        saveRDS(
          file.path(
            saveDir,
            paste0(
              paste(
                "calibration",
                predictionPopulation,
                analysisSettings$databaseName,
                analysisSettings$analysisType,
                analysisSettings$treatmentCohortId,
                analysisSettings$comparatorCohortId,
                predictOutcome,
                sep = "_"
              ),
              ".rds"
            )
          )
        )


    }

    absoluteResult <- readRDS(
      file.path(
        pathToResults,
        predictOutcome,
        "absoluteRiskReduction.rds"
      )
    ) %>%
      dplyr::rename("estimate" = "ARR")
    absoluteResult <- data.frame(
      absoluteResult,
      stratOutcome = predictOutcome,
      estOutcome = predictOutcome,
      database = analysisSettings$databaseName,
      analysisType = analysisSettings$analysisType,
      treatment = analysisSettings$treatmentCohortId,
      comparator = analysisSettings$comparatorCohortId)
    absolute <- rbind(absolute, absoluteResult
    )

    relativeResult <- readRDS(
      file.path(
        pathToResults,
        predictOutcome,
        "relativeRiskReduction.rds"
      )
    ) %>%
      dplyr::rename("estimate" = "HR")
    relativeResult <- data.frame(
      relativeResult,
      stratOutcome = predictOutcome,
      estOutcome = predictOutcome,
      database = analysisSettings$databaseName,
      analysisType = analysisSettings$analysisType,
      treatment = analysisSettings$treatmentCohortId,
      comparator = analysisSettings$comparatorCohortId
    )
    relative <- rbind(relative, relativeResult)

    casesResult <- readRDS(
      file.path(
        pathToResults,
        predictOutcome,
        "cases.rds"
      )
    ) %>%
      dplyr::rename("casesComparator" = "comparator") %>%
      dplyr::rename("casesTreatment" = "treatment")
    casesResult <- data.frame(
      casesResult,
      stratOutcome = predictOutcome,
      estOutcome = predictOutcome,
      database = analysisSettings$databaseName,
      analysisType = analysisSettings$analysisType,
      treatment = analysisSettings$treatmentCohortId,
      comparator = analysisSettings$comparatorCohortId
    )
    cases <- rbind(cases, casesResult)

    # psFailed <- analysisSettings$failed$ps %>%
    #   dplyr::filter(
    #     .$stratOutcome == predictOutcome
    #   ) %>%
    #   dplyr::select(
    #     "estOutcome"
    #   ) %>%
    #   unlist(
    #     use.names = FALSE
    #   )
    #
    # resFailed <- analysisSettings$failed$results %>%
    #   dplyr::filter(
    #     .$stratOutcome == predictOutcome
    #   ) %>%
    #   dplyr::select(
    #     "estOutcome"
    #   ) %>%
    #   unlist(
    #     use.names = FALSE
    #   )
    #
    # failed <- unique(
    #   c(
    #     psFailed,
    #     resFailed
    #   )
    # )

    predLoc <- which(analysisSettings$outcomeIds == predictOutcome)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- compareOutcomes[compareOutcomes != predictOutcome]
    # compareOutcomes <- compareOutcomes[!compareOutcomes %in% failedAnalyses]
    compareOutcomes <- sort(
      compareOutcomes[compareOutcomes != predictOutcome]
    )
    # predLoc <- which(analysisSettings$outcomeIds == predictOutcome)
    # compLoc <- analysisSettings$analysisMatrix[, predLoc]
    # compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    # compareOutcomes <- compareOutcomes[compareOutcomes != predictOutcome]
    # compareOutcomes <- compareOutcomes[!compareOutcomes %in% failedAnalyses]

    if (length(compareOutcomes) != 0)
    {
      for (compareOutcome in compareOutcomes)
      {
        absoluteResult <- tryCatch(
          {
            absoluteResult <- readRDS(
              file.path(
                pathToResults,
                predictOutcome,
                compareOutcome,
                "absoluteRiskReduction.rds"
              )
            ) %>%
              dplyr::rename("estimate" = "ARR")
          },
          error = function(e)
          {
            e$message
          }
        )

        if (!is.character(absoluteResult))
        {
          absoluteResult <- data.frame(
            absoluteResult,
            stratOutcome = predictOutcome,
            estOutcome = compareOutcome,
            database = analysisSettings$databaseName,
            analysisType = analysisSettings$analysisType,
            treatment = analysisSettings$treatmentCohortId,
            comparator = analysisSettings$comparatorCohortId
          )

          absolute <- rbind(absolute, absoluteResult)
        }

        relativeResult <- tryCatch(
          {
            relativeResult <- readRDS(
              file.path(
                pathToResults,
                predictOutcome,
                compareOutcome,
                "relativeRiskReduction.rds"
              )
            ) %>%
              dplyr::rename("estimate" = "HR")
          },
          error = function(e)
          {
            e$message
          }
        )

        if (!is.character(relativeResult))
        {

          relativeResult <- data.frame(
            relativeResult,
            stratOutcome = predictOutcome,
            estOutcome = compareOutcome,
            database = analysisSettings$databaseName,
            analysisType = analysisSettings$analysisType,
            treatment = analysisSettings$treatmentCohortId,
            comparator = analysisSettings$comparatorCohortId
          )

          relative <- rbind(relative, relativeResult)

        }

        casesResult <- tryCatch(
          {
            casesResult <- readRDS(
              file.path(
                pathToResults,
                predictOutcome,
                compareOutcome,
                "cases.rds"
              )
            ) %>%
              dplyr::rename("casesComparator" = "comparator") %>%
              dplyr::rename("casesTreatment" = "treatment")

          },
          error = function(e){
            e$message
          }
        )

        if (!is.character(casesResult))
        {
          casesResult <- data.frame(
            casesResult,
            stratOutcome = predictOutcome,
            estOutcome = compareOutcome,
            database = analysisSettings$databaseName,
            analysisType = analysisSettings$analysisType,
            treatment = analysisSettings$treatmentCohortId,
            comparator = analysisSettings$comparatorCohortId
          )
          cases <- rbind(cases, casesResult)

        }
      }
    }
  }

  absolute %>%
    saveRDS(file.path(saveDir, "mappedOverallAbsoluteResults.rds"))

  relative %>%
    saveRDS(file.path(saveDir, "mappedOverallRelativeResults.rds"))

  cases %>%
    saveRDS(file.path(saveDir, "mappedOverallCasesResults.rds"))

  analysisSettings$mapOutcomes %>%
    saveRDS(
      file.path(
        saveDir,
        "map_outcomes.rds"
      )
    )

  analysisSettings$mapTreatments %>%
    saveRDS(
      file.path(
        saveDir,
        "map_exposures.rds"
      )
    )
  data.frame(
    analysis_id = analysisSettings$analysisId,
    description = analysisSettings$description,
    database = analysisSettings$databaseName,
    analysis_type = analysisSettings$analysisType,
    treatment_id = analysisSettings$treatmentCohortId,
    comparator_id = analysisSettings$comparatorCohortId
  ) %>%
    saveRDS(
      file.path(
        saveDir,
        "analyses.rds"
      )
    )

  return(NULL)
}




fitMultiplePsModelOverall <- function(
  analysisSettings,
  runSettings,
  getDataSettings,
  populationSettings,
  outcomeIds
)
{

  cluster <- ParallelLogger::makeCluster(
    runSettings$runCmSettings$createPsThreads
  )

  ParallelLogger::clusterRequire(
    cluster,
    c(
      "RiskStratifiedEstimation",
      "CohortMethod"
    )
  )

  dummy <- ParallelLogger::clusterApply(
    cluster = cluster,
    x = outcomeIds,
    fun = fitPsModelOverall,
    getDataSettings = getDataSettings,
    populationSettings = populationSettings,
    analysisSettings = analysisSettings,
    runCmSettings = runSettings$runCmSettings
  )

  ParallelLogger::stopCluster(cluster)

  do.call(
    file.remove,
    args = list(
      list.files(
        getOption(
          "fftempdir"
        ),
        full.names = TRUE
      ),
      showWarnings = FALSE
    )
  )

}

