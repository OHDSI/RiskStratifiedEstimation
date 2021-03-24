# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of RiskStratifiedEstimation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# @author Observational Health Data Sciences and Informatics
# @author Alexandros Rekkas
# @author Peter Rijnbeek


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
#' @importFrom stats binom.test density filter quantile sd weights
#' @export

createOverallResults <- function(analysisSettings) {

  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  negativeControls <- analysisSettings$negativeControlOutcomes

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

  if (!dir.exists(saveDir)) {
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
  negativeControlsRelative <- data.frame(
    HR = numeric(),
    lower = numeric(),
    upper = numeric(),
    seLogRr = numeric(),
    riskStratum = character(),
    analysisType = character(),
    stratOutcome = numeric(),
    estOutcome = numeric(),
    treatment = numeric(),
    comparator = numeric(),
    database = character()
  )

  predictionPopulations <- c(
    "EntirePopulation",
    "Matched",
    "Treatment",
    "Comparator"
  )

  for (predictOutcome in predictOutcomes) {
    for (predictionPopulation in predictionPopulations) {
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
        )
      saveRDS(
        data,
        file.path(
          saveDir,
          paste0(
            paste(
              "auc",
              predictionPopulation,
              analysisSettings$analysisId,
              analysisSettings$databaseName,
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
          comparatorId = analysisSettings$comparatorCohortId
        ) %>%
        as.data.frame() %>%
        saveRDS(
          file.path(
            saveDir,
            paste0(
              paste(
                "calibration",
                predictionPopulation,
                analysisSettings$analysisId,
                analysisSettings$databaseName,
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
      treatment = analysisSettings$treatmentCohortId,
      comparator = analysisSettings$comparatorCohortId
    )
    cases <- rbind(cases, casesResult)

    predLoc <- which(analysisSettings$outcomeIds == predictOutcome)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- compareOutcomes[compareOutcomes != predictOutcome]
    # compareOutcomes <- compareOutcomes[!compareOutcomes %in% failedAnalyses]
    compareOutcomes <- sort(
      compareOutcomes[compareOutcomes != predictOutcome]
    )

    if (length(compareOutcomes) != 0) {
      for (compareOutcome in compareOutcomes) {
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

        if (!is.character(absoluteResult)) {
          absoluteResult <- data.frame(
            absoluteResult,
            stratOutcome = predictOutcome,
            estOutcome = compareOutcome,
            database = analysisSettings$databaseName,
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

        if (!is.character(relativeResult)) {

          relativeResult <- data.frame(
            relativeResult,
            stratOutcome = predictOutcome,
            estOutcome = compareOutcome,
            database = analysisSettings$databaseName,
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

        if (!is.character(casesResult)) {
          casesResult <- data.frame(
            casesResult,
            stratOutcome = predictOutcome,
            estOutcome = compareOutcome,
            database = analysisSettings$databaseName,
            treatment = analysisSettings$treatmentCohortId,
            comparator = analysisSettings$comparatorCohortId
          )
          cases <- rbind(cases, casesResult)

        }
      }
    }

    if (!is.null(negativeControls)) {
      for (i in seq_along(negativeControls)) {
        tmp <- tryCatch(
          {
            readRDS(
              file = file.path(
                analysisSettings$saveDirectory,
                analysisSettings$analysisId,
                "Estimation",
                predictOutcome,
                negativeControls[i],
                "relativeRiskReduction.rds"
              )
            )
          },
          error = function(e) {
            e$message
          }
        )
        if (!is.character(tmp) & nrow(tmp) != 0) {
          negativeControlsRelative <- rbind(
            negativeControlsRelative,
            data.frame(
              tmp,
              stratOutcome = predictOutcome,
              estOutcome = negativeControls[i],
              treatment = analysisSettings$treatmentCohortId,
              comparator = analysisSettings$comparatorCohortId,
              database = analysisSettings$databaseName
            )
          )
        }
      }
      saveRDS(
        negativeControlsRelative,
        file = file.path(
          analysisSettings$saveDirectory,
          analysisSettings$analysisId,
          "shiny",
          "negativeControls.rds"
        )
      )
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
    analysis_label = analysisSettings$analysisLabels,
    treatment_id = analysisSettings$treatmentCohortId,
    comparator_id = analysisSettings$comparatorCohortId,
    row.names = NULL
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



#' @importFrom dplyr %>%
#' @export
mergeTempFiles <- function(
  path,
  outcomeId,
  fileName
) {

  if (!missing(outcomeId)) {
    path <- file.path(
      path,
      outcomeId
    )
  }

  if (!dir.exists(path)) return()

  files <- list.files(
    path = path,
    pattern = paste(
      "temp",
      fileName,
      sep = "_"
    ),
    full.names = TRUE
  )

  files %>%
    lapply(readRDS) %>%
    dplyr::bind_rows() %>%
    saveRDS(
      file.path(
        path,
        paste(
          fileName,
          "rds",
          sep = "."
        )
      )
    )

  file.remove(files)

}



#' Absolute risk reduction
#'
#' Calculates absolute risk reduction based on the Kaplan-Meier estimates within risk strata
#'
#' @param population         The study population generated by \code{\link[CohortMethod]{matchOnPs}} when using
#'                           propensity score matching or by \code{\link[CohortMethod]{stratifyByPs}} when stratifying
#'                           on the propensity score. In case of inverse probability of treatment weighting approach,
#'                           it is a datframe with a \code{weights} column.
#' @param timePoint          The time at which the absolute risk difference is estimated
#' @param psMethod           Can be one of "matchOnPs", "stratifyByPs" or "inversePtWeighted".
#'
#' @return                   A dataframe with the absolute risk-stratum specific absolute risk difference estimates,
#'                           along with 95 percent confidence interval.
#'
#' @export

absoluteRiskReduction <- function(population,
                                  timePoint,
                                  psMethod){

  population <- as.data.frame(
    population
  )
  population$event <- ifelse(
    is.na(population$daysToEvent),
    yes = 0,
    no = 1
  )

  population$S <- survival::Surv(
    population$survivalTime,
    population$event
  )

  kaplanMeier <-  survival::survfit(
    S ~ treatment,
    data = population
  )

  if (psMethod == "matchOnPs")
  {

    summaryKM <- summary(
      kaplanMeier,
      times = timePoint
    )

    standardError <- sqrt(
      sum(
        summaryKM$std.err^2
      )
    )

    arr <- diff(
      summaryKM$surv
    )

    res <- c(
      arr,
      arr - 1.96*standardError,
      arr + 1.96*standardError
    )

  }
  else if (psMethod == "stratifyByPs")
  {
    kaplanMeier <- list()
    kk <- sort(
      unique(
        population$stratumId
      )
    )

    for (i in kk)
    {
      kaplanMeier[[i]] <- survival::survfit(
        S ~ treatment,
        data = subset(
          population,
          stratumId == i
        )
      )
    }

    summaryKMList <- lapply(
      kaplanMeier,
      summary,
      times = timePoint
    )

    arrList <- lapply(
      summaryKMList,
      getAbsoluteDifference
    )

    arr <- mean(
      unlist(
        arrList
      )
    )

    standardErrors <- lapply(
      summaryKMList,
      getStandadrdError
    )

    pooledStandardError <- sqrt(
      sum(unlist(standardErrors)^2)/25
    )

    res <- c(
      arr,
      arr - 1.96*pooledStandardError,
      arr + 1.96*pooledStandardError
    )
  }

  else if (psMethod == "inversePtWeighted")
  {
    kaplanMeier <-  survival::survfit(
      S ~ treatment,
      data = population,
      weights = weights
    )

    summaryKM <- summary(
      kaplanMeier,
      times = timePoint
    )

    standardError <- sqrt(
      sum(summaryKM$std.err^2)
    )

    arr <- diff(
      summaryKM$surv
    )

    res <- c(
      arr,
      arr - 1.96*standardError,
      arr + 1.96*standardError
    )
  }

  return(res)

}




#' Relative risk reduction
#'
#' Calculates hazard ratios within risk strata.
#' @param model    The model that was used to fit a cox regression model to the data.
#'
#' @return         A dataframe with hazard ratios for treatment effect across risk strata along with 95 percent
#'                 confidence intervals
#'
#' @export

relativeRiskReduction <- function(model){

  if (class(model) == "OutcomeModel")
  {
    return(
      unlist(
        c(
          exp(
            model$outcomeModelTreatmentEstimate[1:3]
          ),
          model$outcomeModelTreatmentEstimate[4]
        )
      )
    )
  }
  else
  {
    return(
      summary(model)$conf.int[c(1, 3:4)]
    )
  }

}




#' @export
createMapMatrix <- function(
  riskPredictions,
  analysis
) {

  if (analysis$riskStratificationMethod == "equal") {
    lengthSequence <- analysis$riskStratificationThresholds + 1
    breaks <- seq(
      from       = 0,
      to         = 1,
      length.out = lengthSequence
    )
    mapMatrix <- riskPredictions %>%
      dplyr::mutate(
        labels = cut(
          value,
          breaks = quantile(
            value,
            breaks = breaks
          ),
          include.lowest = TRUE
        ),
        riskStratum = as.numeric(labels)
      )
  } else if (analysis$riskStratificationMethod == "quantile") {
    mapMatrix <- riskPredictions %>%
      dplyr::mutate(
        labels = cut(
          value,
          breaks = quantile(
            value,
            probs = analysis$riskStratificationThresholds
          ),
          include.lowest = TRUE
        ),
        riskStratum = as.numeric(labels)
      )
  } else if (analysis$riskStratificationMethod == "custom") {
    mapMatrix <- riskPredictions %>%
      dplyr::mutate(
        labels = cut(
          value,
          breaks = analysis$riskStratificationThresholds,
          include.lowest = TRUE
        ),
        riskStratum = as.numeric(labels)
      )
  }

  return(mapMatrix)
}



## Non-exports ##

getCounts <- function(population,
                      timePoint,
                      psMethod){

  population <- as.data.frame(
    population
  )

  population$event <- ifelse(
    is.na(population$daysToEvent),
    yes = 0,
    no = 1
  )

  population$S <- survival::Surv(
    population$survivalTime,
    population$event
  )

  kaplanMeier <-  survival::survfit(
    S ~ treatment,
    data = population
  )

  if (psMethod == "matchOnPs")
  {
    summaryKM <- summary(
      kaplanMeier,
      times = timePoint
    )

    res <- 1 - summaryKM$surv

  }
  else if (psMethod == "stratifyByPs")
  {
    kaplanMeier <- list()
    stratId <- sort(
      unique(
        population$stratumId
      )
    )

    for (i in stratId)
    {
      kaplanMeier[[i]] <- survival::survfit(
        S ~ treatment,
        data = subset(
          population,
          stratumId == i
        )
      )
    }

    summaryKMList <- lapply(
      kaplanMeier,
      summary,
      times = timePoint
    )

    res <- colMeans(
      do.call(
        rbind,
        lapply(
          summaryKMList,
          function(s) {
            1 - s$surv
          }
        )
      )
    )
  }
  else if (psMethod == "inversePtWeighted")
  {
    kaplanMeier <-  survival::survfit(
      S ~ treatment,
      data = population,
      weights = weights
    )

    summaryKM <- summary(
      kaplanMeier,
      times = timePoint
    )

    res <- 1 - summaryKM$surv

  }

  return(res)

}




getStandadrdError <- function(summaryKmList)
{

  sqrt(sum(summaryKmList$std.err^2))
}




getAbsoluteDifference <- function(summaryKMList)
{
  diff(summaryKMList$surv)
}
