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
#' @param mergedCohortTable The table that will contain the computeIncidenceAnalysis <- funcitiogmerged cohorts.
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
      # subjectId,
      rowId,
      propensityScore
    ) %>%
    dplyr::left_join(
      populationCm,
      by = "rowId"
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
  analysisLabels <- analysisSettings$analysisLabels

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
            x = round(observedIncidence * PersonCountAtRisk),
            PersonCountAtRisk,
            alternative = "two.sided",
            conf.level = .95
          )$conf.int[1],
          upper = binom.test(
            x = round(observedIncidence * PersonCountAtRisk),
            PersonCountAtRisk,
            alternative = "two.sided",
            conf.level = .95
          )$conf.int[2],
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
  }

  mergeAbsolute <- function(path, typeOfResult) {

    fileName <- dplyr::case_when(
      typeOfResult == "absolute" ~ "absoluteRiskReduction.rds",
      typeOfResult == "relative" ~ "relativeRiskReduction.rds",
      typeOfResult == "cases"    ~ "cases.rds"
    )
    if (!file.exists(file.path(path, fileName))) {
      return(NULL)
    } else {
      readRDS(
        file.path(
          path,
          fileName
        )
      ) %>%
        dplyr::mutate(
          estOutcome = as.numeric(basename(path))
        ) %>%
        return()
    }
  }

  mergePredictOutcomes <- function(
    predictOutcome,
    label,
    pathToResults,
    typeOfResult,
    analysisSettings,
    isNegativeControl = FALSE
  ) {

    tmpPath <- file.path(
      pathToResults,
      label,
      predictOutcome
    )
    allOutcomes <- as.numeric(
      list.dirs(
        tmpPath,
        full.names = FALSE,
        recursive = FALSE
      )
    )
    negativeControls <- analysisSettings$negativeControlOutcomes
    if (!isNegativeControl) {
      compareOutcomes <- allOutcomes[!allOutcomes %in% negativeControls]
    } else {
      compareOutcomes <- negativeControls
    }
    pathList <- file.path(tmpPath, compareOutcomes)
    tmpAbsolute <- list()
    for (i in seq_along(pathList)) {
      tmpAbsolute[[i]] <- mergeAbsolute(
        path = pathList[i],
        typeOfResult = typeOfResult
      )
    }

    res <- tmpAbsolute %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        stratOutcome = predictOutcome,
        database     = analysisSettings$databaseName,
        treatment    = analysisSettings$treatmentCohortId,
        comparator   = analysisSettings$comparatorCohortId
      )
    return(res)
  }

  relativeTmp <- absoluteTmp <- casesTmp <- list()
  relativeNcTmp <- absoluteNcTmp <- casesNcTmp <- list()
  for (i in seq_along(analysisLabels)) {
    relativeTmp[[i]] <- purrr::map_dfr(
      .x = predictOutcomes,
      .f = mergePredictOutcomes,
      pathToResults = pathToResults,
      label = analysisLabels[i],
      analysisSettings = analysisSettings,
      typeOfResult = "relative"
    )

    closeAllConnections()
    absoluteTmp[[i]] <- purrr::map_dfr(
      .x = predictOutcomes,
      .f = mergePredictOutcomes,
      pathToResults = pathToResults,
      label = analysisLabels[i],
      analysisSettings = analysisSettings,
      typeOfResult = "absolute"
    )

    closeAllConnections()
    casesTmp[[i]] <- purrr::map_dfr(
      .x = predictOutcomes,
      .f = mergePredictOutcomes,
      pathToResults = pathToResults,
      label = analysisLabels[i],
      analysisSettings = analysisSettings,
      typeOfResult = "cases"
    )
    closeAllConnections()

    if (!is.null(negativeControls)) {
      relativeNcTmp[[i]] <- purrr::map_dfr(
        .x = predictOutcomes,
        .f = mergePredictOutcomes,
        pathToResults = pathToResults,
        label = analysisLabels[i],
        analysisSettings = analysisSettings,
        typeOfResult = "relative",
        isNegativeControl = TRUE
      )

      # relativeNcTmp[[i]] <- lapply(
      #   predictOutcomes,
      #   mergePredictOutcomes,
      #   pathToResults = pathToResults,
      #   label = analysisLabels[i],
      #   analysisSettings = analysisSettings,
      #   typeOfResult = "relative",
      #   isNegativeControl = TRUE
      # ) %>%
      #   dplyr::bind_rows()

      closeAllConnections()
    }
  }

  dplyr::bind_rows(relativeTmp) %>%
    dplyr::tibble() %>%
    saveRDS(
      file.path(
        saveDir,
        "mappedOverallRelativeResults.rds"
      )
    )

  dplyr::bind_rows(absoluteTmp) %>%
    dplyr::tibble() %>%
    saveRDS(
      file.path(
        saveDir,
        "mappedOverallAbsoluteResults.rds"
      )
    )

  dplyr::bind_rows(casesTmp) %>%
    dplyr::tibble() %>%
    saveRDS(
      file.path(
        saveDir,
        "mappedOverallCasesResults.rds"
      )
    )

  if (!is.null(negativeControls)) {
    dplyr::bind_rows(relativeNcTmp) %>%
      dplyr::tibble() %>%
      saveRDS(
        file.path(
          saveDir,
          "negativeControls.rds"
        )
      )
  }

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

  dplyr::tibble(
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


mergeTempFiles <- function(
  path,
  fileName
) {
  paths <- list.files(
    path = path,
    full.names = TRUE,
    pattern = paste("tmp", fileName, sep = "_")
  )
  do.call(
    rbind,
    lapply(paths, readRDS)
  ) %>%
    saveRDS(
      file.path(
        path,
        paste0(
          fileName,
          ".rds"
        )
      )
    )
  file.remove(paths)
}


#' @importFrom dplyr %>%
#' @export
mergeFiles <- function(
  path,
  fileName
) {

  if (!dir.exists(path)) return()
  outcomeId <- as.numeric(basename(path))
  readIfExists <- function(path, outcomeId, fileName) {
    fileDir <- file.path(
      path,
      paste0(fileName, ".rds")
    )
    if (file.exists(fileDir)) {
      readRDS(fileDir) %>%
        dplyr::mutate(
          stratOutcome = outcomeId,
          estOutcome   = as.numeric(basename(path))
        )
    }
  }

  outcomeDirs <- list.dirs(
    path,
    recursive = FALSE,
    full.names = TRUE
  )

  file.path(outcomeDirs) %>%
    lapply(
      readIfExists,
      outcomeId = outcomeId,
      fileName  = fileName
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::tibble()
}



#' @importFrom dplyr %>%
#' @export
mergeAnalysisFiles <- function(
  analysisSettings,
  label,
  fileName
) {
  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "Estimation",
    label
  )
  paths <- list.dirs(
    path = analysisPath,
    full.names = TRUE,
    recursive = FALSE
  )
  do.call(
    rbind,
    lapply(paths, mergeFiles, fileName)
  )
}


#' @importFrom dplyr %>%
#' @export
mergeRseeFiles <- function(
  analysisSettings,
  fileName
) {
  analysisPath = file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "Estimation"
  )

  labels <- list.dirs(
    analysisPath,
    full.names = FALSE,
    recursive = FALSE
  )

  do.call(
    rbind,
    lapply(
      labels,
      mergeAnalysisFiles,
      analysisSettings = analysisSettings,
      fileName         = fileName
    )
  )
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

absoluteRiskReduction <- function(
  population,
  timePoint,
  psMethod
) {

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

  failed <- tryCatch(
    {
    if (psMethod == "matchOnPs") {
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
    } else if (psMethod == "stratifyByPs") {
      kaplanMeier <- list()
      kk <- sort(
        unique(
          population$stratumId
        )
      )

      for (i in kk) {
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
        times = timePoint,
        extend = TRUE
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
    } else if (psMethod == "inversePtWeighted") {
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
  },
    error = function(e) print(e)
  )


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

  if (class(model) == "OutcomeModel") {
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
  }  else {
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
    mapMatrix <- analysis$riskStratificationThresholds(riskPredictions)
  }

  return(mapMatrix)
}



## Non-exports ##

getCounts <- function(
  population,
  timePoint,
  psMethod
) {

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

  if (psMethod == "matchOnPs") {
    summaryKM <- summary(
      kaplanMeier,
      times = timePoint,
      extend = TRUE
    )

    res <- 1 - summaryKM$surv

  } else if (psMethod == "stratifyByPs") {
    kaplanMeier <- list()
    stratId <- sort(
      unique(
        population$stratumId
      )
    )

    for (i in stratId) {
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
      times = timePoint,
      extend = TRUE
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
      times = timePoint,
      extend = TRUE
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


directoryCheck <- function(path) {
  if (!dir.exists(path)) {
    dir.create(
      path      = path,
      recursive = TRUE
    )
  }
}

pullPlpSettings <- function(runPlpSettings, outcomeId) {

  res <- NULL
  analyses <- runPlpSettings$analyses
  ll <- lapply(
    runPlpSettings$analyses,
    function(x) lapply(x, unlist, recursive = F)
  )

  for (i in seq_along(analyses)) {
    if (analyses[[i]]$outcomeId == outcomeId) {
      res <- analyses[[i]]
    }
  }

  return(res)
}
