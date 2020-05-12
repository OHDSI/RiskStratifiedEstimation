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

prepareForPlpData <- function(treatmentCohortId,
                              comparatorCohortId,
                              targetCohortId,
                              cohortDatabaseSchema,
                              cohortTable,
                              resultsDatabaseSchema,
                              mergedCohortTable,
                              connectionDetails){

  addTable(connectionDetails,
           resultsDatabaseSchema = resultsDatabaseSchema,
           table = mergedCohortTable)

  connection <- DatabaseConnector::connect(connectionDetails)


  renderedSql <- SqlRender::loadRenderTranslateSql("mergeCohorts.sql",
                                                   packageName = "RiskStratifiedEstimation",
                                                   result_database_schema = resultsDatabaseSchema,
                                                   merged_cohort_table = mergedCohortTable,
                                                   cohort_database_schema = cohortDatabaseSchema,
                                                   cohort_table = cohortTable,
                                                   target_cohort_id = targetCohortId,
                                                   cohort_ids = c(treatmentCohortId, comparatorCohortId),
                                                   dbms = connectionDetails$dbms)

  DatabaseConnector::executeSql(connection, renderedSql)
  DatabaseConnector::disconnect(connection)

}



addTable <- function(connectionDetails,
                     resultsDatabaseSchema,
                     table){

  renderedSql <- SqlRender::loadRenderTranslateSql("createTable.sql",
                                                   packageName = "RiskStratifiedEstimation",
                                                   result_database_schema = resultsDatabaseSchema,
                                                   target_cohort_table = table,
                                                   dbms = connectionDetails$dbms)

  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::executeSql(connection, renderedSql)
  DatabaseConnector::disconnect(connection)

}




#' @importFrom dplyr %>%
switchOutcome <- function(ps,
                           populationCm){


  result <- ps %>%
    dplyr::select(subjectId, propensityScore) %>%
    dplyr::left_join(populationCm,
                     by = "subjectId") %>%
    dplyr::filter(!is.na(survivalTime))
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

  pathToResults <- file.path(analysisSettings$saveDirectory,
                             analysisSettings$analysisId,
                             "Estimation")

  absolute <- data.frame(estimate = numeric(),
                         lower = numeric(),
                         upper = numeric(),
                         riskStratum = character(),
                         stratOutcome = numeric(),
                         estOutcome = numeric(),
                         database = character(),
                         analysisType = character(),
                         treatment = numeric(),
                         comparator = numeric())
  relative <- data.frame(estimate = numeric(),
                         lower = numeric(),
                         upper = numeric(),
                         riskStratum = character(),
                         stratOutcome = numeric(),
                         estOutcome = numeric(),
                         database = character(),
                         analysisType = character(),
                         treatment = numeric(),
                         comparator = numeric())
  cases <- data.frame(riskStratum = character(),
                      stratOutcome = numeric(),
                      estOutcome = numeric(),
                      database = character(),
                      analysisType = character(),
                      treatment = numeric(),
                      comparator = numeric())


  for(predictOutcome in predictOutcomes){
    absoluteResult <- readRDS(file.path(pathToResults,
                                        predictOutcome,
                                        "absoluteRiskReduction.rds")) %>%
      dplyr::rename("estimate" = "ARR")
    absoluteResult <- data.frame(absoluteResult,
                                 stratOutcome = predictOutcome,
                                 estOutcome = predictOutcome,
                                 database = analysisSettings$databaseName,
                                 analysisType = analysisSettings$analysisType,
                                 treatment = analysisSettings$treatmentCohortId,
                                 comparator = analysisSettings$comparatorCohortId)
    absolute <- rbind(absolute, absoluteResult)

    relativeResult <- readRDS(file.path(pathToResults,
                                        predictOutcome,
                                        "relativeRiskReduction.rds")) %>%
      dplyr::rename("estimate" = "HR")
    relativeResult <- data.frame(relativeResult,
                                 stratOutcome = predictOutcome,
                                 estOutcome = predictOutcome,
                                 database = analysisSettings$databaseName,
                                 analysisType = analysisSettings$analysisType,
                                 treatment = analysisSettings$treatmentCohortId,
                                 comparator = analysisSettings$comparatorCohortId)
    relative <- rbind(relative, relativeResult)

    casesResult <- readRDS(file.path(pathToResults,
                                     predictOutcome,
                                     "cases.rds")) %>%
      dplyr::rename("casesComparator" = "comparator") %>%
      dplyr::rename("casesTreatment" = "treatment")
    casesResult <- data.frame(casesResult,
                              stratOutcome = predictOutcome,
                              estOutcome = predictOutcome,
                              database = analysisSettings$databaseName,
                              analysisType = analysisSettings$analysisType,
                              treatment = analysisSettings$treatmentCohortId,
                              comparator = analysisSettings$comparatorCohortId)
    cases <- rbind(cases, casesResult)

    predLoc <- which(analysisSettings$outcomeIds == predictOutcome)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- compareOutcomes[compareOutcomes != predictOutcome]

    if(length(compareOutcomes) != 0){
      for(compareOutcome in compareOutcomes){
        absoluteResult <- readRDS(file.path(pathToResults,
                                            predictOutcome,
                                            compareOutcome,
                                            "absoluteRiskReduction.rds")) %>%
          dplyr::rename("estimate" = "ARR")
        absoluteResult <- data.frame(absoluteResult,
                                     stratOutcome = predictOutcome,
                                     estOutcome = compareOutcome,
                                     database = analysisSettings$databaseName,
                                     analysisType = analysisSettings$analysisType,
                                     treatment = analysisSettings$treatmentCohortId,
                                     comparator = analysisSettings$comparatorCohortId)
        absolute <- rbind(absolute, absoluteResult)

        relativeResult <- readRDS(file.path(pathToResults,
                                            predictOutcome,
                                            compareOutcome,
                                            "relativeRiskReduction.rds")) %>%
          dplyr::rename("estimate" = "HR")
        relativeResult <- data.frame(relativeResult,
                                     stratOutcome = predictOutcome,
                                     estOutcome = compareOutcome,
                                     database = analysisSettings$databaseName,
                                     analysisType = analysisSettings$analysisType,
                                     treatment = analysisSettings$treatmentCohortId,
                                     comparator = analysisSettings$comparatorCohortId)
        relative <- rbind(relative, relativeResult)

        casesResult <- readRDS(file.path(pathToResults,
                                         predictOutcome,
                                         compareOutcome,
                                         "cases.rds")) %>%
          dplyr::rename("casesComparator" = "comparator") %>%
          dplyr::rename("casesTreatment" = "treatment")
        casesResult <- data.frame(casesResult,
                                  stratOutcome = predictOutcome,
                                  estOutcome = compareOutcome,
                                  database = analysisSettings$databaseName,
                                  analysisType = analysisSettings$analysisType,
                                  treatment = analysisSettings$treatmentCohortId,
                                  comparator = analysisSettings$comparatorCohortId)
        cases <- rbind(cases, casesResult)
      }
    }

    saveDir <- file.path(analysisSettings$saveDirectory,
                         analysisSettings$analysisId,
                         "shiny",
                         "data")
    outputDir <- file.path(saveDir, "Prediction", predictOutcome)
    if(!dir.exists(outputDir)){
      dir.create(outputDir, recursive = T)
    }

    predictionEvaluationDir <- file.path(analysisSettings$saveDirectory,
                                         analysisSettings$analysisId,
                                         "Prediction",
                                         predictOutcome,
                                         analysisSettings$analysisId,
                                         "evaluation")
    listFiles <- list.files(predictionEvaluationDir)
    file.copy(file.path(predictionEvaluationDir,
                        listFiles),
              outputDir)
  }

  absolute %>%
    saveRDS(file.path(saveDir, "mappedOverallAbsoluteResults.rds"))

  relative %>%
    saveRDS(file.path(saveDir, "mappedOverallRelativeResults.rds"))

  cases %>%
    saveRDS(file.path(saveDir, "mappedOverallCasesResults.rds"))

  saveRDS(analysisSettings$mapOutcomes,
          file.path(saveDir,
                    "mapOutcomes.rds"))
  saveRDS(analysisSettings$mapTreatments,
          file.path(saveDir,
                    "mapTreatments.rds"))

  return(NULL)
}

