analysisDirList <- list()
databaseOptions <- c()
analysisTypeOptions <- c()
mapOutcomes <- data.frame(
  idNumber = numeric(),
  label = character()
)

processOverallAbsoluteResults <- function(analysisDirList,
                                          saveDirectory){
  analysisPath <- file.path(
    saveDirectory,
    "Estimation"
  )

  if (!dir.exists(analysisPath)) {
    dir.create(analysisPath, recursive = T)
  }

  mappedOverallAbsoluteResults <- data.frame(
    estimate = numeric(),
    lower = numeric(),
    upper = numeric(),
    riskStratum = character(),
    database = character(),
    analysis = character(),
    treatment = character(),
    comparator = character(),
    stratOutcome = character(),
    estOutcome = character()
  )

  for (i in 1:length(analysisSettingsList)) {
    analysisDir <- file.path(
      analysisSettingsList[[i]]$saveDirectory,
      analysisSettingsList[[i]]$analysisId,
      "Shiny"
    )
    mappedOverallAbsoluteResults <- readRDS(
      file.path(
        analysisDir,
        "data",
        "mappedOverallAbsoluteResults.rds"
      )
    )  %>%
      dplyr::mutate(
        database = as.character(database),
        analysis = as.character(analysis),
        treatment = as.character(treatment),
        comparator = as.character(comparator)
      ) %>%
      dplyr::bind_rows(mappedOverallAbsoluteResults)
  }
  saveRDS(
    mappedOverallAbsoluteResults,
    file.path(
      analysisPath,
      "mappedOverallAbsoluteResults.rds"
    )
  )
  return(NULL)
}

processOverallRelativeResults <- function(analysisDirList,
                                          saveDirectory){

  mappedOverallRelativeResults <- data.frame(
    estimate = numeric(),
    lower = numeric(),
    upper = numeric(),
    riskStratum = character(),
    database = character(),
    analysis = character(),
    treatment = character(),
    comparator = character(),
    stratOutcome = character(),
    estOutcome = character()
  )

  analysisPath <- file.path(
    saveDirectory,
    "Estimation"
  )

  if (!dir.exists(analysisPath)) {
    dir.create(analysisPath, recursive = T)
  }

  for (i in 1:length(analysisSettingsList)) {
    analysisDir <- file.path(
      analysisSettingsList[[i]]$saveDirectory,
      analysisSettingsList[[i]]$analysisId,
      "Shiny"
    )
    mappedOverallAbsoluteResults <- readRDS(
      file.path(
        analysisDir,
        "data",
        "mappedOverallRelativeResults.rds"
      )
    )  %>%
      dplyr::mutate(
        database = as.character(database),
        analysis = as.character(analysis),
        treatment = as.character(treatment),
        comparator = as.character(comparator)
      ) %>%
      dplyr::bind_rows(mappedOverallAbsoluteResults)
  }
  saveRDS(
    mappedOverallRelativeResults,
    file.path(
      analysisPath,
      "mappedOverallRelativeResults.rds"
    )
  )
  return(NULL)
}
processOverallCasesResults <- function(analysisDirList,
                                       saveDirectory){

  mappedOverallCasesResults <- data.frame(
    casesComparator = numeric(),
    casesTreatment = numeric(),
    riskStratum = character(),
    database = character(),
    analysis = character(),
    treatment = character(),
    comparator = character(),
    stratOutcome = character(),
    estOutcome = character()
  )

  analysisPath <- file.path(
    saveDirectory,
    "Estimation"
  )

  if (!dir.exists(analysisPath)) {
    dir.create(analysisPath, recursive = T)
  }

  for (i in 1:length(analysisSettingsList)) {
    analysisDir <- file.path(
      analysisSettingsList[[i]]$saveDirectory,
      analysisSettingsList[[i]]$analysisId,
      "Shiny"
    )
    mappedOverallCasesResults <- readRDS(
      file.path(
        analysisDir,
        "data",
        "mappedOverallCasesResults.rds"
      )
    )  %>%
      dplyr::mutate(
        database = as.character(database),
        analysis = as.character(analysis),
        treatment = as.character(treatment),
        comparator = as.character(comparator)
      ) %>%
      dplyr::bind_rows(mappedOverallAbsoluteResults)
  }
  saveRDS(
    mappedOverallCasesResults,
    file.path(
      analysisPath,
      "mappedOverallCasesResults.rds"
    )
  )
  return(NULL)
}




processMaps <- function(analysisSettingsList,
                        saveDirectory) {

  for (i in 1:length(analysisSettingsList)) {
    mapOutcomes <- readRDS(
      file.path(
        analysisDir,
        "data",
        "mapOutcomes.rds"
      )
    ) %>%
      dplyr::full_join(mapOutcomes)

    mapTreatments <- readRDS(
      file.path(
        analysisDir,
        "data",
        "mapTreatments.rds"
      )
    ) %>%
      dplyr::full_join(mapTreatments)
  }

  saveRDS(
    mapOutcomes,
    file.path(
      saveDirectory,
      "mapOutcomes.rds"

    )
  )
  saveRDS(
    mapTreatments,
    file.path(
      saveDirectory,
      "Estimation",
      "mapTreatments.rds"
    )
  )
  return(NULL)
}







for (i in 1:length(analysisSettingsList)) {
  analysisDir <- file.path(
    analysisSettingsList[[i]]$saveDirectory,
    analysisSettingsList[[i]]$analysisId,
    "Shiny"
  )

  databaseOptions <- c(
    databaseOptions,
    analysisSettingsList[[i]]$databaseName
  )
  databaseOptions <- unique(databaseOptions)

  analysisTypeOptions <- c(
    analysisTypeOptions,
    analysisSettingsList[[i]]$analysisType
  )
  analysisTypeOptions <- unique(analysisTypeOptions)

  mapOutcomes <- readRDS(
    file.path(
      analysisDir,
      "data",
      "mapOutcomes.rds"
    )
  ) %>%
    dplyr::full_join(mapOutcomes)

  mapTreatments <- readRDS(
    file.path(
      analysisDir,
      "data",
      "mapTreatments.rds"
    )
  ) %>%
    dplyr::full_join(mapTreatments)

  mappedOverallRelativeResults <- readRDS(
    file.path(
      analysisDir,
      "data",
      "mappedOverallRelativeResults.rds"
    )
  ) %>%
    dplyr::mutate(
      database = as.character(database),
      analysis = as.character(analysis),
      treatment = as.character(treatment),
      comparator = as.character(comparator)
    )  %>%
    dplyr::bind_rows(mappedOverallRelativeResults)

  mappedOverallAbsoluteResults <- readRDS(
    file.path(
      analysisDir,
      "data",
      "mappedOverallAbsoluteResults.rds"
    )
  )  %>%
    dplyr::mutate(
      database = as.character(database),
      analysis = as.character(analysis),
      treatment = as.character(treatment),
      comparator = as.character(comparator)
    ) %>%
    dplyr::bind_rows(mappedOverallAbsoluteResults)

  mappedOverallCasesResults <- readRDS(
    file.path(
      analysisDir,
      "data",
      "mappedOverallCasesResults.rds"
    )
  )  %>%
    dplyr::mutate(
      database = as.character(database),
      analysis = as.character(analysis),
      treatment = as.character(treatment),
      comparator = as.character(comparator)
    ) %>%
    dplyr::bind_rows(mappedOverallCasesResults)

}
