#' @export

rseeViewer <- function(analysisPath){

 appDir <- system.file(
   "shiny",
   package = "RiskStratifiedEstimation"
 )
 # shinySettings <- analysisSettingsList
 .GlobalEnv$shinySettings <- analysisPath

 shiny::runApp(appDir)

 on.exit(
    rm(
       shinySettings,
       env = .GlobalEnv
    )
 )


}


#' @export

prepareMultipleRseeViewer <- function(analysisSettingsList,
                                      saveDirectory){

    outputDir <- file.path(
        saveDirectory,
        "multipleRseeViewerData"
    )
    if (!dir.exists(outputDir)) {
        dir.create(
            outputDir,
            recursive = T
        )
    }

    createAnlysisPath <- function(analysisSettings,
                                  file){
        res <- file.path(
            analysisSettings$saveDirectory,
            analysisSettings$analysisId,
            "shiny",
            "data",
            file
        )
        return(res)
    }

    analysisDirs <- sapply(
        analysisSettingsList,
        createAnlysisPath,
        file = "balance.rds"
    )

    lapply(
        analysisDirs,
        readRDS
    ) %>%
        dplyr::bind_rows() %>%
        saveRDS(
            file.path(
                outputDir,
                "balance.rds"
            )
        )

    analysisDirs <- sapply(
        analysisSettingsList,
        createAnlysisPath,
        file = "psDensity.rds"
    )


    lapply(
        analysisDirs,
        readRDS
    ) %>%
        dplyr::bind_rows() %>%
        saveRDS(
            file.path(
                outputDir,
                "psDensity.rds"
            )
        )

    analysisDirs <- sapply(
        analysisSettingsList,
        createAnlysisPath,
        file = "mappedOverallAbsoluteResults.rds"
    )


    lapply(
        analysisDirs,
        readRDS
    ) %>%
        dplyr::bind_rows() %>%
        saveRDS(
            file.path(
                outputDir,
                "mappedOverallAbsoluteResults.rds"
            )
        )

    analysisDirs <- sapply(
        analysisSettingsList,
        createAnlysisPath,
        file = "mappedOverallRelativeResults.rds"
    )


    lapply(
        analysisDirs,
        readRDS
    ) %>%
        dplyr::bind_rows() %>%
        saveRDS(
            file.path(
                outputDir,
                "mappedOverallRelativeResults.rds"
            )
        )

    analysisDirs <- sapply(
        analysisSettingsList,
        createAnlysisPath,
        file = "mappedOverallCasesResults.rds"
    )


    lapply(
        analysisDirs,
        readRDS
    ) %>%
        dplyr::bind_rows() %>%
        saveRDS(
            file.path(
                outputDir,
                "mappedOverallCasesResults.rds"
            )
        )

    analysisDirs <- sapply(
        analysisSettingsList,
        createAnlysisPath,
        file = "mapOutcomes.rds"
    )

    dfs <- lapply(
        analysisDirs,
        readRDS
    )
    plyr::join_all(
        dfs = dfs,
        type = "full"
    ) %>%
        saveRDS(
            file.path(
                outputDir,
                "mapOutcomes.rds"
            )
        )

    analysisDirs <- sapply(
        analysisSettingsList,
        createAnlysisPath,
        file = "mapTreatments.rds"
    )

    dfs <- lapply(
        analysisDirs,
        readRDS
    )
    plyr::join_all(
        dfs = dfs,
        type = "full"
    ) %>%
        saveRDS(
            file.path(
                outputDir,
                "mapTreatments.rds"
            )
        )

    return(NULL)
}

