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


#' @importFrom dplyr %>%
#' @export

prepareMultipleRseeViewer <- function(pathList,
                                      saveDirectory) {

    saveDir <- file.path(
        saveDirectory,
        "multipleRseeAnalyses"
    )

    if (!dir.exists(saveDir)) {
        dir.create(
            saveDir,
            recursive = TRUE
        )
    }

    createAnlysisPath <- function(path,
                                  file){
        res <- file.path(
            path,
            file
        )
        return(res)
    }

    analysisDirs <- sapply(
        pathList,
        createAnlysisPath,
        file = "analyses.rds"
    )

    lapply(
        analysisDirs,
        readRDS
    ) %>%
        plyr::join_all(
            type = "full"
        ) %>%
        saveRDS(
            file.path(
                saveDir,
                "analyses.rds"
            )
        )

    analysisDirs <- sapply(
        pathList,
        createAnlysisPath,
        file = "map_exposures.rds"
    )

    lapply(
        analysisDirs,
        readRDS
    ) %>%
        plyr::join_all(
            type = "full"
        ) %>%
        saveRDS(
            file.path(
                saveDir,
                "map_exposures.rds"
            )
        )

    analysisDirs <- sapply(
        pathList,
        createAnlysisPath,
        file = "map_outcomes.rds"
    )

    lapply(
        analysisDirs,
        readRDS
    ) %>%
        plyr::join_all(
            type = "full"
        ) %>%
        saveRDS(
            file.path(
                saveDir,
                "map_outcomes.rds"
            )
        )

    analysisDirs <- sapply(
        pathList,
        createAnlysisPath,
        file = "incidence.rds"
    )

    lapply(
        analysisDirs,
        readRDS
    ) %>%
        dplyr::bind_rows() %>%
        saveRDS(
            file.path(
                saveDir,
                "incidence.rds"
            )
        )

    analysisDirs <- sapply(
        pathList,
        createAnlysisPath,
        file = "predictionPerformance.rds"
    )

    lapply(
        analysisDirs,
        readRDS
    ) %>%
        dplyr::bind_rows() %>%
        saveRDS(
            file.path(
                saveDir,
                "predictionPerformance.rds"
            )
        )

    analysisDirs <- sapply(
        pathList,
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
                saveDir,
                "mappedOverallAbsoluteResults.rds"
            )
        )

    analysisDirs <- sapply(
        pathList,
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
                saveDir,
                "mappedOverallRelativeResults.rds"
            )
        )
    analysisDirs <- sapply(
        pathList,
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
                saveDir,
                "mappedOverallCasesResults.rds"
            )
        )

    for (path in pathList) {

        filesToCopy <- list.files(
            path,
            pattern = "^auc|^bal|^ps|cal",
            full.names = TRUE
        )

        file.copy(
            filesToCopy,
            saveDir
        )
    }
}
