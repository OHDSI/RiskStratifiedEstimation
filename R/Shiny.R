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


#' @export

rseeViewer <- function(
    analysisPath
)
{

    appDir <- system.file(
        "shiny",
        package = "RiskStratifiedEstimation"
    )
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

prepareMultipleRseeViewer <- function(
    pathList,
    saveDirectory
)
{

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

    createAnlysisPath <- function(
        path,
        file
    )

    {
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
      file = "incidenceOverall.rds"
    )
    lapply(
      analysisDirs,
      readRDS
    ) %>%
      dplyr::bind_rows() %>%
      saveRDS(
        file.path(
          saveDir,
          "incidenceOverall.rds"
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
        file = "mappedOverallResults.rds"
    )
    lapply(
        analysisDirs,
        readRDS
    ) %>%
        dplyr::bind_rows() %>%
        saveRDS(
            file.path(
                saveDir,
                "mappedOverallResults.rds"
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
            pattern = "^overall|^auc|^bal|^ps|cal",
            full.names = TRUE
        )
        file.copy(
            filesToCopy,
            saveDir
        )
    }
}
