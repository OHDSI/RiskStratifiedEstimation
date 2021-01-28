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
) {

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
  analysisSettingsList,
  saveDirectory
) {

  pathList <- list()
  for (i in seq_along(analysisSettingsList)) {
    tmp <- analysisSettingsList[[i]]
    pathList[[i]] <- file.path(
      tmp$saveDirectory,
      tmp$analysisId,
      "shiny"
    )
  }

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
  ) {
    res <- file.path(
      path,
      file
    )
    return(res)
  }

  mergeFun <- function(
    analysisDirs,
    file
  ) {
    lapply(
      analysisDirs,
      readRDS
    ) %>%
      dplyr::bind_rows() %>%
      saveRDS(
        file.path(
          saveDir,
          paste(
            file,
            "rds",
            sep = "."
          )
        )
      )
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

  mergeFun(
    analysisDirs = analysisDirs,
    file = "incidence"
  )

  analysisDirs <- sapply(
    pathList,
    createAnlysisPath,
    file = "incidenceOverall.rds"
  )

  mergeFun(
    analysisDirs = analysisDirs,
    file = "incidenceOverall"
  )

  analysisDirs <- sapply(
    pathList,
    createAnlysisPath,
    file = "predictionPerformance.rds"
  )
  mergeFun(
    analysisDirs = analysisDirs,
    file = "predictionPerformance"
  )

  analysisDirs <- sapply(
    pathList,
    createAnlysisPath,
    file = "mappedOverallResults.rds"
  )
  mergeFun(
    analysisDirs = analysisDirs,
    file = "mappedOverallResults"
  )

  analysisDirs <- sapply(
    pathList,
    createAnlysisPath,
    file = "mappedOverallAbsoluteResults.rds"
  )
  mergeFun(
    analysisDirs = analysisDirs,
    file = "mappedOverallAbsoluteResults"
  )

  analysisDirs <- sapply(
    pathList,
    createAnlysisPath,
    file = "mappedOverallRelativeResults.rds"
  )
  mergeFun(
    analysisDirs = analysisDirs,
    file = "mappedOverallRelativeResults"
  )

  analysisDirs <- sapply(
    pathList,
    createAnlysisPath,
    file = "mappedOverallCasesResults.rds"
  )
  mergeFun(
    analysisDirs = analysisDirs,
    file = "mappedOverallCasesResults"
  )

  negativeControlPathList <- list()
  for (i in seq_along(analysisSettingsList)) {
    tmp <- analysisSettingsList[[i]]
    if (!is.null(tmp$negativeControlOutcomes)) {
      negativeControlPathList <- c(
        negativeControlPathList,
        file.path(
          tmp$saveDirectory,
          tmp$analysisId,
          "shiny"
        )
      )
    }
  }

  if (length(negativeControlPathList) != 0) {
    analysisDirs <- sapply(
      negativeControlPathList,
      createAnlysisPath,
      file = "negativeControls.rds"
    )
    mergeFun(
      analysisDirs = analysisDirs,
      file = "negativeControls"
    )

    analysisDirs <- sapply(
      negativeControlPathList,
      createAnlysisPath,
      file = "mappedOverallResultsNegativeControls.rds"
    )
    mergeFun(
      analysisDirs = analysisDirs,
      file = "mappedOverallResultsNegativeControls"
    )
  }

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
