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


#' @import data.table
computeMeansPerGroupFast <- function(cohorts, covariates) {

  hasStrata <- "stratumId" %in% colnames(cohorts)
  # setDT(cohorts)

  if (hasStrata) {

    byVar <- c(
      "stratumId",
      "treatment"
    )
    stratumSize <- cohorts[
      ,
      .N,
      by = byVar
      ]
  }

  if (hasStrata && any(stratumSize %>% dplyr::pull(.data$N) > 1))
  {

    keyCols <- c(
      "stratumId",
      "treatment"
    )

    data.table::setkeyv(
      cohorts,
      keyCols
    )

    data.table::setkeyv(
      stratumSize,
      keyCols
    )

    w <- stratumSize[
      ,
      weight := 1/N
      ][
        cohorts,
        nomatch = 0
        ][
          ,
          list(
            rowId,
            treatment,
            weight
          )
          ]

    wSum <- w[
      ,
      list(
        wSum = sum(weight)
      ),
      by = treatment
      ]

    data.table::setkey(wSum, treatment)
    data.table::setkey(w, treatment)

    w <- w[
      wSum,
      nomatch = 0
      ][
        ,
        weight := weight/wSum
        ][
          ,
          list(
            rowId,
            treatment,
            weight
          )
          ]

    sumW <- 1

    data.table::setkey(covariates, rowId)
    data.table::setkey(w, rowId)

    result <- covariates[
      w,
      nomatch = 0
      ][
        ,
        list(
          sum = sum(
            covariateValue,
            na.rm = TRUE
          ),
          mean = sum(
            weight*covariateValue,
            na.rm = TRUE
          ),
          sumSqr = sum(
            weight*covariateValue^2,
            na.rm = TRUE
          ),
          sumWSqr = sum(
            weight^2,
            na.rm = TRUE
          )
        ),
        by = list(
          covariateId,
          treatment
        )
        ][
          ,
          sd := sqrt(abs(sumSqr - mean^2) * sumW/(sumW^2 - sumWSqr))
          ][
            ,
            list(
              covariateId,
              treatment,
              sum,
              mean,
              sd
            )
            ]

  } else {
    data.table::setkey(covariates, rowId)

    cohortCounts <- cohorts[
      ,
      .N,
      by = treatment
      ]

    data.table::setkey(cohortCounts, treatment)
    data.table::setkey(cohorts, rowId)

    result <- covariates[
      cohorts,
      nomatch = 0
      ][
        ,
        list(
          sum = sum(
            covariateValue,
            na.rm = TRUE
          ),
          sumSqr = sum(
            covariateValue^2,
            na.rm = TRUE
          )
        ),
        by = list(
          covariateId,
          treatment
        )
        ]

    data.table::setkey(result, treatment)

    result <- result[
      cohortCounts,
      nomatch = 0
      ][
        ,
        `:=`(
          sd = sqrt((sumSqr - (sum^2/N))/N),
          mean = sum/N
        )
        ][
          ,
          list(
            covariateId,
            treatment,
            sum,
            mean,
            sd
          )
          ]
  }

  target <- result[
    treatment == 1
    ][
      ,
      list(
        covariateId = covariateId,
        sumTarget = sum,
        meanTarget = mean,
        sdTarget = sd
      )
      ]

  comparator <- result[
    treatment == 0
    ][
      ,
      list(
        covariateId = covariateId,
        sumComparator = sum,
        meanComparator = mean,
        sdComparator = sd
      )
      ]

  data.table::setkey(target, covariateId)
  data.table::setkey(comparator, covariateId)
  dropCols <- c(
    "sdTarget",
    "sdComparator"
  )

  result <- merge(
    target,
    comparator,
    all = TRUE
  )[
    ,
    sd := sqrt((sdTarget^2 + sdComparator^2)/2)
    ][
      ,
      !..dropCols,
      with = FALSE
      ]

  data.table::setDF(result)

  return(result)
}


#' @title                      Compute covariate balance
#' @description                Compute covariate balance before and after
#'                             adjustment with the propensity scores.
#' @param population           A data frame containing the people that are
#'                             remaining after matching and/or trimming.
#' @param cohorts              The cohorts of a \code{CohortMethodData} object.
#' @param covariates           The covariates of a \code{CovariateData} object.
#' @param covariateRef         The covariate reference of a \code{CovariateData}
#'                             object.
#' @param subgroupCovariateId  Optional: a covariate ID of a binary covariate that
#'                             indicates a subgroup of interest. Both the before
#'                             and after populations will be restricted to this
#'                             subgroup before computing covariate balance.
#' @return                     A tibble with the covariate balance before and after
#'                             matching/trimming.
#' @export
computeCovariateBalance <- function(
  population,
  cohorts,
  covariates,
  covariateRef,
  subgroupCovariateId = NULL
) {
  ParallelLogger::logTrace("Computing covariate balance")
  start <- Sys.time()

  if (!is.null(subgroupCovariateId)) {
    subGroupCovariate <- covariates %>%
      dplyr::filter(.data$covariateId == subgroupCovariateId)

    if (nrow(subGroupCovariate) == 0) {
      stop("Cannot find covariate with ID ", subgroupCovariateId)
    }

    selectCols <- c(
      "rowId",
      "treatment"
    )

    tempCohorts <- cohorts[
      ,
      selectCols
      ]

    if (nrow(tempCohorts) == 0)
    {
      stop("Cannot find covariate with ID ", subgroupCovariateId, " in population before matching/trimming")
    }

    sumTreatment <- sum(tempCohorts$treatment)

    if (sumTreatment == 0 || sumTreatment == nrow(tempCohorts))
    {
      stop("Subgroup population before matching/trimming doesn't have both target and comparator")
    }

    tempCohortsAfterMatching <- population %>%
      dplyr::filter(.data$rowId %in% subGroupCovariate$rowId) %>%
      as.data.frame()

    if (nrow(tempCohortsAfterMatching) == 0)
    {
      stop("Cannot find covariate with ID ", subgroupCovariateId, " in population after matching/trimming")
    }

    sumTreatment <- sum(tempCohortsAfterMatching$treatment)
    if (sumTreatment == 0 || sumTreatment == nrow(tempCohortsAfterMatching))
    {
      stop("Subgroup population before matching/trimming doesn't have both target and comparator")
    }

    cohortMethodData$tempCohorts <- tempCohorts %>%
      dplyr::select(.data$rowId, .data$treatment)

    cohortMethodData$tempCohortsAfterMatching <- tempCohortsAfterMatching %>%
      dplyr::select(.data$rowId, .data$treatment, .data$stratumId)
  }
  else
  {
    tempCohorts <- cohorts[
      ,
      list(
        rowId,
        treatment
      )
    ]

    tempCohortsAfterMatching <- data.table::setDT(
      population %>%
        dplyr::select(.data$rowId, .data$treatment, .data$stratumId)
    )
  }
  beforeMatching <- computeMeansPerGroupFast(tempCohorts, covariates)
  afterMatching <- computeMeansPerGroupFast(tempCohortsAfterMatching, covariates)

  colnames(beforeMatching)[colnames(beforeMatching) == "meanTarget"] <- "beforeMatchingMeanTarget"
  colnames(beforeMatching)[colnames(beforeMatching) == "meanComparator"] <- "beforeMatchingMeanComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumTarget"] <- "beforeMatchingSumTarget"
  colnames(beforeMatching)[colnames(beforeMatching) == "sumComparator"] <- "beforeMatchingSumComparator"
  colnames(beforeMatching)[colnames(beforeMatching) == "sd"] <- "beforeMatchingSd"
  colnames(afterMatching)[colnames(afterMatching) == "meanTarget"] <- "afterMatchingMeanTarget"
  colnames(afterMatching)[colnames(afterMatching) == "meanComparator"] <- "afterMatchingMeanComparator"
  colnames(afterMatching)[colnames(afterMatching) == "sumTarget"] <- "afterMatchingSumTarget"
  colnames(afterMatching)[colnames(afterMatching) == "sumComparator"] <- "afterMatchingSumComparator"
  colnames(afterMatching)[colnames(afterMatching) == "sd"] <- "afterMatchingSd"

  balance <- beforeMatching %>%
    dplyr::full_join(afterMatching, by = "covariateId") %>%
    dplyr::inner_join(covariateRef, by = "covariateId") %>%
    dplyr::mutate(
      beforeMatchingStdDiff = (.data$beforeMatchingMeanTarget - .data$beforeMatchingMeanComparator)/.data$beforeMatchingSd,
      afterMatchingStdDiff = (.data$afterMatchingMeanTarget - .data$afterMatchingMeanComparator)/.data$afterMatchingSd
    )

  balance$beforeMatchingStdDiff[balance$beforeMatchingSd == 0] <- 0
  balance$afterMatchingStdDiff[balance$beforeMatchingSd == 0] <- 0
  balance <- balance[order(-abs(balance$beforeMatchingStdDiff)), ]
  balance <- dplyr::as_tibble(balance)
  delta <- Sys.time() - start
  cat(paste("Computing covariate balance took", signif(delta, 3), attr(delta, "units"), "\n"))
  return(balance)
}


computeCovariateBalanceOverall <- function(
  path,
  stratOutcome,
  analysisType,
  analysisSettings,
  getDataSettings
) {

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  cohortMethodData <- CohortMethod::loadCohortMethodData(
    file = getDataSettings$cohortMethodDataFolder
  )

  cohorts <- data.table::setDT(
    dplyr::collect(
      cohortMethodData$cohorts
    )
  )

  covariates <- data.table::setDT(
    dplyr::collect(
      cohortMethodData$covariates
    )
  )

  covariateRef <- data.table::setDT(
    dplyr::collect(
      cohortMethodData$covariateRef
    )
  )

  estOutcome <- as.numeric(
    basename(
      path
    )
  )

  psFileLocation <- file.path(
    path,
    "ps_analysis.rds"
  )

  if (!file.exists(psFileLocation)) {
    return()
  }

  ps <- readRDS(psFileLocation)

  lapply(
    ps,
    computeCovariateBalance,
    cohorts      = cohorts,
    covariates   = covariates,
    covariateRef = covariateRef,
  ) %>%
    dplyr::bind_rows(
      .id = "riskStratum"
    ) %>%
    dplyr::mutate(
      riskStratum  = paste0("Q", riskStratum),
      database     = analysisSettings$databaseName,
      analysisId   = analysisSettings$analysisId,
      stratOutcome = stratOutcome,
      estOutcome   = estOutcome,
      treatmentId  = analysisSettings$treatmentCohortId,
      comparatorId = analysisSettings$comparatorCohortId,
      analysisType = analysisType
    ) %>%
    dplyr::select(
      riskStratum,
      covariateId,
      covariateName,
      beforeMatchingStdDiff,
      afterMatchingStdDiff,
      database,
      analysisId,
      stratOutcome,
      estOutcome,
      treatmentId,
      comparatorId,
      analysisType
    ) %>%
    saveRDS(
      file = file.path(
        saveDir,
        paste0(
          paste(
            "balance",
            analysisSettings$analysisId,
            analysisSettings$databaseName,
            analysisType,
            analysisSettings$treatmentCohortId,
            analysisSettings$comparatorCohortId,
            stratOutcome,
            estOutcome,
            sep = "_"
          ),
          ".rds"
        )
      )
    )


}

#' @title                  Compute risk stratified covariate balance
#' @description            Computes covariate balance within strata of predicted
#'                         risk for all specified analyses
#' @param analysisSettings The analysis settings. Should be created using
#'                         \code{\link[RiskstratifiedEstimation]{createAnalysisSettings}}
#'
#' @param getDataSettings  The \code{getDataSettings} object with the
#'                         \code{cohortMethodDataFolder} pointing to the location
#'                         where the \code{CohortMethodData} object is stored.
#' @return                 There is no value returned. The covariate balance results
#'                         are stored in the proper locations.
#'
#' @import data.table
#' @export
computeRseeCovariateBalance <- function(
  analysisSettings,
  getDataSettings
) {

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "Estimation"
  )

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  labels <- list.dirs(
    path = analysisPath,
    recursive = FALSE
  )

  cluster <- ParallelLogger::makeCluster(
    analysisSettings$balanceThreads
  )

  ParallelLogger::clusterRequire(
    cluster,
    "RiskStratifiedEstimation"
  )

  for (i in seq_along(labels)) {
    predictOutcomeDirs <- list.dirs(
      path       = labels[i],
      recursive  = FALSE,
      full.names = TRUE
    )

    predictOutcomes <- as.numeric(
      basename(
        predictOutcomeDirs
      )
    )

    for (j in seq_along(predictOutcomes)) {
      compareOutcomeDirs <- list.dirs(
        predictOutcomeDirs[j],
        recursive = FALSE
      )

      dummy <- ParallelLogger::clusterApply(
        cluster          = cluster,
        x                = compareOutcomeDirs,
        fun              = computeCovariateBalanceOverall,
        analysisSettings = analysisSettings,
        getDataSettings  = getDataSettings,
        stratOutcome     = predictOutcomes[j],
        analysisType     = basename(labels[i])
      )
    }
  }
}
