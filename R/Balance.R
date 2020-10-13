# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
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
# @author Patrick Ryan
# @author Marc Suchard
# @author Martijn Schuemie


#' @import data.table
#' @export
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

  if (hasStrata && any(stratumSize %>% pull(.data$N) > 1))
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

  }
  else
  {
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


#' @import data.table
#' @export
computeCovariateBalance4 <- function(population, cohorts, covariates, covariateRef, subgroupCovariateId = NULL) {
  ParallelLogger::logTrace("Computing covariate balance")
  start <- Sys.time()

  if (!is.null(subgroupCovariateId)) {
    subGroupCovariate <- covariates %>%
      filter(.data$covariateId == subgroupCovariateId)

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
      filter(.data$rowId %in% subGroupCovariate$rowId) %>%
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
      select(.data$rowId, .data$treatment)

    cohortMethodData$tempCohortsAfterMatching <- tempCohortsAfterMatching %>%
      select(.data$rowId, .data$treatment, .data$stratumId)
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

    # selectCols <- c(
    #   "rowId",
    #   "treatment"
    # )
    #
    # tempCohorts <- cohorts[
    #   ,
    #   selectCols
    #   ]


    # tempCohorts <- data.table::setDT(
    #   dplyr::collect(
    #     cohortMethodData$cohorts
    #   ) %>%
    #     select(.data$rowId, .data$treatment)
    # )

    tempCohortsAfterMatching <- data.table::setDT(
      population %>%
        select(.data$rowId, .data$treatment, .data$stratumId)
    )
  }
  # on.exit(cohortMethodData$tempCohorts <- NULL)
  # on.exit(cohortMethodData$tempCohortsAfterMatching <- NULL, add = TRUE)

  # covariates <- setDT(
  #   dplyr::collect(cohortMethodData$covariates))
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
    full_join(afterMatching, by = "covariateId") %>%
    inner_join(covariateRef, by = "covariateId") %>%
    mutate(
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



#' @import data.table
#' @export
computeCovariatebalanceBase <- function(
  analysisSettings,
  runSettings,
  covariates,
  covariateRef,
  cohorts,
  stratOutcome,
  estOutcome
)
{


  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  if (!dir.exists(saveDir)) {
    dir.create(
      saveDir,
      recursive = TRUE
    )
  }

  # if (is.null(cohortMethodData)) {
  #   cohortMethodData <-
  #     CohortMethod::loadCohortMethodData(
  #       file = getDataSettings$cohortMethodDataFolder
  #     )
  # }

  analysisPath <- file.path(

    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )


  ps <- tryCatch(
    {
      if (stratOutcome == estOutcome)
      {

        readRDS(
          file.path(
            analysisPath,
            "Estimation",
            stratOutcome,
            "ps.rds"
          )
        )
      }
      else
      {
        readRDS(
          file.path(
            analysisPath,
            "Estimation",
            stratOutcome,
            estOutcome,
            "ps.rds"
          )
        )

      }
    },
    error = function(e)
    {
      e$message
    }
  )

  if (is.character(ps)) {
    return(NULL)
  } else {

    ps <- lapply(
      ps,
      data.table::setDT
    )

    covariateBalance <- tryCatch(
      {
        computeCovariateBalanceOverall(
          ps,
          covariates = covariates,
          covariateRef = covariateRef,
          cohorts = cohorts,
          analysisSettings = analysisSettings,
          runSettings = runSettings
        )
      },
      error = function(e) {
        e$message
      }
    )

    if (!is.character(covariateBalance)) {
      covariateBalance %>%
        dplyr::mutate(
          database = analysisSettings$databaseName,
          analysisId = analysisSettings$analysisId,
          stratOutcome = stratOutcome,
          estOutcome = estOutcome,
          treatmentId = analysisSettings$treatmentCohortId,
          comparatorId = analysisSettings$comparatorCohortId,
          analysisType = analysisSettings$analysisType
        ) %>%
        dplyr::select(
          c(
            "riskStratum",
            "covariateId",
            "covariateName",
            "beforeMatchingStdDiff",
            "afterMatchingStdDiff",
            "database",
            "analysisId",
            "stratOutcome",
            "estOutcome",
            "treatmentId",
            "comparatorId",
            "analysisType"
          )
        ) %>%
        saveRDS(
          file.path(
            saveDir,
            paste0(
              paste(
                "balance",
                analysisSettings$analysisId,
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
  }
}



#' @import data.table
#' @export
computeCovariateBalanceAnalysis2 <- function(
  analysisSettings,
  runSettings,
  getDataSettings = NULL,
  balanceThreads = 1
)
{

  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

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

  analysisPath <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId
  )

  saveDir <- file.path(
    analysisSettings$saveDirectory,
    analysisSettings$analysisId,
    "shiny"
  )

  if (!dir.exists(saveDir)) {
    dir.create(
      saveDir,
      recursive = TRUE
    )
  }

  for (predictOutcome in predictOutcomes)
  {

    predLoc <- which(analysisSettings$outcomeIds == predictOutcome)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- compareOutcomes[compareOutcomes != predictOutcome]
    compareOutcomes <- sort(
      compareOutcomes[compareOutcomes != predictOutcome]
    )

    estOutcomes <- c(
      predictOutcome,
      compareOutcomes
    )


    cluster <- ParallelLogger::makeCluster(analysisSettings$balanceThreads)
    ParallelLogger::clusterRequire(
      cluster,
      "RiskStratifiedEstimation"
    )
    ParallelLogger::clusterRequire(
      cluster,
      "data.table"
    )
    ParallelLogger::clusterRequire(
      cluster,
      "dplyr"
    )

    dummy <- ParallelLogger::clusterApply(
      cluster = cluster,
      x = estOutcomes,
      fun = computeCovariatebalanceBase,
      analysisSettings = analysisSettings,
      runSettings = runSettings,
      covariates = covariates,
      covariateRef = covariateRef,
      cohorts = cohorts,
      stratOutcome = predictOutcome
    )

    ParallelLogger::stopCluster(cluster)
  }
}



#' @import data.table
#' @export
computeCovariateBalanceOverall <- function(
  ps,
  covariates,
  covariateRef,
  cohorts,
  analysisSettings,
  runSettings
)
{


  psMethod <- runSettings$runCmSettings$psMethod

  if (psMethod == "inversePtWeighted") {
    covariateBalanceList <-
      lapply(
        ps,
        computeCovariateBalanceWeighted,
        cohortMethodData = cohortMethodData
      )  %>%
      dplyr::bind_rows(
        .id = "riskStratum"
      ) %>%
      dplyr::mutate(
        riskStratum = paste0(
          "Q",
          riskStratum
        )
      )

  } else if (psMethod == "stratifyByPs" | psMethod == "matchOnPs") {

    # for (i in 1:4)
    # {
    #   computeCovariateBalance4(
    #     population = ps[[i]],
    #     covariates = covariates,
    #     covariateRef = covariateRef,
    #     cohorts = cohorts
    #   )
    # }

    covariateBalanceList <- lapply(
      ps,
      computeCovariateBalance4,
      covariates = covariates,
      covariateRef = covariateRef,
      cohorts = cohorts
    ) %>%
      dplyr::bind_rows(
        .id = "riskStratum"
      ) %>%
      dplyr::mutate(
        riskStratum = paste0(
          "Q",
          riskStratum
        )
      )

  }

  return(covariateBalanceList)

}
