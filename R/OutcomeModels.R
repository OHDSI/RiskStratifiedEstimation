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
# @author Alexandros Rekkasbcemsx
# @author Peter Rijnbeek




#' Fit outcome models
#'
#' Fits outcome models within risk strata, estimating relative and absolute differences. Designed to be performed within
#' a parellelized analysis.
#'
#' @param outcomeId               The outcome of interest for which the esitmation is performed. That is the outcome
#'                                for which risk stratification is performed.
#' @param getDataSettings         An R object of type \code{getDataSettings} created using the function
#'                                \code{\link[RiskStratifiedEstimation]{createGetDataSettings}}.
#' @param pathToPs                The path to the \code{RSEE} analysis results.
#' @param analysis                An analysis object contained in \code{runCmSettings}
#'
#' @importFrom magrittr %>%

fitOutcomeModels <- function(
  outcomeId,
  getDataSettings,
  pathToPs,
  analysis
) {

  ParallelLogger::logInfo(
    paste(
      "Calculating main results for outcome:",
      outcomeId
    )
  )
  analysisPath <- file.path(
    pathToPs,
    outcomeId
  )

  ps <- tryCatch(
    {
      readRDS(
        file.path(
          analysisPath,
          "ps.rds"
        )
      )
    },
    error = function(e)
    {
      e$message
    }
  )

  ParallelLogger::logInfo(
    "Read PS and CohortMethod data"
  )

  ParallelLogger::logInfo(
    "Starting estimation of treatment effects"
  )

  treatmentEffects <- tryCatch(
    {
      estimateTreatmentEffect(
        ps = ps,
        analysis = analysis
      )
    },
    error = function(e)
    {
      e$message
    }
  )


  ParallelLogger::logInfo(
    "Done estimating treatment effects"
  )

  if (!is.character(treatmentEffects)) {

    saveRDS(
      treatmentEffects$relativeRiskReduction,
      file = file.path(
        analysisPath,
        "relativeRiskReduction.rds"
      )
    )
    saveRDS(
      treatmentEffects$absoluteRiskReduction,
      file = file.path(
        analysisPath,
        "absoluteRiskReduction.rds"
      )
    )
    saveRDS(
      treatmentEffects$models,
      file = file.path(
        analysisPath,
        "models.rds"
      )
    )
    saveRDS(
      treatmentEffects$cases,
      file = file.path(
        analysisPath,
        "cases.rds"
      )
    )
    saveRDS(
      treatmentEffects$ps,
      file = file.path(
        analysisPath,
        paste0(
          paste(
            "ps",
            "analysis",
            sep = "_"
          ),
          ".rds"
        )
      )
    )
  }

  ParallelLogger::logInfo(
    'Saved results'
  )

  return(NULL)
}



#' Calculate stratified Kaplan-Meier estimates
#'
#' @param population                The population of interest stratified using the
#'                                  \code{\link[CohortMethod]{stratifyByPs}}
#' @param timePoint                 The point in time for which the absolute risk difference is required
#'
#' @return                          A vector of the absolute risk difference along with the lowest and highest limits
#'                                  of the the 95 percent confidence interval
#'
stratifiedKaplanMeier <- function(population, timePoint) {

  kaplanMeier <- list()

  for (i in unique(population$stratumId))
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
    sum(unlist(standardErrors)^2)/25)

  return(
    c(
      arr,
      arr - 1.96*pooledStandardError,
      arr + 1.96*pooledStandardError
    )
  )

}



#' Estimate treatment effects within risk strata
#'
#' Estimates treatment effects within risk strata based on the length of list \code{ps}.
#'
#' @param ps               A list of objects created from \code{\link[CohortMethod]{createPs}} estimated within risk
#'                         strata.
#' @param analysis         An analysis object.
#' @return                 A list containing :
#'                         \itemize{
#'                           \item{relativeRiskReduction}{Hazard ratios along with confidence intervals within risk
#'                             strata}
#'                           \item{absoluteRiskReduction}{Absolute risk differences along with confidence intervals
#'                             within risk strata}
#'                           \item{cases}{Observed outcome proportions within risk strata}
#'                           \item{models}{The models used to estimate relative risk reduction within risk strata}
#'                         }
#'
#'

estimateTreatmentEffect <- function(
  ps,
  analysis
) {

  if (analysis$psMethod == "matchOnPs") {
    ps <- lapply(
      ps,
      CohortMethod::matchOnPs,
      caliper = analysis$effectEstimationSettings$caliper,
      caliperScale = analysis$effectEstimationSettings$caliperScale,
      maxRatio = analysis$effectEstimationSettings$maxRatio,
      stratificationColumns = analysis$effectEstimationSettings$stratificationColumns
    )

    if (analysis$effectEstimationSettings$maxRatio > 1) {
      models <- lapply(
        ps,
        CohortMethod::fitOutcomeModel,
        stratified = TRUE,
        modelType = "cox"
      )
    } else {
      models <- lapply(
        ps,
        CohortMethod::fitOutcomeModel,
        stratified = FALSE,
        modelType = "cox"
      )
    }

    models <- lapply(
      ps,
      CohortMethod::fitOutcomeModel,
      stratified = FALSE,
      modelType = "cox"
    )

    cases <- do.call(
      rbind,
      lapply(
        ps,
        getCounts,
        timePoint = analysis$timePoint,
        psMethod = analysis$psMethod
      )
    )

    colnames(cases) <- c(
      "casesComparator",
      "casesTreatment"
    )

    cases <- as.data.frame(
      cases
    )

    riskStrata <- length(ps)

    cases$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )

    arr <- do.call(
      rbind,
      lapply(
        ps,
        absoluteRiskReduction,
        timePoint = analysis$timePoint,
        psMethod = analysis$psMethod
      )
    )

    colnames(arr) <- c(
      "estimate",
      "lower",
      "upper"
    )

    arr <- as.data.frame(arr)

    arr$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )

    rrr <- do.call(
      rbind,
      lapply(
        models,
        relativeRiskReduction
      )
    )

    colnames(rrr) <- c(
      "estimate",
      "lower",
      "upper",
      "seLogRr"
    )

    rrr <- as.data.frame(rrr)

    rrr$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )

  } else if (analysis$psMethod == "stratifyByPs") {
    ps <- lapply(
      ps,
      CohortMethod::stratifyByPs,
      numberOfStrata = analysis$effectEstimationSettings$numberOfStrata,
      stratificationColumns = analysis$effectEstimationSettings$stratificationColumns,
      baseSelection = analysis$effectEstimationSettings$baseSelection
    )

    ParallelLogger::logInfo(
      "Stratified by PS in risk strata"
    )

    cases <- do.call(
      rbind,
      lapply(
        ps,
        getCounts,
        timePoint = analysis$timePoint,
        psMethod = analysis$psMethod
      )
    )

    ParallelLogger::logInfo(
      "Calculated number of cases"
    )

    colnames(cases) <- c(
      "casesComparator",
      "casesTreatment"
    )

    cases <- as.data.frame(cases)
    riskStrata <- length(ps)
    cases$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )
    models <- lapply(
      ps,
      CohortMethod::fitOutcomeModel, stratified = TRUE, modelType = "cox")
    ParallelLogger::logInfo("Fitted outcome models")
    arr <- do.call(
      rbind,
      lapply(
        ps,
        absoluteRiskReduction,
        timePoint = analysis$timePoint,
        psMethod = analysis$psMethod
      )
    )

    ParallelLogger::logInfo(
      "Calculated absolute risk reduction across risk strata"
    )

    colnames(arr) <- c(
      "estimate",
      "lower",
      "upper"
    )

    arr <- as.data.frame(arr)
    arr$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )

    rrr <- do.call(
      rbind,
      lapply(
        models,
        relativeRiskReduction
      )
    )

    ParallelLogger::logInfo(
      "Calculated relative risk reduction across risk strata"
    )

    colnames(rrr) <- c(
      "estimate",
      "lower",
      "upper",
      "seLogRr"
    )

    rrr <- as.data.frame(rrr)
    rrr$riskStratum <- paste0(
      "Q",
      1:riskStrata
    )
  }

  return(
    list(
      relativeRiskReduction = rrr %>% dplyr::mutate(analysisType = analysis$label),
      absoluteRiskReduction = arr %>% dplyr::mutate(analysisType = analysis$label),
      cases = cases %>% dplyr::mutate(analysisType = analysis$label),
      models = models,
      ps = ps
    )
  )

}
