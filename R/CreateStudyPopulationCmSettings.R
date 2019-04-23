#' Create a parameter object for the function createStudyPopulation
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param population If specified, this population will be used as the startingpoint instead of the cohorts in the cohortMethodDataobject.
#' @param firstExposureOnly Should only the first exposure per subject be included? Notethat this is typically done in thecreateStudyPopulation function,
#' @param restrictToCommonPeriod Restrict the analysis to the period when both exposures are observed?
#' @param washoutPeriod The mininum required continuous observation time prior toindex date for a person to be included in the cohort.
#' @param removeDuplicateSubjects Remove subjects that are in both the target and comparatorcohort? See details for allowed values.
#' @param removeSubjectsWithPriorOutcome Remove subjects that have the outcome prior to the riskwindow start?
#' @param priorOutcomeLookback How many days should we look back when identifying prioroutcomes?
#' @param minDaysAtRisk The minimum required number of days at risk.
#' @param riskWindowStart The start of the risk window (in days) relative to the indexdate (+ days of exposure if theaddExposureDaysToStart parameter is specified).
#' @param addExposureDaysToStart Add the length of exposure the start of the risk window?
#' @param riskWindowEnd The end of the risk window (in days) relative to the indexdata (+ days of exposure if the addExposureDaysToEndparameter is specified).
#' @param addExposureDaysToEnd Add the length of exposure the risk window?
#' @param censorAtNewRiskWindow If a subject is in multiple cohorts, should time-at-risk be censoredwhen the new time-at-risk starts to prevent overlap?
#'
#' @export

createStudyPopulationCmSettings <-
  function(population = NULL,
           firstExposureOnly = FALSE,
           restrictToCommonPeriod = FALSE,
           washoutPeriod = 0,
           removeDuplicateSubjects = FALSE,
           removeSubjectsWithPriorOutcome = TRUE,
           priorOutcomeLookback = 99999,
           minDaysAtRisk = 1,
           riskWindowStart = 0,
           addExposureDaysToStart = FALSE,
           riskWindowEnd = 0,
           addExposureDaysToEnd = TRUE,
           censorAtNewRiskWindow = FALSE) {


    # First: get default values:
    analysis <- list()
    for (name in names(formals(createStudyPopulationCmSettings))) {
      analysis[[name]] <- get(name)
    }
    # Second: overwrite defaults with actual values:
    values <- lapply(as.list(match.call())[-1], function(x) eval(x, envir = sys.frame(-3)))
    for (name in names(values)) {
      if (name %in% names(analysis))
        analysis[[name]] <- values[[name]]
    }
    class(analysis) <- "args"
    return(analysis)


  }
