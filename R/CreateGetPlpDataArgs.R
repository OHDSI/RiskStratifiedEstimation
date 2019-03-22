#' Create a parameter object for the function getPlpData
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param studyStartDate A calendar date specifying the minimum date that a cohort indexdate can appear. Date format is 'yyyymmdd'.
#' @param studyEndDate A calendar date specifying the maximum date that a cohort indexdate can appear. Date format is 'yyyymmdd'. Important: the studyend data is also used to truncate risk windows, meaning no outcomesbeyond the study end date will be considered.
#' @param firstExposureOnly Should only the first exposure per subject be included? Note thatthis is typically done in the createStudyPopulation function,but can already be done here for efficiency reasons.
#' @param washoutPeriod The mininum required continuous observation time prior to indexdate for a person to be included in the at risk cohort. Note thatthis is typically done in the createStudyPopulation function,but can already be done here for efficiency reasons.
#' @param excludeDrugsFromCovariates A redundant option
#'
#' @export
createGetPlpDataArgs <- function(studyStartDate = "",
                                 studyEndDate = "",
                                 firstExposureOnly = FALSE,
                                 washoutPeriod = 0,
                                 excludeDrugsFromCovariates = FALSE) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createGetPlpDataArgs))) {
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
