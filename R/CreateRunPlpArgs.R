#' Create a parameter object for the function runPlp
#'
#' @details
#' Create an object defining the parameter values.
#'
#' @param minCovariateFraction The minimum fraction of target population who must have a covariate for it to be included in the model training
#' @param normalizeData Whether to normalise the covariates before training (Default: TRUE)
#' @param testSplit Either 'person' or 'time' specifying the type of evaluation used.'time' find the date where testFraction of patients had an index after the date and assigns patients with an index prior to this date into the training set and post the date into the test set'person' splits the data into test (1-testFraction of the data) andtrain (validationFraction of the data) sets.  The split is stratified by the class label.
#' @param testFraction The fraction of the data to be used as the test set in the patientsplit evaluation.
#' @param trainFraction A real number between 0 and 1 indicating the train set fraction of the data.If not set trainFraction is equal to 1 - test
#' @param splitSeed The seed used to split the test/train set when using a person type testSplit
#' @param nfold The number of folds used in the cross validation (default 3)
#' @param indexes A dataframe containing a rowId and index column where the index value of -1 means in the test set, and positive integer represents the cross validation fold (default is NULL)
#' @param savePlpData Binary indicating whether to save the plpData object (default is T)
#' @param savePlpResult Binary indicating whether to save the object returned by runPlp (default is T)
#' @param savePlpPlots Binary indicating whether to save the performance plots as pdf files (default is T)
#' @param saveEvaluation Binary indicating whether to save the oerformance as csv files (default is T)
#' @param verbosity Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:DEBUGHighest verbosity showing all debug statementsTRACEShowing information about start and end of stepsINFOShow informative information (Default)WARNShow warning messagesERRORShow error messagesFATALBe silent except for fatal errors
#' @param timeStamp If TRUE a timestamp will be added to each logging statement. Automatically switched on for TRACE level.
#' @param analysisId Identifier for the analysis. It is used to create, e.g., the result folder. Default is a timestamp.
#'
#' @export
createRunPlpArgs <- function(minCovariateFraction = 0.001,
                             normalizeData = TRUE,
                             testSplit = "person",
                             testFraction = 0.25,
                             trainFraction = NULL,
                             splitSeed = NULL,
                             nfold = 3,
                             indexes = NULL,
                             savePlpData = TRUE,
                             savePlpResult = TRUE,
                             savePlpPlots = TRUE,
                             saveEvaluation = TRUE,
                             verbosity = "INFO",
                             timeStamp = FALSE,
                             analysisId = NULL) {
  # First: get default values:
  analysis <- list()
  for (name in names(formals(createRunPlpArgs))) {
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
