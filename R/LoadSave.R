#' Load the result of a risk stratified analysis
#'
#' Loads the result of a risk stratified analysis.
#'
#' @param file                   The file location where the results are stored. It should point at the "Estimation" folder of
#'                               a risk stratified analysis
#' @param mainOutcomes           The main outcomes for which the results should be loaded. If set to \code{NULL} the results
#'                               for all the outcomes are loaded.
#' @param loadPs                 Should the propensity scores along with the matrices mapping risk stratification be loaded?
#'
#' @return                       The result of a previous risk stratified analysis.
#'
#' @export

loadRSEE <- function(file,
                     mainOutcomes = NULL,
                     loadPs = TRUE){

  if(is.null(mainOutcomes))
    mainOutcomes <- list.dirs(file,
                              recursive = FALSE,
                              full.names = FALSE)

  mainOutcomesRes <- list()
  otherOutcomesRes <- list()
  results <- list()

  if(loadPs){
    psRes <- list()
    for(i in 1:length(mainOutcomes))
      psRes[[i]] <- list(ps = readRDS(file = file.path(file, mainOutcomes[i], "ps.rds")),
                         mapMatrix = readRDS(file = file.path(file, mainOutcomes[i], "mapMatrix.rds")))

  }

  for(i in 1:length(mainOutcomes)){

    models <- readRDS(file = file.path(file, mainOutcomes[i], "models.rds"))
    relative <- readRDS(file = file.path(file, mainOutcomes[i], "relativeRiskReduction.rds"))
    absolute <- readRDS(file = file.path(file, mainOutcomes[i], "absoluteRiskReduction.rds"))
    cases <- readRDS(file = file.path(file, mainOutcomes[i], "cases.rds"))


    mainOutcomesRes <- list(models = models,
                            relative = relative,
                            absolute = absolute,
                            cases = cases)

    otherOutcomes <- list.dirs(file.path(file, mainOutcomes[i]),
                               recursive = FALSE,
                               full.names = FALSE)

    for(j in 1:length(otherOutcomes)){

      models <- readRDS(file = file.path(file, mainOutcomes[i], otherOutcomes[j], "models.rds"))
      relative <- readRDS(file = file.path(file, mainOutcomes[i], otherOutcomes[j], "relativeRiskReduction.rds"))
      absolute <- readRDS(file = file.path(file, mainOutcomes[i], otherOutcomes[j], "absoluteRiskReduction.rds"))
      cases <- readRDS(file = file.path(file, mainOutcomes[i], otherOutcomes[j], "cases.rds"))

      otherOutcomesRes[[j]] <- list(models = models,
                                    relative = relative,
                                    absolute = absolute,
                                    cases = cases)

    }

    names(otherOutcomesRes) <- paste("outcome", otherOutcomes, sep = "_")
    results[[i]] <- list(mainOutcome = mainOutcomesRes,
                         otherOutcomes = otherOutcomesRes)


  }

  names(results) <- names(psRes) <- paste("outcome", mainOutcomes, sep = "_")

  finalRes <- list(results = results)

  if(loadPs)
    finalRes$ps <- psRes


  return(finalRes)
}
