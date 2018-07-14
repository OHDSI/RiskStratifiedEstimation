#' Loads the result from runRiskStratifiedEstimation
#'
#' @param location The location where the result of \code{\link{runRiskStratifiedEstimation}} was saved. By default it is the folder called RSEE along with the date and time of the start
#'
#' @return An object with all the output created from runRiskStratifiedEstimation
#' @export

loadRSEEResult <- function(location){

  result <- list()
  result$absoluteRiskReduction.rds <- readRDS(file.path(location, 'absoluteRiskReduction.rds'))
  result$relativeRiskReduction <- readRDS(file.path(location, 'relativeRiskReduction.rds'))
  result$dataKM <- readRDS(file.path(location, 'dataKM.rds'))
  result$mapMatrix <- readRDS(file.path(location, 'mapMatrix.rds'))
  result$ps <- readRDS(file.path(location, 'ps.rds'))
  result$cases <- readRDS(file.path(location, 'cases.rds'))

  return(result)


}
