#' Evaluate an existing model
#'
#' Evaluates an existing model within the setting of a risk stratified analysis
#'
#' @param plpData a plpData object
#' @param outcomeId the outcome cohort id
#' @param riskWindowStart Parameter to be passed to \code{\link[PatientLevelPrediction]{createStudyPopulation}}.The start of the risk window (in days) relative to the index date (+ days of exposure if the addExposureDaysToStart parameter is specified)
#' @param riskWindowEnd Parameter to be passed to \code{\link[PatientLevelPrediction]{createStudyPopulation}}. The end of the risk window (in days) relative to the index data (+ days of exposure if the addExposureDaysToEnd parameter is specified)
#' @param requireTimeAtRisk Parameter to be passed to \code{\link[PatientLevelPrediction]{createStudyPopulation}}. Should subject without time at risk be removed?
#' @param minTimeAtRisk Parameter to be passed to \code{\link[PatientLevelPrediction]{createStudyPopulation}}. The minimum number of days at risk required to be included
#' @param includeAllOutcomes Parameter to be passed to \code{\link[PatientLevelPrediction]{createStudyPopulation}}. (binary) indicating whether to include people with outcomes who are not observed for the whole at risk period
#'
#' @return A list containing the performance values


evaluateExistingModelCombine <- function(plpData,
                                          outcomeId,
                                          riskWindowStart = 1,
                                          riskWindowEnd = 365,
                                          requireTimeAtRisk = T,
                                          minTimeAtRisk = 364,
                                          includeAllOutcomes = T){

  population <- PatientLevelPrediction::createStudyPopulation(plpData = plpData, outcomeId = outcomeId,
                                                              includeAllOutcomes = includeAllOutcomes, requireTimeAtRisk = requireTimeAtRisk,
                                                              minTimeAtRisk = minTimeAtRisk, riskWindowStart = riskWindowStart,
                                                              riskWindowEnd = riskWindowEnd
  )
  prediction <- merge(population, ff::as.ram(plpData$covariates), by='rowId', all.x=T)
  colnames(prediction)[colnames(prediction)=='covariateValue'] <- 'value'
  prediction$value <- prediction$value/max(prediction$value)
  attr(prediction, "metaData")$predictionType <- "binary"
  performance <- PatientLevelPrediction::evaluatePlp(prediction, plpData)

  # reformatting the performance
  analysisId <-   '000000'
  nr1 <- length(unlist(performance$evaluationStatistics[-1]))
  performance$evaluationStatistics <- cbind(analysisId= rep(analysisId,nr1),
                                            Eval=rep('validation', nr1),
                                            Metric = names(unlist(performance$evaluationStatistics[-1])),
                                            Value = unlist(performance$evaluationStatistics[-1])
  )
  nr1 <- nrow(performance$thresholdSummary)
  performance$thresholdSummary <- cbind(analysisId=rep(analysisId,nr1),
                                        Eval=rep('validation', nr1),
                                        performance$thresholdSummary)
  nr1 <- nrow(performance$demographicSummary)
  if(!is.null(performance$demographicSummary)){
    performance$demographicSummary <- cbind(analysisId=rep(analysisId,nr1),
                                            Eval=rep('validation', nr1),
                                            performance$demographicSummary)
  }
  nr1 <- nrow(performance$calibrationSummary)
  performance$calibrationSummary <- cbind(analysisId=rep(analysisId,nr1),
                                          Eval=rep('validation', nr1),
                                          performance$calibrationSummary)
  nr1 <- nrow(performance$predictionDistribution)
  performance$predictionDistribution <- cbind(analysisId=rep(analysisId,nr1),
                                              Eval=rep('validation', nr1),
                                              performance$predictionDistribution)

  return(list(performance=performance, prediction=prediction))

}
