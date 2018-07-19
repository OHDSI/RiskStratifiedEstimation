#' Creates a data frame of absolute risk reductions within risk strata
#'
#' @param dataKM A list with the Kaplan-Meier estimates within risk strata
#' @param timePoint The time point based on which the absolute risk reductions will be calculated
#'
#' @return A data frame with the absolute risk reductions along with confidence intervals
#'
#' @export

absoluteRiskReduction <- function(dataKM,
                                  timePoint){

  ARRDataFrame <- data.frame(ARR = numeric(),
                             lower95 = numeric(),
                             upper95 = numeric(),
                             riskStratum = numeric())

  for(i in 1:length(dataKM)){

    treatmentEvents <- subset(res$dataKM[[i]], eventTime == 1 & cohort == 'treatment')
    sortTimes <- sort(c(timePoint, treatmentEvents$time))
    if(sum(sortTimes == timePoint) == 1){
      positionTreatment <- which(sortTimes == timePoint)
      survivalTreatment <- treatmentEvents$S[positionTreatment - 1]
    }else{
      positionTreatment <- which(treatmentEvents$time == timePoint)
      survivalTreatment <- treatmentEvents$S[positionTreatment]
    }

    comparatorEvents <- subset(res$dataKM[[i]], eventTime == 1 & cohort == 'comparator')
    sortTimes <- sort(c(timePoint, comparatorEvents$time))
    if(sum(sortTimes == timePoint) == 1){
      positionComparator <- which(sortTimes == timePoint)
      survivalComparator <- comparatorEvents$S[positionComparator - 1]
    }else{
      positionComparator <- which(comparatorEvents$time == timePoint)
      survivalComparator <- comparatorEvents$S[positionTreatment]
    }

    ARRValue <- survivalTreatment - survivalComparator
    seARR <- sqrt(treatmentEvents$varS[positionTreatment] + comparatorEvents$varS[positionComparator])
    ARRDataFrame[i, ] <- c(ARRValue, ARRValue - 1.96*seARR, ARRValue + 1.96*seARR, i)

  }
  ARRDataFrame
}
