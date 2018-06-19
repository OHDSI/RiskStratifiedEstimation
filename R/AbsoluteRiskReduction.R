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

    treatmentKM <- subset(dataKM[[i]], cohort == 'treatment')
    comparatorKM <- subset(dataKM[[i]], cohort == 'comparator')

    p <- which(treatmentKM$time >= timePoint)[1]

    if(timePoint < treatmentKM$time[p] & timePoint >= treatmentKM$time[p - 1]){
      survivalProbabilityTreatment <- treatmentKM$S[p - 1]
      p0Treatment <- p - 1
    }else{
      survivalProbabilityTreatment <- treatmentKM$S[p]
      p0Treatment <- p
    }


    p <- which(comparatorKM$time >= timePoint)[1]

    if(timePoint < comparatorKM$time[p] & timePoint >= comparatorKM$time[p - 1]){
      survivalProbabilityComparator <- comparatorKM$S[p - 1]
      p0Comparator <- p - 1
    }else{
      survivalProbabilityComparator <- comparatorKM$S[p]
      p0Comparator <- p
    }

    ARRvalue <- survivalProbabilityTreatment - survivalProbabilityComparator
    seARR <- sqrt(comparatorKM$varS[p0Comparator] + treatmentKM$varS[p0Treatment])
    ARRDataFrame[i, ] <- c(ARRvalue, ARRvalue - 1.96*seARR, ARRvalue + 1.96*seARR, i)

  }
  ARRDataFrame
}
