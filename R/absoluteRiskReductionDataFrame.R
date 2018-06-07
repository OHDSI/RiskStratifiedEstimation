#' Creates a data frame of absolute risk reductions within risk strata
#'
#' @param psList A list with the propensity data frames as generated from \code{\link[CohortMethod]{createPs}}
#' @param timePoint The time point based on which the absolute risk reductions will be calculated
#' @param weightsType The type of weights to be used. Possible values are 'ATE' or 'ATT'
#' @param useSW Whether to use stabilized weights.
#' @param truncatedWeights Whether to truncate the weights
#' @param truncationQuantiles The quantiles used to truncate the weights
#'
#' @return A data frame with the absolute risk reductions along with confidence intervals
#'
#' @export

absoluteRiskReductionDataFrame <- function(psList,
                                           timePoint,
                                           weightsType = 'ATE',
                                           useSW = TRUE,
                                           truncatedWeights = TRUE,
                                           truncationQuantiles = c(.01, .99)){
  ARRDataFrame <- data.frame(ARR = numeric(),
                             lower95 = numeric(),
                             upper95 = numeric(),
                             riskStratum = numeric())

  for(i in 1:length(psList)){

    ps <- psList[[i]]

    if(is.null(ps$weights))
      ps <- createIPW(ps,
                      weightsType = weightsType,
                      useSW = useSW,
                      truncatedWeights = truncatedWeights,
                      truncationQuantiles = truncationQuantiles)

    adjustedKM <- weightedKM(ps,
                             calculateWeights = FALSE,
                             returnPlot = FALSE,
                             truncationQuantiles = truncationQuantiles)$data
    treatmentKM <- subset(adjustedKM, cohort == 'treatment')
    comparatorKM <- subset(adjustedKM, cohort == 'comparator')

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
