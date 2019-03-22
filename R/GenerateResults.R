#' Generates results within risk strata
#'
#' Generates results within risk strata. More specifically, it calculates the weighted Kaplan-Meier estimates, the relative and absolute risk
#' differences within risk strata.
#'
#' @param ps           A list of the estimated propensity scores within risk strata with a column containing the inverse probability weights.
#'                     The elements can be either dataframes or \code{ffdf} objects.
#' @param timePoint    The time point of interest for the calculation of the absolute risk reduction.
#'
#' @return A list containing the weighted Kaplan-Meier estimates, the absolute and relative risk reductions along with the observed case-counts
#'         wtihin risk strata.
#' @export
#'


generateResults <- function(ps,
                            timePoint){

  ps <- lapply(ps, as.data.frame)

  dataKM <- list()
  for(j in 1:length(ps)){

    dataKM[[j]] <- weightedKM(ps[[j]],
                              calculateWeights = FALSE)
  }


  ParallelLogger::logInfo('Generated weighted Kaplan-Meier estimates within risk strata')

  AbsoluteRiskReduction <- absoluteRiskReduction(dataKM,
                                                 timePoint)
  ParallelLogger::logInfo('Estimated absolute risk differences within risk strata')

  RelativeRiskReduction <- relativeRiskReduction(ps,
                                                 calculateWeights = FALSE)

  ParallelLogger::logInfo('Estimated hazard ratios within risk strata')

  treatedCases <- data.frame(riskStratum = numeric(),
                             outcomeRate = numeric())
  comparatorCases <- data.frame(riskStratum = numeric(),
                                outcomeRate = numeric())


  for(j in 1:length(ps)){

    treatmentEvents <- subset(dataKM[[j]], eventTime == 1 & cohort == 'treatment')
    sortTimes <- sort(c(timePoint, treatmentEvents$time))
    if(sum(sortTimes == timePoint) == 1){
      positionTreatment <- which(sortTimes == timePoint)
      survivalTreatment <- 1 - treatmentEvents$S[positionTreatment - 1]
    }else{
      positionTreatment <- which(treatmentEvents$time == timePoint)
      survivalTreatment <- 1 - treatmentEvents$S[positionTreatment]
    }

    comparatorEvents <- subset(dataKM[[j]], eventTime == 1 & cohort == 'comparator')
    sortTimes <- sort(c(timePoint, comparatorEvents$time))
    if(sum(sortTimes == timePoint) == 1){
      positionComparator <- which(sortTimes == timePoint)
      survivalComparator <- 1 - comparatorEvents$S[positionComparator - 1]
    }else{
      positionComparator <- which(comparatorEvents$time == timePoint)
      survivalComparator <- 1 - comparatorEvents$S[positionComparator]
    }

    treatedCases[j, ] <- c(j, survivalTreatment)
    comparatorCases[j, ] <- c(j, survivalComparator)

  }

  cases <- dplyr::bind_rows(data = treatedCases, comparatorCases, .id = 'cohort')
  cases$cohort <- factor(cases$cohort, levels = 1:2, labels = c('treatment', 'comparator'))
  cases$riskStratum <- paste('Q', cases$riskStratum, sep = '')
  ParallelLogger::logInfo('Generated counts within risk strata')
  return(list(dataKM = dataKM,
              absoluteRiskReduction = AbsoluteRiskReduction,
              relativeRiskReduction = RelativeRiskReduction,
              cases = cases))

}

