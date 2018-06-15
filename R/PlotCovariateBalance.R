#' Plots the covariate balance before and after balancing
#'
#' Plots covariate before and after weighting using the inverse of the propensity score
#'
#' @param ps A propensity score data frame as created from \code{\link[CohortMethod]{createPs}}
#' @param cohortMethodData A cohortMethodData object
#' @param calculateWeights Should the weights be calculated?
#' @param weightsType The type of the weights to be used. Allowed options are 'ATE' for average treatment effect and 'ATT' for average treatment effect on the treated weights
#' @param useStabilizedWeights Should stabilized weights be used?
#' @param truncatedWeights Should truncated weights be used? If FALSE, then stabilized weights are used instead
#' @param truncationQuantiles The quantiles to perform weight truncation
#'
#' @return The covariate balance plot
#' @export



plotCovariateBalance <- function(ps,
                                 cohortMethodData,
                                 calculateWeights = TRUE,
                                 weightsType = 'ATE',
                                 useStabilizedWeights = TRUE,
                                 truncatedWeights = TRUE,
                                 truncationQuantiles = c(.01, .99)){
  if(calculateWeights)
    ps <- createIPW(ps,
                    weightsType = weightsType,
                    useStabilizedWeights = useStabilizedWeights,
                    truncatedWeights = truncatedWeights,
                    truncationQuantiles = truncationQuantiles)

  nTreatmentRaw <- sum(ps$treatment)
  nComparatorRaw <- sum(!ps$treatment)
  nTreatmentWeighted <- sum(ps$treatment*ps$weights)
  nComparatorWeighted <- sum((!ps$treatment)*ps$weights)
  uniqueRowIds <- unique(cohortMethodData$covariates$rowId)
  uniqueCovariateIdsFull <- unique(cohortMethodData$covariates$covariateId)
  d <- data.frame(covariateId = numeric(),
                  before = numeric(),
                  after = numeric())

  for(i in 1:length(uniqueCovariateIdsFull)){

    uniqueCovariateId <- uniqueCovariateIdsFull[i]
    subsetCovariates <- subset(cohortMethodData$covariates, covariateId == uniqueCovariateId)
    subsetPs <- subset(ps, rowId %in% subsetCovariates[]$rowId)
    pHatTreatment <- sum(subsetPs$treatment)/nTreatmentRaw
    pHatComparator <- sum(!subsetPs$treatment)/nComparatorRaw
    beforeWeighting <- 100*abs((pHatTreatment - pHatComparator) /
                                 sqrt((pHatTreatment*(1 - pHatComparator) + pHatComparator*(1 - pHatTreatment))/2))
    pHatTreatment <- sum(subsetPs$treatment*subsetPs$weights)/nTreatmentWeighted
    pHatComparator <- sum((!subsetPs$treatment)*subsetPs$weights)/nComparatorWeighted
    afterWeighting <- 100*abs((pHatTreatment - pHatComparator) /
                                sqrt((pHatTreatment*(1 - pHatComparator) + pHatComparator*(1 - pHatTreatment))/2))
    d[i, ] <- c(uniqueCovariateId, beforeWeighting, afterWeighting)

  }
  d$inside <- ifelse((d$before >10 & d$after>10), d$covariateId, '')
  ggplot2::ggplot(d, ggplot2::aes(before, after)) +
    ggplot2::geom_point(size = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 10), linetype = 'dashed', color = 'red') +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 10, color = 'red'), linetype = 'dashed', color = 'red') +
    ggplot2::geom_text(ggplot2::aes(label=inside), hjust=0, vjust=1.2) +
    ggplot2::xlab('Before weighting') +
    ggplot2::ylab('After weighting')

}
