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
#' @param showNotBalancedCovariateIds Show covariate ids that were not balanced after weighting?
#'
#' @return The covariate balance plot
#' @export



plotCovariateBalance <- function(ps,
                                 cohortMethodData,
                                 calculateWeights = TRUE,
                                 weightsType = 'ATE',
                                 useStabilizedWeights = TRUE,
                                 truncatedWeights = TRUE,
                                 truncationQuantiles = c(.01, .99),
                                 showNotBalancedCovariateIds = TRUE){

  if(calculateWeights)
    ps <- createIPW(ps,
                    weightsType = weightsType,
                    useStabilizedWeights = useStabilizedWeights,
                    truncatedWeights = truncatedWeights,
                    truncationQuantiles = truncationQuantiles)

  nTreatment <- sum(ps$treatment)
  nComparator <- sum(!ps$treatment)

  stratumSubset <- ff::as.ffdf(cohortMethodData$covariates[cohortMethodData$covariates$rowId[] %in% ps$rowId, ]) # better way?
  subsetPs <- subset(ps, select = c(rowId, treatment, weights))
  mergedSubset <- ffbase::merge.ffdf(stratumSubset, ff::as.ffdf(subsetPs))

  sTreatment <- FeatureExtraction::bySumFf(mergedSubset$treatment*mergedSubset$covariateValue,
                                           mergedSubset$covariateId)
  sComparator <- FeatureExtraction::bySumFf((!mergedSubset$treatment)*mergedSubset$covariateValue,
                                            mergedSubset$covariateId)
  names(sTreatment) <- c('covariateId', 'sum')
  names(sComparator) <- c('covariateId', 'sum')
  pHatTreatment <- sTreatment$sum / nTreatment
  pHatComparator <- sComparator$sum / nComparator
  beforeWeighting <- 100*abs((pHatTreatment - pHatComparator) /
                               sqrt((pHatTreatment*(1 - pHatComparator) + pHatComparator*(1 - pHatTreatment))/2))

  sTreatment <- FeatureExtraction::bySumFf(mergedSubset$treatment*mergedSubset$covariateValue*mergedSubset$weights,
                                           mergedSubset$covariateId)
  sComparator <- FeatureExtraction::bySumFf((!mergedSubset$treatment)*mergedSubset$covariateValue*mergedSubset$weights,
                                            mergedSubset$covariateId)
  names(sTreatment) <- c('covariateId', 'sum')
  names(sComparator) <- c('covariateId', 'sum')
  nTreatment <- sum(ps$treatment*ps$weights)
  nComparator <- sum((!ps$treatment)*ps$weights)
  pHatTreatment <- sTreatment$sum / nTreatment
  pHatComparator <- sComparator$sum / nComparator
  afterWeighting <- 100*abs((pHatTreatment - pHatComparator) /
                              sqrt((pHatTreatment*(1 - pHatComparator) + pHatComparator*(1 - pHatTreatment))/2))

  result <- data.frame(beforeWeighting,
                       afterWeighting,
                       covariateId = sTreatment$covariateId)
  result$inside <- ifelse((result$beforeWeighting >10 & result$afterWeighting>10), result$covariateId, '')

  p <- ggplot2::ggplot(result, ggplot2::aes(beforeWeighting, afterWeighting)) +
    ggplot2::geom_point(size = 1, color = 'blue', alpha = .5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 10), linetype = 'dashed', color = 'red') +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 10, color = 'red'), linetype = 'dashed', color = 'red') +
    ggplot2::xlab('Before weighting') +
    ggplot2::ylab('After weighting')
  if(showNotBalancedCovariateIds)
    p <- p + ggplot2::geom_text(ggplot2::aes(label=inside), hjust=0, vjust=1.2)

  return(p)


}
