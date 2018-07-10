#' Creates Inverse Probability Weights
#'
#' Calcuates inverse probability weights based on the propensity score
#'
#' @param ps A propensity score data frame as created from \code{\link[CohortMethod]{createPs}}
#' @param weightsType The type of the weights to be used. Allowed options are 'ATE' for average treatment effect and 'ATT' for average treatment effect on the treated weights
#' @param useStabilizedWeights Should stabilized weights be used?
#' @param extremeWeights The way to assess extreme weights. Possible options are 'unadjusted, 'cvLikeTruncation', 'crumpTrimming', 'fixedTruncation'
#' @param truncationLevels The level of truncation expressed in percentiles of the propensity score. If extremeWeights is 'fixedTruncation' then the weights will be truncated at the levels defined here. If extremeWeights is 'cvLikeTruncation' then the data adaptive procedure will only assess truncation up to the levels defined here
#' @param cvLikeRepetitions The number of times to repeat the 2-fold cross-validations
#' @param stepTruncationLevels The steps for the grid of possible truncation levels
#'
#' @return The ps data frame provided as input along with a weights column
#'
#' @export

createIPW <- function(ps,
                      weightsType = 'ATE',
                      useStabilizedWeights = TRUE,
                      extremeWeights = NULL,
                      truncationLevels = c(.01, .99),
                      cvLikeRepetitions,
                      stepTruncationLevels){

  if(weightsType == 'ATE')
    ps$weights <- ps$treatment / ps$propensityScore + (1 - ps$treatment) / (1 - ps$propensityScore)
  else
    ps$weights <- ps$treatment + ps$propensityScore*(1 - ps$treatment) / (1 - ps$propensityScore)

  if(useStabilizedWeights){
    ps$stability <- mean(ps$treatment)
    ps$weights <- ps$treatment*ps$weights*ps$stability +
      (1 - ps$treatment)*ps$weights*(1 - ps$stability)
    ps <- dplyr::select(ps, -stability)
  }

  if(extremeWeights == 'cvLikeTruncation'){

    alpha <- cvLikeTruncation(ps = ps,
                              truncationLevels = truncationLevels,
                              stepTruncationLevels = stepTruncationLevels,
                              cvLikeRepetitions = cvLikeRepetitions)

    ps <-  dplyr::mutate(ps, weights = pmin(pmax(weights, quantile(weights, alpha)),
                                            quantile(weights, 1 - alpha)))

  }
  else if(extremeWeights == 'fixedTruncation')
    ps <-  dplyr::mutate(ps, weights = pmin(pmax(weights, quantile(weights, truncationLevels[1])),
                                            quantile(weights, truncationLevels[2])))



  return(ps)

}
