#' Creates Inverse Probability Weights
#'
#' Calcuates inverse probability weights based on the propensity score
#'
#' @param ps A propensity score data frame as created from \code{\link[CohortMethod]{createPs}}
#' @param weightsType The type of the weights to be used. Allowed options are 'ATE' for average treatment effect and 'ATT' for average treatment effect on the treated weights
#' @param useSW Should stabilized weights be used?
#' @param truncatedWeights Should truncated weights be used? If FALSE, then stabilized weights are used instead
#' @param truncationQuantiles THe quantiles to perform weight truncation
#'
#' @return The ps data frame provided as input along with a weights column
#'
#' @export

createIPW <- function(ps,
                      weightsType = 'ATE',
                      useSW = TRUE,
                      truncatedWeights = TRUE,
                      truncationQuantiles = c(.01, .99)){

  if(weightsType == 'ATE')
    ps$weights <- ps$treatment / ps$propensityScore + (1 - ps$treatment) / (1 - ps$propensityScore)
  else
    ps$weights <- ps$treatment + ps$propensityScore*(1 - ps$treatment) / (1 - ps$propensityScore)

  if(useSW){
    ps$stability <- glm(treatment ~ 1,
                        family = binomial,
                        data = ps)$fitted.values
    ps$weights <- ps$treatment*ps$weights*ps$stability +
      (1 - ps$treatment)*ps$weights*(1 - ps$stability)
    ps <- dplyr::select(ps, -stability)
  }

  if(truncatedWeights)
    ps <-  dplyr::mutate(ps, weights = pmin(pmax(weights, truncationQuantiles[1]),
                                            truncationQuantiles[2]))

  return(ps)


}
