#' Creates a data frame with hazard ratios within risk strata
#'
#' @param ps A list with the propensity data frames as generated from \code{\link[CohortMethod]{createPs}}
#' @param calculateWeights Whether to calculate the weights using \code{\link[RiskStratifiedEstimation]{createIPW}}
#' @param weightsType The type of the weights to be used. Allowed options are 'ATE' for average treatment effect and 'ATT' for average treatment effect on the treated weights
#' @param useStabilizedWeights Should stabilized weights be used?
#' @param truncationLevels The level of truncation expressed in percentiles of the propensity score.
#' @return A data frame with hazard ratios along with confidence intervals
#'
#' @export

relativeRiskReduction <- function(ps,
                                  calculateWeights = TRUE,
                                  weightsType = 'ATE',
                                  useStabilizedWeights = TRUE,
                                  truncationLevels){

  HRDataFrame <- data.frame(HR = numeric(),
                            lower95 = numeric(),
                            upper95 = numeric(),
                            riskStratum = numeric())
  nStrata <- length(ps)
  for(i in 1:length(ps)){

    if(calculateWeights)
      ps[[i]] <- createIPW(ps[[i]],
                           weightsType = weightsType,
                           useStabilizedWeights = useStabilizedWeights,
                           truncationLevels = truncationLevels)


    ps[[i]]$outcomeCount <- ifelse(ps[[i]]$outcomeCount != 0, 1, 0)
    summaryFit <- summary(survival::coxph(survival::Surv(survivalTime, outcomeCount) ~ treatment,
                                          data = ps[[i]],
                                          weights = ps[[i]]$weights, robust = TRUE))

    HRDataFrame[i, ] <- c(summaryFit$conf.int[c(1, 3:4)], i)

  }
  return(HRDataFrame)
}

