#' Creates a data frame with hazard ratios within risk strata
#'
#' @param ps A list with the propensity data frames as generated from \code{\link[CohortMethod]{createPs}}
#' @param calculateWeights Whether to calculate the weights using \code{\link[RiskStratifiedEstimation]{createIPW}}
#' @param weightsType The type of weights to be used. Possible values are 'ATE' or 'ATT'
#' @param useStabilizedWeights Whether to use stabilized weights.
#' @param truncatedWeights Whether to truncate the weights
#' @param truncationQuantiles The quantiles used to truncate the weights
#'
#' @return A data frame with hazard ratios along with confidence intervals
#'
#' @export

relativeRiskReduction <- function(ps,
                                  calculateWeights = TRUE,
                                  weightsType = 'ATE',
                                  useStabilizedWeights = TRUE,
                                  truncatedWeights = TRUE,
                                  truncationQuantiles = c(.01, .99)){

  HRDataFrame <- data.frame(HR = numeric(),
                            lower95 = numeric(),
                            upper95 = numeric(),
                            riskStratum = numeric())
  nStrata <- length(ps)
  for(i in 1:length(ps)){

    if(is.null(ps[[i]]$weights))
      ps[[i]] <- createIPW(ps[[i]],
                           weightsType = weightsType,
                           useStabilizedWeights = useStabilizedWeights,
                           truncatedWeights = truncatedWeights,
                           truncationQuantiles = truncationQuantiles)


    ps[[i]]$failures <- ifelse(ps[[i]]$outcomeCount != 0, 1, 0)
    fit <- survival::coxph(survival::Surv(survivalTime, failures) ~ treatment +cluster(subjectId),
                           data = ps[[i]],
                           weights = ps$weights)
    confintFit <- exp(confint(fit))
    HRDataFrame[i, ] <-  c(exp(fit$coefficients), confintFit, i)

  }
  return(HRDataFrame)
}

