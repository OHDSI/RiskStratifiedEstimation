#' Creates a data frame with hazard ratios within risk strata
#'
#' @param ps A list with the propensity data frames as generated from \code{\link[CohortMethod]{createPs}}
#' @param calculateWeights Whether to calculate the weights using \code{\link[RiskStratifiedEstimation]{createIPW}}
#' @param weightsType The type of the weights to be used. Allowed options are 'ATE' for average treatment effect and 'ATT' for average treatment effect on the treated weights
#' @param useStabilizedWeights Should stabilized weights be used?
#' @param extremeWeights The way to assess extreme weights. Possible options are 'unadjusted, 'cvLikeTruncation', 'crumpTrimming', 'fixedTruncaiton'
#' @param fixedTruncationLevels The levels for fixed truncation weighting
#' @param truncationLevels The level of truncation expressed in percentiles of the propensity score. Only symmetric truncation is available. E.g. truncationLevels =.01 will assess truncation up to the .99th percentile of ps
#' @param cvLikeRepetitions The number of times to repeat the 2-fold cross-validations
#' @param stepTruncationLevels The steps for the grid of possible truncation levels
#'
#' @return A data frame with hazard ratios along with confidence intervals
#'
#' @export

relativeRiskReduction <- function(ps,
                                  calculateWeights = TRUE,
                                  weightsType = 'ATE',
                                  useStabilizedWeights = TRUE,
                                  extremeWeights,
                                  truncationLevels,
                                  cvLikeRepetitions,
                                  stepTruncationLevels){

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

