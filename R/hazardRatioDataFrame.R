#' Creates a data frame with hazard ratios within risk strata
#'
#' @param psList A list with the propensity data frames as generated from \code{\link[CohortMethod]{createPs}}
#' @param weightsType The type of weights to be used. Possible values are 'ATE' or 'ATT'
#' @param useSW Whether to use stabilized weights.
#' @param truncatedWeights Whether to truncate the weights
#' @param truncationQuantiles The quantiles used to truncate the weights
#'
#' @return A data frame with hazard ratios along with confidence intervals
#'
#' @export

hazardRatioDataFrame <- function(psList,
                                 weightsType = 'ATE',
                                 useSW = TRUE,
                                 truncatedWeights = TRUE,
                                 truncationQuantiles = c(.01, .99)){

  HRDataFrame <- data.frame(HR = numeric(),
                            lower95 = numeric(),
                            upper95 = numeric(),
                            riskStratum = numeric())
  nStrata <- length(psList)
  for(i in 1:length(psList)){

    ps <- psList[[i]]
    ps <- createIPW(ps,
                    weightsType = weightsType,
                    useSW = useSW,
                    truncatedWeights = truncatedWeights,
                    truncationQuantiles = truncationQuantiles)
    ps$failures <- ifelse(ps$outcomeCount != 0, 1, 0)
    fit <- survival::coxph(survival::Surv(survivalTime, failures) ~ treatment +cluster(subjectId),
                           data = ps,
                           weights = ps$weights)
    confintFit <- exp(confint(fit))
    HRDataFrame[i, ] <-  c(exp(fit$coefficients), confintFit, i)

  }
  HRDataFrame
}

