#' Calculates the cv-like truncation level
#'
#' Calculates the cv-like truncation level using 2-fold cross-validation for a specified number of repetitions
#'
#' @param ps A propensity score data frame as created from \code{\link[CohortMethod]{createPs}}
#' @param truncationLevels The level of truncation expressed in percentiles of the propensity score. Only symmetric truncation is available. E.g. truncationLevels =.01 will assess truncation up to the 99th percentile of ps
#' @param stepTruncationLevels The steps for the grid of possible truncation levels
#' @param cvLikeRepetitions The number of times to repeat the 2-fold cross-validations
#'
#' @return The truncation quantile
#' @export

cvLikeTruncation <- function(ps,
                             truncationLevels,
                             stepTruncationLevels,
                             cvLikeRepetitions = 50){

  nObservationsPs <- dim(ps)[1]
  psTemp <- ps
  ps$failures <- ifelse(ps$outcomeCount != 0, 1, 0)
  mseEstimate <- rep(0, length(seq(0, truncationLevels, stepTruncationLevels)))
  l <- 1
  seqTruncationLevels <- seq(0, truncationLevels, stepTruncationLevels)

  pb <- txtProgressBar(max = truncationLevels, style=3)
  progress <- function(n) setTxtProgressBar(pb, n)

  for(k in seqTruncationLevels){
    psTemp <- dplyr::mutate(psTemp, weights = pmin(pmax(weights, quantile(weights, k)),
                                                   quantile(weights, 1 - k)))
    varTemp <- survival::coxph(survival::Surv(survivalTime, failures) ~ treatment + cluster(subjectId),
                               data = psTemp,
                               weights = weights)$var
    s <- 0

    for(i in 1:cvLikeRepetitions){

      keepLines <- rep(0, nObservationsPs)

      cvLikeSplit <- sample(x = 1:nObservationsPs,
                            size = round(nObservationsPs/2),
                            replace = FALSE)
      keepLines[cvLikeSplit] <- 1
      betaTrue <- survival::coxph(survival::Surv(survivalTime, failures) ~ treatment + cluster(subjectId),
                                  data = ps[which(keepLines == 1), ],
                                  weights = weights)$coefficients

      betaHat <- survival::coxph(survival::Surv(survivalTime, failures) ~ treatment + cluster(subjectId),
                                 data = psTemp[which(keepLines == 0), ],
                                 weights = weights)$coefficients

      bias1 <- (betaHat - betaTrue)^2

      betaTrue <- survival::coxph(survival::Surv(survivalTime, failures) ~ treatment + cluster(subjectId),
                                  data = ps[which(keepLines == 0), ],
                                  weights = weights)$coefficients

      betaHat <- survival::coxph(survival::Surv(survivalTime, failures) ~ treatment + cluster(subjectId),
                                 data = psTemp[which(keepLines == 1), ],
                                 weights = weights)$coefficients
      bias2 <- (betaHat - betaTrue)^2

      s <- s + (bias1 + bias2)/2/folds

    }
    mseEstimate[l] <- varTemp + s
    l <- l + 1
    progress(k)
  }

  return(seqTruncationLevels[which.min(mseEstimate)])
}
