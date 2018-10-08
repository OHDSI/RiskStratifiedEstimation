#' Calculates and plots weighted Kaplan-Meier estimates
#'
#' Calculates the weighted Kaplan-Meier estimates based on: Xie J, Liu C. Adjusted Kaplan-Meier estimator and log-rank test with inverse probability of treatment weighting for survival data. Statistics in Medicine 2005; 2:3089–3110.
#'
#' @param ps A data frame including the propensity scores as generated from \code{\link[CohortMethod]{createPs}}
#' @param calculateWeights Whether to calculate the weights using \code{\link[RiskStratifiedEstimation]{createIPW}}
#' @param weightsType The type of the weights to be used. Allowed options are 'ATE' for average treatment effect and 'ATT' for average treatment effect on the treated weights
#' @param useStabilizedWeights Should stabilized weights be used?
#' @param truncationLevels The level of truncation expressed in percentiles of the propensity score.
#'
#' @return A data frame with the Kaplan-Meier estimates
#'
#' @export

weightedKM <- function(ps,
                       calculateWeights = TRUE,
                       weightsType = 'ATE',
                       useStabilizedWeights = TRUE,
                       truncationLevels){

  if(calculateWeights)
    ps <- createIPW(ps,
                    weightsType = weightsType,
                    useStabilizedWeights = useStabilizedWeights,
                    truncationLevels = truncationLevels)

  ps <- subset(ps, select = c('subjectId', 'treatment', 'outcomeCount', 'daysToEvent', 'survivalTime', 'weights'))
  ps$failure <- ifelse(is.na(ps$daysToEvent), 0, 1)
  ps$outcomeCount <- ifelse(ps$outcomeCount>0, 1, 0)

  psTreatment <- subset(ps, treatment == 1)
  psTreatment <- dplyr::arrange(psTreatment, survivalTime)
  p1 <- length(psTreatment$weights)

  psComparator <- subset(ps, treatment == 0)
  psComparator <- dplyr::arrange(psComparator, survivalTime)
  p0 <- length(psComparator$weights)
  s <- 1
  S <- 1
  V <- 0
  dataTreatment <- data.frame(time = numeric(),
                              S = numeric(),
                              varS = numeric(),
                              lower = numeric(),
                              upper = numeric(),
                              eventTime = numeric())

  for(i in unique(psTreatment$survivalTime)){

    j <- which(psTreatment$survivalTime == i)
    v <- sum(psTreatment$failure[j]*psTreatment$weights[j])
    eventTime <- ifelse(v>0, 1, 0)
    y <- tail(cumsum(psTreatment$weights[min(j):p1]), n = 1)
    M <- y^2/tail(cumsum(psTreatment$weights[min(j):p1]^2), n = 1)
    s_j <- 1 - v/y
    V <- V + (1 - s_j)/M/s_j
    S <- S*s_j
    varS <- S^2*V
    lower = exp(-exp(log(-log(S)) - 1.96*sqrt(V)/log(S)))
    upper = exp(-exp(log(-log(S)) + 1.96*sqrt(V)/log(S)))
    dataTreatment[s, ] <- c(i, S, varS, lower, upper, eventTime)
    s <- s + 1

  }

  s <- 1
  S <- 1
  V <- 0
  dataComparator <- data.frame(time = numeric(),
                               S = numeric(),
                               varS = numeric(),
                               lower = numeric(),
                               upper = numeric(),
                               eventTime = numeric())

  for(i in unique(psComparator$survivalTime)){

    j <- which(psComparator$survivalTime == i)
    v <- sum(psComparator$failure[j]*psComparator$weights[j])
    eventTime <- ifelse(v>0, 1, 0)
    y <- tail(cumsum(psComparator$weights[min(j):p0]), n = 1)
    M <- y^2/tail(cumsum(psComparator$weights[min(j):p0]^2), n = 1)
    s_j <- 1 - v/y
    V <- V + (1 - s_j)/M/s_j
    S <- S*s_j
    varS <- S^2*V
    lower = exp(-exp(log(-log(S)) - 1.96*sqrt(V)/log(S)))
    upper = exp(-exp(log(-log(S)) + 1.96*sqrt(V)/log(S)))
    dataComparator[s, ] <-c(i, S, varS, lower, upper, eventTime)
    s <- s + 1

  }

  dataCombined <- dplyr::bind_rows(list(treatment=dataTreatment,
                                        comparator = dataComparator),
                                   .id = 'cohort')


  return(dataCombined)

}
