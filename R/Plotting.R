#' Plot the results of a risk stratified analysis
#'
#' Plots the overall results of a risk stratified analysis.
#'
#' @param rseeResult                    The overall result of a risk stratified analysis.
#' @param mapOutcomes                   A dataframe with the outcome labels. It should have 2 columns called "outcome" and "labelOutcome".
#'                                      The former should contain character values of the form "outcome_x" and the latter should contain
#'                                      the outcome label. If set to \code{NULL}, the outcome defintion id's appear in the graphs.
#' @param mapTreatments                 A dataframe with the treatment labels. It should have 2 columns named "cohort" and "labelTreatments".
#'                                      The former should contain the values "treatment" and "comparator" and the latter should contain
#'                                      the treatment labels. If set to \code{NULL}, the labels treatment and comaprator appear in
#'                                      the graph,
#'
#' @return                              A 3-level graph. In the first level observed outcome rates are presented for all treatment-outcome
#'                                      combinations across risk strata. In the second level, hazard ratios across risk strata are given.
#'                                      In the final level, absolute risk difference across risk strata are presented.
#'
#' @export

plotRSEE <- function(rseeResult,
                     mapOutcomes = NULL,
                     mapTreatments = NULL){


  nPlots <- length(rseeResult$results)
  plots <- list()

  for(i in 1:nPlots){

    otherRelativeList <- list()
    otherAbsoluteList <- list()
    otherCasesList <- list()
    mainRelative <- rseeResult$results[[i]]$mainOutcome$relative
    mainAbsolute <- rseeResult$results[[i]]$mainOutcome$absolute
    mainCases <- rseeResult$results[[i]]$mainOutcome$cases
    mainCases$outcome <- mainRelative$outcome <-  mainAbsolute$outcome <- names(rseeResult$results)[i]

    for(j in 1:length(rseeResult$results[[i]]$otherOutcomes)){

      otherRelativeList[[j]] <- rseeResult$results[[i]]$otherOutcomes[[j]]$relative
      otherAbsoluteList[[j]] <- rseeResult$results[[i]]$otherOutcomes[[j]]$absolute
      otherCasesList[[j]] <- rseeResult$results[[i]]$otherOutcomes[[j]]$cases

    }
    bindOtherRelative <- dplyr::bind_rows(otherRelativeList, .id = "outcome")
    bindOtherRelative$outcome <- names(rseeResult$results[[i]]$otherOutcomes)[as.numeric(bindOtherRelative$outcome)]
    relative <- rbind(mainRelative, bindOtherRelative)
    if(!is.null(mapOutcomes)){
      relative <- merge(relative, mapOutcomes)
    }


    bindOtherAbsolute <- dplyr::bind_rows(otherAbsoluteList, .id = "outcome")
    bindOtherAbsolute$outcome <- names(rseeResult$results[[i]]$otherOutcomes)[as.numeric(bindOtherAbsolute$outcome)]
    absolute <- rbind(mainAbsolute, bindOtherAbsolute)
    if(!is.null(mapOutcomes)){
      absolute <- merge(absolute, mapOutcomes)
      absolute$outcome <- absolute$labelOutcome
    }

    bindOtherCases <- dplyr::bind_rows(otherCasesList, .id = "outcome")
    bindOtherCases$outcome <- names(rseeResult$results[[i]]$otherOutcomes)[as.numeric(bindOtherCases$outcome)]
    cases <- rbind(mainCases, bindOtherCases)
    cases <-  reshape::melt(cases,
                            id.vars = c("outcome", "riskStratum"),
                            measure.vars = c("comparator", "treatment"))
    names(cases)[which(names(cases)=="variable")] <- "cohort"
    if(!is.null(mapTreatments)){
      cases <- merge(cases, mapTreatments)
      cases$cohort <- cases$labelTreatments
    }
    if(!is.null(mapOutcomes)){
      cases <- merge(cases, mapOutcomes)
      cases$outcome <- cases$labelOutcome
    }
    cases$test <- paste(cases$outcome, cases$cohort)

    ylimCases <- c(0, max(cases$value))
    ylimRRR <- c(min(relative$lower), max(relative$upper))
    ylimARR <- c(min(absolute$lower), max(absolute$upper))

    casesPlot <- ggplot2::ggplot(data = cases, ggplot2::aes(x = riskStratum, y = value)) +
      ggplot2::geom_bar(stat = 'identity', position = ggplot2::position_dodge(), ggplot2::aes(fill = test), width = .5)+
      ggplot2::xlab('Risk Stratum') +
      ggplot2::ylab('Outcome Rate') +
      ggplot2::geom_hline(yintercept = 0, size = .8) +
      ggplot2::coord_cartesian(ylim = ylimCases) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     legend.direction = 'horizontal',
                     legend.position = 'top') + ggplot2::scale_y_reverse()

    rrrPlot <- ggplot2::ggplot(relative, ggplot2::aes(x = riskStratum,
                                                      y = HR,
                                                      group = outcome,
                                                      color = outcome)) +
      ggplot2::geom_point(ggplot2::aes(color  = outcome, shape = outcome),
                          size = 3,
                          position = ggplot2::position_dodge(w = .3)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                             width = 0,
                             position = ggplot2::position_dodge(w = .3)) +
      ggplot2::geom_hline(yintercept = 1, linetype = 'dashed', size = .8) +
      ggplot2::xlab('Risk Stratum') +
      ggplot2::ylab('Hazard Ratio') +
      ggplot2::coord_cartesian(ylim = ylimRRR) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     legend.position = 'none',
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank())

    arrPlot <- ggplot2::ggplot(absolute, ggplot2::aes(x = riskStratum,
                                                      y = ARR,
                                                      group = outcome,
                                                      color = outcome)) +
      ggplot2::geom_point(ggplot2::aes(color= outcome, shape = outcome),
                          size = 3,
                          position = ggplot2::position_dodge(w = .3)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                             width = 0,
                             position = ggplot2::position_dodge(w = .3)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', size = .8) +
      ggplot2::xlab('Risk Stratum') +
      ggplot2::ylab('Absolute \n Risk Reduction') +
      ggplot2::coord_cartesian(ylim = ylimARR) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.direction = 'horizontal',
                     legend.position = 'bottom',
                     legend.title = ggplot2::element_blank())

    # plots[[i]] <- grid::grid.draw(rbind(ggplot2::ggplotGrob(rrrPlot), ggplot2::ggplotGrob(arrPlot), size = "last"))
    plots[[i]] <- ggpubr::ggarrange(casesPlot, rrrPlot, arrPlot, nrow = 3, align = "v")
  }

  names(plots) <- names(rseeResult$results)

  return(plots)
}




#' Plots the covariate balance before and after balancing
#'
#' Plots covariate before and after weighting using the inverse of the propensity score
#'
#' @param ps A propensity score data frame as created from \code{\link[CohortMethod]{createPs}}
#' @param cohortMethodData A cohortMethodData object
#' @param calculateWeights Should the weights be calculated?
#' @param weightsType The type of the weights to be used. Allowed options are 'ATE' for average treatment effect and 'ATT' for average treatment effect on the treated weights
#' @param useStabilizedWeights Should stabilized weights be used?
#' @param truncationLevels The level of truncation expressed in percentiles of the propensity score.
#' @param showNotBalancedCovariateIds Show covariate ids that were not balanced after weighting?
#'
#' @return The covariate balance plot
#' @export



plotCovariateBalance <- function(ps,
                                 cohortMethodData,
                                 calculateWeights = TRUE,
                                 weightsType = 'ATE',
                                 useStabilizedWeights = TRUE,
                                 truncationLevels,
                                 showNotBalancedCovariateIds = TRUE){

  if(calculateWeights)
    ps <- createIPW(ps,
                    weightsType = weightsType,
                    useStabilizedWeights = useStabilizedWeights,
                    truncationLevels = truncationLevels)

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
  axisLimits <- c(0, max(max(beforeWeighting, na.rm = TRUE), max(afterWeighting, na.rm = TRUE)))

  result <- data.frame(beforeWeighting,
                       afterWeighting,
                       covariateId = sTreatment$covariateId)
  result$inside <- ifelse((result$beforeWeighting >10 & result$afterWeighting>10), result$covariateId, '')

  p <- ggplot2::ggplot(result, ggplot2::aes(beforeWeighting, afterWeighting)) +
    ggplot2::geom_point(size = 1, color = 'blue', alpha = .5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 10),
                        linetype = 'dashed', color = 'red', alpha = .5, size = 1.1) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 10, color = 'red'),
                        linetype = 'dashed', color = 'red', alpha = .5, size = 1.1) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
    ggplot2::ylim(axisLimits) +
    ggplot2::xlim(axisLimits) +
    ggplot2::xlab('Before weighting') +
    ggplot2::ylab('After weighting')
  if(showNotBalancedCovariateIds)
    p <- p + ggplot2::geom_text(ggplot2::aes(label=inside), hjust=0, vjust=1.2)

  return(p)
}




#' Plot a single outcome
#'
#' Plots the result of a risk stratified analysis for a single outcome of interest
#'
#' @param outcomeId                 The outcome Id of interest
#' @param mapMatrix                 The matrix that maps patients to their risk quantiles
#' @param title                     The title of the graph
#' @param overallResult             The hazard ratio from an analysis on the overall study population
#' @param mapTreatments             A dataframe with the treatment labels. It should have 2 columns named "cohort" and "labelTreatments".
#'                                  The former should contain the values "treatment" and "comparator" and the latter should contain
#'                                  the treatment labels. If set to \code{NULL}, the labels treatment and comaprator appear in
#'                                  the graph.
#' @return                          A 3-level graph for a single outcome. In the first level observed outcome rates are presented
#'                                  for all treatment-outcome combinations across risk strata. In the second level, hazard ratios
#'                                  across risk strata are given. In the final level, absolute risk difference across risk strata are presented.
#' @importFrom dplyr %>%
#'
#' @export

singlePlotRSEE <- function(outcomeId,
                           mapMatrix,
                           title = NULL,
                           overallResult = NULL,
                           mapTreatments = NULL){



  mapMatrix <- mapMatrix %>%
    mutate(riskStratum = paste0("Q", riskStratum)) %>%
    group_by(riskStratum) %>%
    summarise(meanRisk = round(median(value), 3))

  relative <- outcomeId$relative
  relative <- merge(relative, mapMatrix)

  absolute <- outcomeId$absolute
  absolute <- merge(absolute, mapMatrix)

  cases <- outcomeId$cases
  cases <- merge(cases, mapMatrix)

  cases <- merge(cases, mapMatrix)
  cases <-  reshape::melt(cases,
                          id.vars = c("riskStratum"),
                          measure.vars = c("comparator", "treatment"))
  names(cases)[which(names(cases)=="variable")] <- "cohort"
  if(!is.null(mapTreatments)){
    cases <- merge(cases, mapTreatments)
    cases$cohort <- cases$labelTreatments
  }

  cases$test <- paste(cases$outcome, cases$cohort)
  cases <- merge(cases, mapMatrix)

  ylimCases <- c(0, max(cases$value))*100
  ylimRRR <- c(min(relative$lower), max(relative$upper))
  ylimARR <- c(min(absolute$lower), max(absolute$upper))*100



  casesPlot <- ggplot2::ggplot(data = cases, ggplot2::aes(x = factor(meanRisk*100), y = value*100)) +
    ggplot2::geom_bar(stat = 'identity', position = ggplot2::position_dodge(), ggplot2::aes(fill = test), width = .5)+
    ggplot2::xlab('Risk Stratum') +
    ggplot2::ylab('Outcome Rate (%)') +
    ggplot2::geom_hline(yintercept = 0, size = .8) +
    ggplot2::coord_cartesian(ylim = ylimCases) +
    ggplot2::scale_fill_brewer(palette="Paired") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   legend.direction = 'horizontal',
                   legend.position = 'top') + ggplot2::scale_y_reverse()
  if(!is.null(title))
    casesPlot <- casesPlot +
    ggplot2::ggtitle(title)

  rrrPlot <- ggplot2::ggplot(relative, ggplot2::aes(x = factor(meanRisk*100),
                                                    y = HR)) +
    ggplot2::geom_point(size = 3,
                        position = ggplot2::position_dodge(w = .3)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                           width = 0,
                           position = ggplot2::position_dodge(w = .3)) +
    ggplot2::geom_hline(yintercept = 1, linetype = 'dashed', size = .8) +
    ggplot2::xlab('Risk Stratum') +
    ggplot2::ylab('Hazard Ratio') +
    ggplot2::coord_cartesian(ylim = ylimRRR) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = 'none',
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank())
  if(!is.null(overallResult))
    rrrPlot <- rrrPlot +
    ggplot2::geom_hline(yintercept = overallResult, linetype = 'dashed', size = .8, color = 2, alpha = .4)


  arrPlot <- ggplot2::ggplot(absolute, ggplot2::aes(x = factor(meanRisk*100),
                                                    y = ARR*100)) +
    ggplot2::geom_point(size = 3,
                        position = ggplot2::position_dodge(w = .3)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower*100, ymax = upper*100),
                           width = 0,
                           position = ggplot2::position_dodge(w = .3)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', size = .8) +
    ggplot2::xlab('Risk stratum median risk (%)') +
    ggplot2::ylab('Absolute \n Risk Reduction (%)') +
    ggplot2::coord_cartesian(ylim = ylimARR) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.direction = 'horizontal',
                   legend.position = 'bottom',
                   legend.title = ggplot2::element_blank())

  # plots[[i]] <- grid::grid.draw(rbind(ggplot2::ggplotGrob(rrrPlot), ggplot2::ggplotGrob(arrPlot), size = "last"))
  ggpubr::ggarrange(casesPlot, rrrPlot, arrPlot, nrow = 3, align = "v")
}
