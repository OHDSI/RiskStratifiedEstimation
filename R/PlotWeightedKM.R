#' Plots the weighted Kaplan-Meier estimate
#'
#' @param dataKM A dataframe containing the weigthed Kaplan-Meier estimates as computed by \code{\link[RiskStratifiedEstimation]{weightedKM}}
#' @param xlim Limits on x-axis
#' @param ylim Limits on y-axis
#' @param ci Should confidence intervals be displayed?
#' @param title The title on the graph
#' @param legend.position The position of the legend on the graph
#'
#' @return A weighted Kaplan-Meier plot
#' @export

plotWeightedKM <- function(dataKM,
                           xlim,
                           ylim,
                           legend.position = c(.15, .1),
                           ci = TRUE,
                           title = NULL){

  p <- ggplot2::ggplot(dataKM, ggplot2::aes(x = time, y = S, group = cohort, color = cohort)) +
    ggplot2::geom_step() +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::xlab('Time') +
    ggplot2::ylab('Survival') +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = legend.position,
                   legend.text = element_text(size = 8, face = "bold"))
  if(ci)
    p <- p + ggplot2::geom_ribbon(aes(ymin=lower, ymax=upper, fill = cohort),
                                  alpha = 0.3,
                                  linetype = 2)
  if(!is.null(title))
    p <- p + ggplot2::ggtitle(title)

  return(p)

}
