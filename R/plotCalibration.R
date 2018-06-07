#' Get the Calibration Plot of a Prediction Model
#'
#' Get the calibration plot and the calibration summary of a prediction model
#'
#' @param prediction A prediction dataframe as returned by \code{\link[PatientLevelPrediction]{runPlp}}. The predicted probabilities should be under the name "value"
#' @param nQuantiles The number of quantiles of predicted risk to split the data
#' @param lims The limits on the x and y-axis. Cannot be lower than 0 or higher than 1
#'
#' @return The calibration plot and the calibration summary
#'
#' @export

plotCalibration <- function(prediction,
                            nQuantiles = 10,
                            lims = c(0, 1)){

  if(is.null(prediction$value))
    stop('The predicted probabilities should be under the name: value')

  prediction = dplyr::mutate(prediction, decile = dplyr::ntile(value, nQuantiles))
  calibrationSummary <- matrix(NA, ncol = 5, nrow = nQuantiles)
  colnames(calibrationSummary) <- c('nObs',
                                   'sumPred',
                                   'sumObs',
                                   'averagePred',
                                   'averageObs')
  calibrationSummary <- data.frame(calibrationSummary)
  for(i in 1:nQuantiles){

    subPrediction <- subset(prediction, decile == i)
    calibrationSummary[i, 1] <- dim(subPrediction)[1]
    calibrationSummary[i, 2] <- sum(subPrediction$value)
    calibrationSummary[i, 3] <- sum(subPrediction$outcomeCount)
    calibrationSummary[i, 4] <- calibrationSummary$sumPred[i] / calibrationSummary$nObs[i]
    calibrationSummary[i, 5] <- calibrationSummary$sumObs[i] / calibrationSummary$nObs[i]

  }
  calibrationPlot <- ggplot2::ggplot(calibrationSummary, ggplot2::aes(x = averageObs, y = averagePred)) +
    ggplot2::geom_point(fill = 'blue', color = 'blue', size = 3) +
    ggplot2::geom_line(color = 'red', size = .8, linetype = 'twodash') +
    ggplot2::coord_cartesian(xlim = lims, ylim = lims) +
    ggplot2::geom_abline(slope = 1,intercept = 0, linetype = 'solid', size = .9) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Observed outcomes', y = 'Predicted outcomes')

  return(list(calibrationPlot = calibrationPlot,
              calibrationSummary = calibrationSummary))

}
