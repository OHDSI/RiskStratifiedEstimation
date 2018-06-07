#' Prediction Summary
#'
#' Calulates the AUC and the calibration results based on cross validation
#'
#' @param predictionData The data frame from which the prediction will be calculated
#' @param repsonse The name of the response variable
#' @param predictors The vector with the names of the predictor variables
#' @param nFolds The number of the folds to be used in cross validation
#' @param nQuantiles The number of risk quantiles to be considered
#' @param lims The axes limits on the calibration plot
#'
#' @return The AUC, calibration summary and the calibration plot
#'
#' @export


predictionSummary <- function(predictionData,
                              response,
                              predictors,
                              nFolds = 4,
                              nQuantiles = 10,
                              lims = c(0, 1)){

  folds <- caret::createFolds(1:dim(predictionData)[1],
                              nFolds)
  predictionData <- subset(predictionData, select = c(response, predictors))
  value <- rep(0, dim(predictionData)[1])
  for(i in 1:nFolds){

    cvModel <- glm(as.formula(paste(response, '~', paste(predictors, collapse = '+'))),
                   data = predictionData[-folds[[i]], ],
                   family = binomial)
    value[folds[[i]]] <- predict(cvModel,
                                 dplyr::select(predictionData, predictors)[folds[[i]], ],
                                 type = 'response')

  }
  predictionData$value <- value

  cvAUC <- AUC(logit(predictionData$value), subset(predictionData, select = response))
  calibrationSummary <- matrix(NA, ncol = 5, nrow = nQuantiles)
  colnames(calibrationSummary) <- c('nObs',
                                    'sumPred',
                                    'sumObs',
                                    'averagePred',
                                    'averageObs')
  calibrationSummary <- data.frame(calibrationSummary)
  predictionData <- dplyr::mutate(predictionData,
                                  quant = dplyr::ntile(value, nQuantiles))

  for(i in 1:nQuantiles){

    subPrediction <- subset(predictionData, quant == i)
    calibrationSummary[i, 1] <- dim(subPrediction)[1]
    calibrationSummary[i, 2] <- sum(subPrediction$value)
    calibrationSummary[i, 3] <- sum(subset(subPrediction, select = response))
    calibrationSummary[i, 4] <- calibrationSummary$sumPred[i] / calibrationSummary$nObs[i]
    calibrationSummary[i, 5] <- calibrationSummary$sumObs[i] / calibrationSummary$nObs[i]

  }
  calibrationPlot <- ggplot2::ggplot(calibrationSummary, ggplot2::aes(x = averagePred, y = averageObs)) +
    ggplot2::geom_point(fill = 'blue', color = 'blue', size = 3) +
    ggplot2::geom_line(color = 'red', size = .8, linetype = 'twodash') +
    ggplot2::coord_cartesian(xlim = lims, ylim = lims) +
    ggplot2::geom_abline(slope = 1,intercept = 0, linetype = 'solid', size = .9) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = 'Predicted outcomes', y = 'Observed outcomes')

  return(list(AUC = cvAUC,
              calibrationPlot = calibrationPlot,
              calibrationSummary = calibrationSummary,
              predictionSummary = predictionData))

}

AUC <- function(xb.hat,y){
  n<-length(xb.hat)
  n1<-sum(y)
  mean.rank <- mean(rank(xb.hat)[y == 1])
  AUC<-(mean.rank - (n1 + 1)/2)/(n - n1)
  return(AUC)
}

logit <- function(p) log(p / (1 - p))
