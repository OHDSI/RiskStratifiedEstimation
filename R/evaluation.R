#' @importFrom dplyr %>%
#' @export

evaluatePrediction <- function(analysisSettings,
                               predictionId){

  saveDir <- file.path(analysisSettings$saveDirectory,
                       analysisSettings$analysisId)

  plpData <- PatientLevelPrediction::loadPlpData(
    file.path(saveDir,
              "Data",
              "plpData")
  )

  plpResult <- PatientLevelPrediction::loadPlpResult(
    file.path(saveDir,
              "Prediction",
              predictionId,
              analysisSettings$analysisId,
              "plpResult")
  )

  psFull <- readRDS(
    file.path(saveDir,
              "Estimation",
              predictionId,
              "psFull.rds")
  ) %>%
    dplyr::mutate(outcomeCount = ifelse(outcomeCount > 0, 1, 0))

  # Evaluate on matched set
  populationSubset <- CohortMethod::matchOnPs(psFull)
  attr(populationSubset, "metaData") <- list(cohortId = 1,
                                             outcomeId = predictionId)

  modelEvaluationOnSubset <- PatientLevelPrediction::applyModel(population = populationSubset,
                                                                plpData = plpData,
                                                                plpModel = plpResult$model)

  # Save evaluation on matched set
  analysisPath <- file.path(saveDir,
                            "Prediction",
                            predictionId,
                            analysisSettings$analysisId,
                            "Matched")
  analysisPath <- file.path(saveDir,
                            "Prediction",
                            predictionId,
                            analysisSettings$analysisId,
                            "Matched")
  if(!dir.exists(analysisPath)){
    dir.create(analysisPath, recursive=T)
  }
  saveRDS(
    modelEvaluationOnSubset$performanceEvaluation,
    file = file.path(analysisPath,
                     "performanceEvaluation.rds")
  )
  saveRDS(
    modelEvaluationOnSubset$prediction,
    file = file.path(analysisPath,
                     "prediction.rds")
  )
  saveRDS(
    modelEvaluationOnSubset$covariateSummary,
    file = file.path(analysisPath,
                     "covariateSummary.rds")
  )

  # Load entire target population
  population <- plpData$cohorts %>%
    dplyr::right_join(psFull)

  attr(population, "metaData") <- list(cohortId = 1,
                                       outcomeId = predictionId)

  modelEvaluationOnPopulation <- PatientLevelPrediction::applyModel(population = population,
                                                                    plpData = plpData,
                                                                    plpModel = plpResult$model)

  # Save evaluation on the entire target population
  analysisPath <- file.path(saveDir,
                            "Prediction",
                            predictionId,
                            analysisSettings$analysisId,
                            "EntirePopulation")
  if(!dir.exists(analysisPath)){
    dir.create(analysisPath, recursive=T)
  }
  saveRDS(
    modelEvaluationOnPopulation$performanceEvaluation,
    file = file.path(analysisPath,
                     "performanceEvaluation.rds")
  )
  saveRDS(
    modelEvaluationOnPopulation$prediction,
    file = file.path(analysisPath,
                     "prediction.rds")
  )
  saveRDS(
    modelEvaluationOnPopulation$covariateSummary,
    file = file.path(analysisPath,
                     "covariateSummary.rds")
  )


  # Evaluate on treatment arm
  populationSubset <- population %>%
    dplyr::filter(treatment == 1)

  modelEvaluationOnSubset <- PatientLevelPrediction::applyModel(population = populationSubset,
                                                                plpData = plpData,
                                                                plpMode = plpResult$model)

  # Save evaluation on treatment arm
  analysisPath <- file.path(saveDir,
                            "Prediction",
                            predictionId,
                            analysisSettings$analysisId,
                            "Treatment")
  if(!dir.exists(analysisPath)){
    dir.create(analysisPath, recursive=T)
  }
  saveRDS(
    modelEvaluationOnSubset$performanceEvaluation,
    file = file.path(analysisPath,
                     "performanceEvaluation.rds")
  )
  saveRDS(
    modelEvaluationOnSubset$prediction,
    file = file.path(analysisPath,
                     "prediction.rds")
  )
  saveRDS(
    modelEvaluationOnSubset$covariateSummary,
    file = file.path(analysisPath,
                     "covariateSummary.rds")
  )

  # Evaluate on comparator arm
  populationSubset <- population %>%
    dplyr::filter(treatment == 0)

  modelEvaluationOnSubset <- PatientLevelPrediction::applyModel(population = populationSubset,
                                                                plpData = plpData,
                                                                plpModel = plpResult$model)

  # Save evaluation on comparator arm
  analysisPath <- file.path(saveDir,
                            "Prediction",
                            predictionId,
                            analysisSettings$analysisId,
                            "Comparator")
  if(!dir.exists(analysisPath)){
    dir.create(analysisPath, recursive=T)
  }
  saveRDS(
    modelEvaluationOnSubset$performanceEvaluation,
    file = file.path(analysisPath,
                     "performanceEvaluation.rds")
  )
  saveRDS(
    modelEvaluationOnSubset$prediction,
    file = file.path(analysisPath,
                     "prediction.rds")
  )
  saveRDS(
    modelEvaluationOnSubset$covariateSummary,
    file = file.path(analysisPath,
                     "covariateSummary.rds")
  )



  return(NULL)

}




#' @importFrom dplyr %>%
#' @export
plotSmoothCalibration <- function(analysisSettings,
                                  predictionId,
                                  type) {

  ## Add check for type input!!

  analysisPath <- file.path(analysisSettings$saveDir,
                            analysisSettings$analysisId,
                            "Prediction",
                            predictionId,
                            analysisSettings$analysisId)

  if(type == "treatments"){
    analysisPath1 <- file.path(analysisPath,
                               "Treatment")
    analysisPath2 <- file.path(analysisPath,
                               "Comparator")
  }else{
    analysisPath1 <- file.path(analysisPath,
                               "EntirePopulation")
    analysisPath2 <- file.path(analysisPath,
                               "Matched")
  }

  prediction <-readRDS(
    file.path(analysisPath1,
              "prediction.rds")
  ) %>%
    dplyr::bind_rows(
      readRDS(
        file.path(analysisPath2,
                  "prediction.rds")
      ),
      .id = "source"
    )

  evaluation1 <- readRDS(
    file.path(analysisPath1,
              "performanceEvaluation.rds")
  )
  evaluation2 <- readRDS(
    file.path(analysisPath2,
              "performanceEvaluation.rds")
  )

  calibrationSummary <- evaluation1$calibrationSummary %>%
    dplyr::bind_rows(evaluation2$calibrationSummary,
                     .id = "source")

  maxVal <- max(calibrationSummary$averagePredictedProbability, calibrationSummary$observedIncidence)
  maxes <- max(max(calibrationSummary$averagePredictedProbability), max(calibrationSummary$observedIncidence))

  y1 <- prediction %>%
    filter(source == 1) %>%
    select(outcomeCount) %>%
    unlist()
  names(y1) <- NULL
  y2 <- prediction %>%
    filter(source == 2) %>%
    select(outcomeCount) %>%
    unlist()
  names(y2) <- NULL
  p1 <- prediction %>%
    filter(source == 1) %>%
    select(value) %>%
    unlist()
  names(p1) <- NULL
  p2 <- prediction %>%
    filter(source == 2) %>%
    select(value) %>%
    unlist()
  names(p2) <- NULL

  logit1 <- log(p1/(1 - p1))  # delete cases with 0 and 1 probs
  logit2 <- log(p2/(1 - p2))
  nonInf1 <- !is.infinite(logit1)
  nonInf2 <- !is.infinite(logit2)
  sumNonInf <- sum(!nonInf1) + sum(!nonInf2)
  if (sumNonInf > 0)
    warning(paste(sumNonInf, "observations deleted due to probabilities of 0 or 1"))
  y1 <- y1[nonInf1]
  p1 <- p1[nonInf1]
  y2 <- y2[nonInf2]
  p2 <- p2[nonInf2]

  y1 <- y1[order(p1)]
  p1 <- p1[order(p1)]
  y2 <- y2[order(p2)]
  p2 <- p2[order(p2)]


  # Restricted cubic splines

  expit <- function(x) exp(x)/(1 + exp(x))
  dd <- data.frame(y = y1, p = p1) %>%
    dplyr::bind_rows(
      data.frame(
        y = y2,
        p = p2),
      .id = "source")

  smoothFit1 <- rms::lrm(y1 ~ rms::rcs(p1, 3),
                         x = T,
                         y = T)

  smoothFit2 <- rms::lrm(y2 ~ rms::rcs(p2, 3),
                         x = T,
                         y = T)

  # create the rcs mapping
  xRange1 <- seq(0, p1[length(p1)], length.out = 1000)
  pred1 <- stats::predict(smoothFit1, xRange1, se.fit = T, type = "lp")
  xRange2 <- seq(0, p2[length(p2)], length.out = 1000)
  pred2 <- stats::predict(smoothFit2, xRange2, se.fit = T, type = "lp")
  predXRange1 <- expit(pred1$linear.predictors)
  predXRange2 <- expit(pred2$linear.predictors)

  smoothData <- data.frame(xRange = xRange1,
                           predXRange = predXRange1) %>%
    dplyr::bind_rows(
      data.frame(
        xRange = xRange2,
        predXRange = predXRange2
      ),
      .id = "source"
    )

  if(type == "treatments"){
    smoothData <- smoothData %>%
      dplyr::mutate(source = ifelse(source == 1, "treatment", "comparator"))
  }else{
    smoothData <- smoothData %>%
      dplyr::mutate(source = ifelse(source == 1, "Overall", "Matched"))
  }


  # the main plot object, this one uses RCS
  smooth_plot <- ggplot2::ggplot(data = smoothData, ggplot2::aes(x = xRange, y = predXRange, color = source)) +
    ggplot2::geom_line() +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, color = 2) +
    ggplot2::labs(x = "Predicted probability", y = "Observed incidence")

  return(smooth_plot)
}

#' @importFrom dplyr %>%
#' @export
computeCovariateBalanceWeighted <- function(population,
                                            cohortMethodData){

  pops <- population %>%
    dplyr::group_by(treatment) %>%
    dplyr::summarise(n = n(),
              nWeighted = sum(weights))

  covariates <- cohortMethodData$covariates[]
  tt <- covariates %>%
    dplyr::filter(rowId %in% population$rowId) %>%
    dplyr::left_join(population %>%
                       select(c("rowId", "treatment", "weights")))

  test = tt %>%
    dplyr::group_by(treatment, covariateId) %>%
    dplyr::summarise(covariateSum = sum(covariateValue)) %>%
    dplyr::left_join(pops) %>%
    dplyr::mutate(unweighted = covariateSum/n,
                  weighted = covariateSum/nWeighted,
                  sdWeighted = weighted*(1 - weighted)/2,
                  sdUnweighted = unweighted*(1 - unweighted)/2) %>%
    as.data.frame()

  t1 <- test %>%
    dplyr::mutate(treatment = ifelse(treatment == 1, 1, -1)) %>%
    dplyr::group_by(covariateId) %>%
    dplyr::summarise(beforeWeighting = 100*abs(sum(unweighted*treatment))/sqrt(sum(sdUnweighted))) %>%
    as.data.frame()
  t2 <- test %>%
    dplyr::mutate(treatment = ifelse(treatment == 1, 1, -1)) %>%
    dplyr::group_by(covariateId) %>%
    dplyr::summarise(afterWeighting = 100*abs(sum(weighted*treatment))/sqrt(sum(sdWeighted))) %>%
    as.data.frame()
  t1 %>%
    dplyr::left_join(t2) %>%
    ggplot2::ggplot(ggplot2::aes(x = beforeWeighting, y = afterWeighting)) +
    ggplot2::geom_point(size = .5) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::scale_x_continuous(limits = c(0, 100)) +
    ggplot2::geom_hline(yintercept = 10, linetype = 2, color = "red") +
    ggplot2::xlab(label = "Before weighting") +
    ggplot2::ylab(label = "After weighting")
}
