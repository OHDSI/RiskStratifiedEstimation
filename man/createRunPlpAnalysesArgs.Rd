% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateArgFuns.R
\name{createRunPlpAnalysesArgs}
\alias{createRunPlpAnalysesArgs}
\title{Create a parameter object for the function runPlp}
\usage{
createRunPlpAnalysesArgs(
  outcomeId = NULL,
  splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
  sampleSettings = PatientLevelPrediction::createSampleSettings(),
 
    featureEngineeringSettings = PatientLevelPrediction::createFeatureEngineeringSettings(),
  preprocessSettings = PatientLevelPrediction::createPreprocessSettings(),
  modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
  logSettings = PatientLevelPrediction::createLogSettings(),
  executeSettings = PatientLevelPrediction::createDefaultExecuteSettings(),
  matchingSettings = CohortMethod::createMatchOnPsArgs(),
  timepoint = NULL
)
}
\arguments{
\item{outcomeId}{The outcomeId for the prediction.}

\item{splitSettings}{An object of type splitSettings that specifies
how to split the data into train/validation/test.
The default settings can be created using
\code{\link[PatientLevelPrediction]{createDefaultSplitSetting}}}

\item{sampleSettings}{An object of type \code{sampleSettings} that specifies any
under/over sampling to be done. Should be created with
\code{\link[PatientLevelPrediction]{createSampleSettings}}}

\item{featureEngineeringSettings}{An object of \code{featureEngineeringSettings} specifying
any feature engineering to be learned (using the train data)}

\item{preprocessSettings}{An object of \code{preprocessSettings}. This setting specifies
the minimum fraction of target population who must have
a covariate for it to be included in the model training
and whether to normalise the covariates before training.
Should be created with
\code{\link[PatientLevelPrediction]{createPreprocessSettings}}.}

\item{modelSettings}{An object of class \code{modelSettings}.}

\item{logSettings}{An object of \code{logSettings} created using
\code{\link[PatientLevelPrediction]{createLogSettings}} specifying
how the logging is done.}

\item{executeSettings}{An object of type \code{executeSettings} specifying which parts
of the analysis to run. Should be created using
\code{\link[PatientLevelPrediction]{createExecuteSettings}}.}

\item{matchingSettings}{The settings for the construction of the population on which the
prediction model will be developed.}

\item{timepoint}{The timepoint to predict risk (survival models only)}
}
\description{
Create a parameter object for the function runPlp
}
\details{
Create an object defining the parameter values.
}
