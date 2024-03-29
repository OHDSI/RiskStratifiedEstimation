% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HelperFunctions.R
\name{prepareForPlpData}
\alias{prepareForPlpData}
\title{Prepares for the running the PatientLevelPrediction package}
\usage{
prepareForPlpData(
  treatmentCohortId,
  comparatorCohortId,
  targetCohortId,
  cohortDatabaseSchema,
  cohortTable,
  resultsDatabaseSchema,
  mergedCohortTable,
  connectionDetails
)
}
\arguments{
\item{treatmentCohortId}{The treatment cohort id}

\item{comparatorCohortId}{The comparator cohort id}

\item{targetCohortId}{The id of the merged cohorts}

\item{cohortDatabaseSchema}{The name of the database schema that is the location
where the cohort data used to define the at risk
cohort is available}

\item{cohortTable}{The table that contains the treatment and comparator
cohorts.}

\item{resultsDatabaseSchema}{The name of the database schema to store the new tables.
Need to have write access.}

\item{mergedCohortTable}{The table that will contain the computeIncidenceAnalysis}

\item{connectionDetails}{The connection details required to connect to adatabase.}
}
\value{
Creates the tables resultsDatabaseSchema.mergedCohortTable,
        resultsDatabaseSchema.attributeDefinitionTable and
        resultsDatabaseSchema.cohortAttributeTable
}
\description{
Prepares for running the PatientLevelPrediction package by merging the treatment
and comparator cohorts and defining a new covariate for treatment.
}
