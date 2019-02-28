#' Transforms a cohortMethodData object into a plpData object
#'
#' Transforms a cohortMethodData object, as generated from \code{\link[CohortMethod]{getDbCohortMethodData}}, into a plpData object
#' appropriate for risk stratified analysis
#'
#' @param cohortMethodData The cohortMethodData object to be transformed
#'
#' @return A plpData object
#'
#' @export

cmToPlpData <- function(cohortMethodData){

  names(cohortMethodData$cohorts)[which(names(cohortMethodData$cohorts) == 'treatment')] <- 'cohortId'
  cohortMethodData$cohorts$cohortId <- 0
  class(cohortMethodData) <- 'plpData'


  attr(cohortMethodData$cohorts, 'metaData') <- list(cohortId = paste0(attr(cohortMethodData$cohorts, 'metaData')$targetId,
                                                                       attr(cohortMethodData$cohorts, 'metaData')$comparatorId),
                                                     studyStartDate = attr(cohortMethodData$cohorts, 'metaData')$studyStartDate,
                                                     studyEndDate = attr(cohortMethodData$cohorts, 'metaData')$studyEndDate,
                                                     attrition = data.frame(outcomeId = attr(cohortMethodData$outcomes, 'metaData'),
                                                                            description = attr(cohortMethodData$cohorts, 'metaData')$attrition$description,
                                                                            targetCount =
                                                                              dplyr::mutate(attr(cohortMethodData$cohorts, 'metaData')$attrition,
                                                                                            targetCount = attr(cohortMethodData$cohorts,
                                                                                                               'metaData')$attrition$targetPersons +
                                                                                              comparatorPersons)$targetCount,
                                                                            outcomes = dim(cohortMethodData$outcomes)[1],
                                                                            uniquePeople = length(unique(cohortMethodData$cohorts$subjectId))))

  cohortMethodData$metaData$call <- call("PatientLevelPrediction::getPlpData",
                                         connectionDetails = cohortMethodData$metaData$call$connectionDetails,
                                         cdmDatabaseSchema = cohortMethodData$metaData$call$cdmDatabaseSchema,
                                         cohortId = cohortMethodData$metaData$call$targetId,
                                         outcomeIds = cohortMethodData$metaData$call$outcomeIds,
                                         studyStartDate = cohortMethodData$metaData$call$studyStartDate,
                                         studyEndDate = cohortMethodData$metaData$call$studyEndDate,
                                         cohortDatabaseSchema = cohortMethodData$metaData$call$cohortDatabaseSchema,
                                         cohortTable = cohortMethodData$metaData$call$exposureTable,
                                         outcomeDatabaseSchema = cohortMethodData$metaData$call$outcomeDatabaseSchema,
                                         outcomeTable = cohortMethodData$metaData$call$outcomeTable,
                                         cdmVersion = cohortMethodData$metaData$call$cdmVersion,
                                         firstExposureOnly = cohortMethodData$metaData$call$firstExposureOnly,
                                         washoutPeriod = cohortMethodData$metaData$call$washoutPeriod,
                                         covariateSettings = cohortMethodData$metaData$call$covariateSettings,
                                         excludeDrugsFromCovariates = cohortMethodData$metaData$call$excludeDrugsFromCovariates)
  return(cohortMethodData)
}
