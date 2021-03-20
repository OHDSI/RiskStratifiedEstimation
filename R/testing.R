# fitPsModel <- function(
#   outcomeId,
#   runSettings,
#   getDataSettings,
#   populationSettings,
#   analysisSettings
# )
# {
#
#   analysisPath <- file.path(
#     analysisSettings$saveDirectory,
#     analysisSettings$analysisId
#   )
#
#   # runSettings$runCmSettings <- runSettings$runCmSettings[[1]]
#
#   ParallelLogger::logInfo(
#     "Reading cohort method data"
#   )
#
#   cohortMethodData <- CohortMethod::loadCohortMethodData(
#     file = getDataSettings$cohortMethodDataFolder
#   )
#
#   ParallelLogger::logInfo(
#     "Reading patient level prediction data"
#   )
#
#   plpData <- PatientLevelPrediction::loadPlpData(
#     file = getDataSettings$plpDataFolder
#   )
#
#   analysisLabels <- unlist(
#     rlist::list.map(                     # extract the second element of a
#       runSettings$runCmSettings$analyses,         # list of lists (here the label)
#       label
#     )
#   )
#
#   names(analysisLabels) <- NULL
#
#   ParallelLogger::logInfo(
#     "Generating the prediction population"
#   )
#
#   populationPlp <- PatientLevelPrediction::createStudyPopulation(
#     plpData = plpData,
#     outcomeId = outcomeId,
#     binary = populationSettings$populationPlpSettings$binary,
#     includeAllOutcomes = populationSettings$populationPlpSettings$includeAllOutcomes,
#     firstExposureOnly = populationSettings$populationPlpSettings$firstExposureOnly,
#     washoutPeriod = populationSettings$populationPlpSettings$washoutPeriod,
#     removeSubjectsWithPriorOutcome = populationSettings$populationPlpSettings$removeSubjectsWithPriorOutcome,
#     priorOutcomeLookback = populationSettings$populationPlpSettings$priorOutcomeLookback,
#     requireTimeAtRisk = populationSettings$populationPlpSettings$requireTimeAtRisk,
#     minTimeAtRisk = populationSettings$populationPlpSettings$minTimeAtRisk,
#     riskWindowStart = populationSettings$populationPlpSettings$riskWindowStart,
#     startAnchor = populationSettings$populationPlpSettings$startAnchor,
#     riskWindowEnd = populationSettings$populationPlpSettings$riskWindowEnd,
#     endAnchor = populationSettings$populationPlpSettings$endAnchor,
#     verbosity = populationSettings$populationPlpSettings$verbosity
#   )
#
#   ParallelLogger::logInfo(
#     "Generating the estimation poppulation"
#   )
#
#   populationCm <- CohortMethod::createStudyPopulation(
#     cohortMethodData = cohortMethodData,
#     outcomeId = outcomeId,
#     firstExposureOnly = populationSettings$populationCmSettings$firstExposureOnly,
#     restrictToCommonPeriod = populationSettings$populationCmSettings$restrictToCommonPeriod,
#     washoutPeriod = populationSettings$populationCmSettings$washoutPeriod,
#     removeDuplicateSubjects = populationSettings$populationCmSettings$removeDuplicateSubjects,
#     removeSubjectsWithPriorOutcome = populationSettings$populationCmSettings$removeSubjectsWithPriorOutcome,
#     priorOutcomeLookback = populationSettings$populationCmSettings$priorOutcomeLookback,
#     minDaysAtRisk = populationSettings$populationCmSettings$minDaysAtRisk,
#     riskWindowStart = populationSettings$populationCmSettings$riskWindowStart,
#     startAnchor = populationSettings$populationCmSettings$startAnchor,
#     riskWindowEnd = populationSettings$populationCmSettings$riskWindowEnd,
#     endAnchor = populationSettings$populationCmSettings$endAnchor,
#     censorAtNewRiskWindow = populationSettings$populationCmSettings$censorAtNewRiskWindow
#   )
#
#   populationCmMetaData <- attr(
#     populationCm,
#     "metaData"
#   )
#
#   attr(populationCm, "metaData") <- attr(
#     populationPlp,
#     "metaData"
#   )
#
#   ParallelLogger::logInfo(
#     "Loading the prediction result"
#   )
#
#   pathToPlpResult <- runSettings$runPlpSettings$plpResults %>%
#     dplyr::filter(
#       outcomeId == !!outcomeId
#     ) %>%
#     dplyr::select(
#       "directory"
#     ) %>%
#     unlist() %>%
#     as.character()
#
#   predictionResult <- PatientLevelPrediction::loadPlpResult(
#     file.path(
#       pathToPlpResult,
#       "plpResult"
#     )
#   )
#
#   ParallelLogger::logInfo(
#     "Predicting on the estimation population"
#   )
#
#   riskPredictions <- predictionResult$model$predict(
#     plpData = plpData,
#     population = populationCm
#   ) %>%
#     dplyr::tibble() %>%
#     dplyr::select(
#       rowId,
#       subjectId,
#       value
#     )
#
#   attr(populationCm, "metaData") <- populationCmMetaData  # Delete that?
#
#   ParallelLogger::logInfo(
#     "Stratifying estimation population"
#   )
#
#   testFunction <- function(
#     label,
#     runSettings,
#     riskPredictions
#   ) {
#     analysis <- runSettings$runCmSettings$analyses[[label]]
#     mapMatrix <- createMapMatrix(
#       riskPredictions = riskPredictions,
#       analysis        = analysis
#     )
#
#     nRiskStrata <- ifelse(
#       analysis$riskStratificationMethod == "equal",
#       yes = analysis$riskStratificationThresholds,
#       no  = length(analysis$riskStratificationThresholds) + 1
#     )
#
#     ps <- list()
#     for (i in 1:nRiskStrata) {
#       population <- populationCm[populationCm$rowId %in% mapMatrix[mapMatrix$riskStratum == i,]$rowId, ]
#       ps[[i]] <- CohortMethod::createPs(
#         cohortMethodData = cohortMethodData,
#         population = population,
#         excludeCovariateIds = runSettings$runCmSettings$psSettings$excludeCovariateIds,
#         includeCovariateIds = runSettings$runCmSettings$psSettings$includeCovariateIds,
#         maxCohortSizeForFitting = runSettings$runCmSettings$psSettings$maxCohortSizeForFitting,
#         errorOnHighCorrelation = runSettings$runCmSettings$psSettings$errorOnHighCorrelation,
#         stopOnError = runSettings$runCmSettings$psSettings$stopOnError,
#         prior = runSettings$runCmSettings$psSettings$prior,
#         control = runSettings$runCmSettings$psSettings$control
#       )
#     }
#
#     saveDir <- file.path(
#       analysisPath,
#       "Estimation",
#       outcomeId,
#       label
#     )
#
#     dir.create(
#       saveDir,
#       recursive = TRUE
#     )
#
#     saveRDS(
#       lapply(
#         ps, dplyr::as_tibble
#       ),
#       file.path(
#         saveDir,
#         "ps.rds"
#       )
#     )
#
#     saveRDS(
#       mapMatrix,
#       file.path(
#         saveDir
#         'mapMatrix.rds'
#       )
#     )
#
#     mapMatrix %>%
#       dplyr::mutate(
#         riskStratum = paste0(
#           "Q",
#           riskStratum
#         )
#       ) %>%
#       dplyr::group_by(
#         riskStratum
#       ) %>%
#       dplyr::summarise(
#         minRisk = min(value),
#         maxRisk = max(value),
#         meanRisk = mean(value)
#       ) %>%
#       saveRDS(
#         file.path(
#           saveDir,
#           "riskOverall.rds"
#         )
#       )
#     return(NULL)
#   }
#
#
#   ParallelLogger::logInfo(
#     "Estimating propensity scores within risk strata"
#   )
#
#
#
#
#   ParallelLogger::logInfo(
#     paste(
#       "Saved the map matrix for outcome",
#       outcomeId
#     )
#   )
#
#   return(NULL)
#
# }
#
# createMapMatrix <- function(
#   riskPredictions,
#   analysis
# ) {
#
#   if (analysis$riskStratificationMethod == "equal") {
#     lengthSequence <- analysis$riskStratificationThresholds + 1
#     breaks <- seq(
#       from       = 0,
#       to         = 1,
#       length.out = lengthSequence
#     )
#     mapMatrix <- riskPredictions %>%
#       dplyr::mutate(
#         labels = cut(
#           value,
#           breaks = quantile(
#             value,
#             breaks = breaks
#           )
#         ),
#         riskStratum = as.numeric(labels)
#       )
#   } else if (analysis$riskStratificationMethod == "quantile") {
#     mapMatrix <- riskPredictions %>%
#       dplyr::mutate(
#         labels = cut(
#           value,
#           breaks = quantile(
#             value,
#             breaks = analysis$riskStratificationThresholds
#           )
#         ),
#         riskStratum = as.numeric(labels)
#       )
#   } else if (analysis$riskStratificationMethod == "custom") {
#     mapMatrix <- riskPredictions %>%
#       dplyr::mutate(
#         labels = cut(
#           value,
#           breaks = analysis$riskStratificationThresholds
#         ),
#         riskStratum = as.numeric(labels)
#       )
#   }
# }
