library(dplyr)
library(plotly)
library(RColorBrewer)

if (is.null(.GlobalEnv$shinySettings)) {
  analysisSettingsList <- NULL
} else {
  analysisPath <- .GlobalEnv$shinySettings
}

assembleFileName <- function(path, fileName, suffix) {
  file.path(
    path,
    paste0(
      paste(fileName, suffix, sep = "_"),
      ".rds"
    )
  )
}

addConceptNames <- function(data, mapExposures, mapOutcomes) {
  data %>%
    dplyr::left_join(
      mapOutcomes,
      by = c(
        "analysisId" = "analysisId",
        "outcomeId" = "outcome_id"
      )
    ) %>%
    dplyr::select(-"outcomeId") %>%
    dplyr::rename(
      "stratOutcome" = "outcome_name"
    ) %>%
    dplyr::left_join(
      mapExposures,
      by = c(
        "analysisId" = "analysisId",
        "treatmentId" = "exposure_id"
      )
    ) %>%
    dplyr::select(-"treatmentId") %>%
    dplyr::rename(
      "treatment" = "exposure_name"
    ) %>%
    dplyr::left_join(
      mapExposures,
      by = c(
        "analysisId" = "analysisId",
        "comparatorId" = "exposure_id"
      )
    ) %>%
    dplyr::select(-"comparatorId") %>%
    dplyr::rename("comparator" = "exposure_name") %>%
    return()
}

pathToHtml <- file.path(
  analysisPath,
  "html"
)
analysesFile <- list.files(
  analysisPath,
  pattern = "analyses"
)

# analyses <- readRDS(file.path(analysisPath, analysesFile))
analyses <- lapply(file.path(analysisPath, analysesFile), readRDS) %>%
  dplyr::bind_rows()

if (length(unique(analyses$analysisId)) == 1) {
  suffix <- analyses$analysisId[1]
} else {
  suffix <- "combined"
}

stratOptions <- data.frame(outcome_id = unique(analyses$stratificationOutcome))
mapOutcomes <- readRDS(
	file.path(
		analysisPath,
		paste0(
		  paste("map_outcomes", suffix, sep = "_"),
		  ".rds"
		)
	)
) %>%
  dplyr::select(analysisId, outcome_id, outcome_name)

stratOptions <- stratOptions %>%
  dplyr::left_join(mapOutcomes, by = "outcome_id") %>%
  dplyr::pull(outcome_name)

mapExposures <- readRDS(
	file.path(
		analysisPath,
		paste0(
		  paste("map_exposures", suffix, sep = "_"),
		  ".rds"
		)
	)
) %>%
  dplyr::select(analysisId, exposure_id, exposure_name)


analyses <- analyses %>%
  dplyr::left_join(
    mapOutcomes,
    by = c(
      "analysisId" = "analysisId",
      "stratificationOutcome" = "outcome_id"
    )
  ) %>%
  dplyr::select(-"stratificationOutcome") %>%
  dplyr::rename(
    "stratificationOutcome" = "outcome_name"
  ) %>%
  dplyr::left_join(
    mapExposures,
    by = c(
      "analysisId" = "analysisId",
      "treatmentCohortId" = "exposure_id"
    )
  ) %>%
  dplyr::select(-"treatmentCohortId") %>%
  dplyr::rename(
    "treatmentCohort" = "exposure_name"
  ) %>%
  dplyr::left_join(
    mapExposures,
    by = c(
      "analysisId" = "analysisId",
      "comparatorCohortId" = "exposure_id"
    )
  ) %>%
  dplyr::select(-"comparatorCohortId") %>%
  dplyr::rename(
    "comparatorCohort" = "exposure_name"
  )


overallAnalysisFiles <- list.files(
  analysisPath,
  pattern = "^overall"
)

if (!is.null(overallAnalysisFiles)) {
  overallAnalysis <- TRUE

  overallMappedOverallRelativeResults <- readRDS(
    assembleFileName(analysisPath, "mappedOverallResults", suffix)
  ) %>%
    addConceptNames(
      mapExposures = mapExposures,
      mapOutcomes = mapOutcomes
    )

  overallIncidence <- readRDS(
    assembleFileName(analysisPath, "incidenceOverall", suffix)
  ) %>%
    addConceptNames(
      mapExposures = mapExposures,
      mapOutcomes = mapOutcomes
    )
}

hasOverallNegativeControls <- FALSE
testFile <- list.files(
  analysisPath,
  pattern = "mappedOverallResultsNegativeControls"
)
if (length(testFile) != 0) {
  hasOverallNegativeControls <- TRUE
  overallNegativeControls <- readRDS(
    assembleFileName(analysisPath, "mappedOverallResultsNegativeControls", suffix)
  ) %>%
    dplyr::left_join(
      mapExposures,
      by = c(
        "analysisId" = "analysisId",
        "treatmentId" = "exposure_id"
      )
    ) %>%
    dplyr::select(-"treatmentId") %>%
    dplyr::rename(
      "treatment" = "exposure_name"
    ) %>%
    dplyr::left_join(
      mapExposures,
      by = c(
        "analysisId" = "analysisId",
        "comparatorId" = "exposure_id"
      )
    ) %>%
    dplyr::select(-"comparatorId") %>%
    dplyr::rename("comparator" = "exposure_name") %>%
    dplyr::mutate(
      logRr = log(estimate)
    )
}

hasNegativeControls <- FALSE
testFile <- list.files(
  analysisPath,
  pattern = "negativeControls"
)
if (length(testFile) != 0) {

  hasNegativeControls <- TRUE
  negativeControls <- readRDS(
      assembleFileName(analysisPath, "negativeControls", suffix)
  ) %>%
    dplyr::left_join(
      mapOutcomes,
      by = c(
        "analysisId" = "analysisId",
        "stratOutcome" = "outcome_id"
      )
    ) %>%
    dplyr::select(-"stratOutcome") %>%
    dplyr::rename(
      "stratOutcome" = "outcome_name"
    ) %>%
    dplyr::left_join(
      mapExposures,
      by = c(
        "analysisId" = "analysisId",
        "treatment" = "exposure_id"
      )
    ) %>%
    dplyr::select(-"treatment") %>%
    dplyr::rename(
      "treatment" = "exposure_name"
    ) %>%
    dplyr::left_join(
      mapExposures,
      by = c(
        "analysisId" = "analysisId",
        "comparator" = "exposure_id"
      )
    ) %>%
    dplyr::select(-"comparator") %>%
    dplyr::rename(
      "comparator" = "exposure_name"
    )
}

# analyses <- readRDS(
# 	file.path(
# 		analysisPath,
# 		"analyses.rds"
# 	)
# ) %>%
#   dplyr::left_join(
#     mapExposures,
#     by = c(
#      "treatment_id" = "exposure_id"
#     )
#   ) %>%
#   dplyr::rename(
#     "treatment" = "exposure_name"
#   ) %>%
#   dplyr::left_join(
#     mapExposures,
#     by = c(
#       "comparator_id" = "exposure_id"
#     )
#   ) %>%
#   dplyr::rename(
#     "comparator" = "exposure_name"
#   ) %>%
#   dplyr::select(
#     -c(
#       "treatment_id",
#       "comparator_id"
#     )
#   )

incidence <- readRDS(
  assembleFileName(analysisPath, "incidence", suffix)
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
		  "analysisId" = "analysisId",
			"estOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"estOutcome") %>%
	dplyr::rename(
		"estOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
		  "analysisId" = "analysisId",
			"stratOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"stratOutcome") %>%
	dplyr::rename(
		"stratOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
		  "analysisId" = "analysisId",
			"treatmentId" = "exposure_id"
		)
	) %>%
	dplyr::select(-"treatmentId") %>%
	dplyr::rename(
		"treatment" = "exposure_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
		  "analysisId" = "analysisId",
			"comparatorId" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparatorId") %>%
	dplyr::rename("comparator" = "exposure_name")

predictionPerformance <-
	readRDS(
	  assembleFileName(analysisPath, "predictionPerformance", suffix)
	) %>%
  tibble() %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
		  "analysisId" = "analysisId",
			"stratOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"stratOutcome") %>%
	dplyr::rename(
		"stratOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
		  "analysisId" = "analysisId",
			"treatmentId" = "exposure_id"
		)
	) %>%
	dplyr::select(-"treatmentId") %>%
	dplyr::rename(
		"treatment" = "exposure_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
		  "analysisId" = "analysisId",
			"comparatorId" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparatorId") %>%
	dplyr::rename(
		"comparator" = "exposure_name"
	)

mappedOverallAbsoluteResults <-
	readRDS(
	  assembleFileName(analysisPath, "mappedOverallAbsoluteResults", suffix)
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
		  "analysisId" = "analysisId",
			"estOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"estOutcome") %>%
	dplyr::rename(
		"estOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
		  "analysisId" = "analysisId",
			"stratOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"stratOutcome") %>%
	dplyr::rename(
		"stratOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
		  "analysisId" = "analysisId",
			"treatment" = "exposure_id"
		)
	) %>%
	dplyr::select(-"treatment") %>%
	dplyr::rename(
		"treatment" = "exposure_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
		  "analysisId" = "analysisId",
			"comparator" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparator") %>%
	dplyr::rename(
		"comparator" = "exposure_name"
	)

mappedOverallRelativeResults <-
	readRDS(
	  assembleFileName(analysisPath, "mappedOverallRelativeResults", suffix)
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
		  "analysisId" = "analysisId",
			"estOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"estOutcome") %>%
	dplyr::rename(
		"estOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
		  "analysisId" = "analysisId",
			"stratOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"stratOutcome") %>%
	dplyr::rename(
		"stratOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
		  "analysisId" = "analysisId",
			"treatment" = "exposure_id"
		)
	) %>%
	dplyr::select(-"treatment") %>%
	dplyr::rename(
		"treatment" = "exposure_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
		  "analysisId" = "analysisId",
			"comparator" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparator") %>%
	dplyr::rename(
		"comparator" = "exposure_name"
	)


mappedOverallCasesResults <-
	readRDS(
	  assembleFileName(analysisPath, "mappedOverallCasesResults", suffix)
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
		  "analysisId" = "analysisId",
			"estOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"estOutcome") %>%
	dplyr::rename(
		"estOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
		  "analysisId" = "analysisId",
			"stratOutcome" = "outcome_id"
		)
	) %>%
	dplyr::select(-"stratOutcome") %>%
	dplyr::rename(
		"stratOutcome" = "outcome_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
		  "analysisId" = "analysisId",
			"treatment" = "exposure_id"
		)
	) %>%
	dplyr::select(-"treatment") %>%
	dplyr::rename(
		"treatment" = "exposure_name"
	) %>%
	dplyr::left_join(
		mapExposures,
		by = c(
		  "analysisId" = "analysisId",
			"comparator" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparator") %>%
	dplyr::rename(
		"comparator" = "exposure_name"
	)


databaseOptions <- unique(analyses$database)

analysisTypeOptions <- unique(
  analyses$analysis_type
)

# ==============================================================================
# Functions
# ==============================================================================

getOutcomeNames <- function(string, mapOutcomes) {
  tibble(
    outcomeId = as.numeric(
      stringr::str_split(string, pattern = ";", simplify = TRUE)
    )
  ) %>%
    dplyr::left_join(mapOutcomes, by = c("outcomeId" = "outcome_id")) %>%
    dplyr::distinct(outcome_name) %>%
    dplyr::pull()
}

stratIsEst <- function(choices, stratOutcome) {
  if (stratOutcome %in% choices) {
    return(stratOutcome)
  } else {
    choices[1]
  }
}

constructFileName <- function(
    prefix,
    analysisPath,
    analysisId,
    runLabel,
    estOutcome = NULL,
    mapOutcomes = NULL
) {
  if (is.null(estOutcome)) {
    file.path(
      analysisPath,
      paste0(
        paste(prefix, analysisId, runLabel, sep = "_"),
        ".rds"
      )
    ) %>%
      return()
  } else {

    estOutcomeId <- mapOutcomes %>%
      dplyr::filter(outcome_name == estOutcome) %>%
      dplyr::pull(outcome_id)
    file.path(
      analysisPath,
      paste0(
        paste(prefix, analysisId, runLabel, estOutcomeId, sep = "_"),
        ".rds"
      )
    ) %>%
      return()
  }
}

getAnalysis <- function(
    analyses,
    database,
    treatmentCohort,
    comparatorCohort
) {
  analyses %>%
    dplyr::filter(
      database == !!database,
      treatmentCohort  == !!treatmentCohort,
      comparatorCohort == !!comparatorCohort
    ) %>%
    dplyr::distinct(analysisId) %>%
    pull()
}

getRunLabel <- function(
    analyses,
    analysisId,
    stratificationOutcome,
    riskStratificationMethod,
    psAdjsutmentMethod
) {
  pp <- c(stratificationOutcome, riskStratificationMethod, psAdjsutmentMethod)
  if (any(pp == "")) {
    return(analyses[1, ]$runLabel)
  }
  analyses %>%
    dplyr::filter(
      analysisId == !!analysisId,
      stratificationOutcome == !!stratificationOutcome,
      riskStratificationMethod == !!riskStratificationMethod,
      psAdjsutmentMethod == !!psAdjsutmentMethod
    ) %>%
    dplyr::distinct(runLabel) %>%
    pull()
}

getResults <- function(
  treat, comp, strat, est, db, anal,
  mappedOverallRelativeResults,
  mappedOverallAbsoluteResults,
  mappedOverallCasesResults
) {

	result <- list()

	result$relative <-
		mappedOverallRelativeResults %>%
		dplyr::filter(
      stratOutcome %in% strat & estOutcome %in% est & treatment %in% treat & comparator %in% comp & database %in% db & analysisType %in% anal
		)

	result$absolute <-
		mappedOverallAbsoluteResults %>%
		dplyr::filter(
			stratOutcome %in% strat & estOutcome %in% est & treatment %in% treat & comparator %in% comp & database %in% db & analysisType %in% anal
		)

	result$cases <-
		mappedOverallCasesResults %>%
		dplyr::filter(
			stratOutcome %in% strat & estOutcome %in% est & treatment %in% treat & comparator %in% comp & database %in% db & analysisType %in% anal
		)

	return(result)


}

getResultsOverall <- function(
  treat, comp, strat, db, anal,
  overallMappedOverallRelativeResults
) {
  result <- overallMappedOverallRelativeResults %>%
    dplyr::filter(
      treatment %in% treat & comparator %in% comp & database %in% db & analysisType %in% anal
    )
  if (!missing(strat)) {
    result <- result %>%
      dplyr::filter(
        stratOutcome %in% strat
      )
  }
  return(result)
}


getIncidence <- function(
  treat,
  comp,
  strat,
  est,
  db,
  anal,
  incidence
  )
{
  incidence %>%
		dplyr::filter(
			stratOutcome %in% strat & estOutcome %in% est & treatment %in% treat & comparator %in% comp & database %in% db & analysisType %in% anal
		) %>%
		return()
}

getIncidenceOverall <- function(
  treat,
  comp,
  strat,
  db,
  anal,
  incidence
  )
{
  incidence %>%
		dplyr::filter(
			stratOutcome == strat & treatment %in% treat & comparator %in% comp & database %in% db & analysisType %in% anal
		) %>%
		return()
}

getPredictionPerformance <- function(
  treat,
  comp,
  strat,
  coh,
  db,
  predictionPerformance
) {

	predictionPerformance %>%
		dplyr::filter(
			stratOutcome %in% strat & cohort %in% coh & treatment %in% treat &
			  comparator %in% comp & database %in% db
		) %>%
		return()

}

getBalance <- function(
  treat,
  comp,
  strat,
  est = NULL,
  db,
  anal,
  analyses,
  mapExposures,
  mapOutcomes,
  analysisPath,
  isOverall = FALSE
)
{

  res <- analyses %>%
		dplyr::filter(
			treatment == treat,
			comparator == comp,
			database == db,
			analysis_label == anal
		)

	stratOutcomeId <- mapOutcomes %>%
		dplyr::filter(outcome_name == strat) %>%
		dplyr::select("outcome_id") %>%
		unlist()

	estOutcomeId <- mapOutcomes %>%
		dplyr::filter(outcome_name == est) %>%
		dplyr::select("outcome_id") %>%
		unlist()

	treatmentId <- mapExposures %>%
		dplyr::filter(exposure_name == treat) %>%
		dplyr::select("exposure_id") %>%
		unlist()

	comparatorId <- mapExposures %>%
		dplyr::filter(exposure_name == comp) %>%
		dplyr::select("exposure_id") %>%
		unlist()

	if (isOverall)
	{
	  readRDS(
	    file.path(
	      analysisPath,
	      paste0(
	        paste(
	          "overall",
	          "balance",
	          res$analysis_id,
	          res$database,
	          res$analysis_label,
	          treatmentId,
	          comparatorId,
	          stratOutcomeId,
	          sep = "_"
	        ),
	        ".rds"
	      )
	    )
	  )
	}
	else
	{
	  readRDS(
	    file.path(
	      analysisPath,
	      paste0(
	        paste(
	          "balance",
	          res$analysis_id,
	          res$database,
	          res$analysis_label,
	          treatmentId,
	          comparatorId,
	          stratOutcomeId,
	          estOutcomeId,
	          sep = "_"
	        ),
	        ".rds"
	      )
	    )
	  )
	}
}


getPsDensity <- function(treat,
						 comp,
						 strat,
						 est,
						 db,
						 anal,
						 analyses,
						 mapExposures,
						 mapOutcomes,
						 analysisPath) {

	res <- analyses %>%
		dplyr::filter(
			treatment == treat,
			comparator == comp,
			database == db,
			analysis_label == anal
		)

	stratOutcomeId <- mapOutcomes %>%
		dplyr::filter(outcome_name == strat) %>%
		dplyr::select("outcome_id") %>%
		unlist()

	estOutcomeId <- mapOutcomes %>%
		dplyr::filter(outcome_name == est) %>%
		dplyr::select("outcome_id") %>%
		unlist()

	treatmentId <- mapExposures %>%
		dplyr::filter(exposure_name == treat) %>%
		dplyr::select("exposure_id") %>%
		unlist()

	comparatorId <- mapExposures %>%
		dplyr::filter(exposure_name == comp) %>%
		dplyr::select("exposure_id") %>%
		unlist()

	readRDS(
		file.path(
			analysisPath,
			paste0(
				paste(
					"psDensity",
					res$analysis_id,
					db,
					anal,
					treatmentId,
					comparatorId,
					stratOutcomeId,
					estOutcomeId,
					sep = "_"
				),
				".rds"
			)
		)
	)
}


getPsDensityOverall <- function(
  treat,
  comp,
  strat,
  db,
  anal,
  analyses,
  mapExposures,
  mapOutcomes,
  analysisPath
)
{

  res <- analyses %>%
		dplyr::filter(
			treatment == treat,
			comparator == comp,
			database == db,
			analysis_label == anal
		)

	stratOutcomeId <- mapOutcomes %>%
		dplyr::filter(outcome_name == strat) %>%
		dplyr::select("outcome_id") %>%
		unlist()

	treatmentId <- mapExposures %>%
		dplyr::filter(exposure_name == treat) %>%
		dplyr::select("exposure_id") %>%
		unlist()

	comparatorId <- mapExposures %>%
		dplyr::filter(exposure_name == comp) %>%
		dplyr::select("exposure_id") %>%
		unlist()

	readRDS(
		file.path(
			analysisPath,
			paste0(
				paste(
				  "overall",
					"psDensity",
					res$analysis_id,
					res$database,
					res$analysis_label,
					treatmentId,
					comparatorId,
					stratOutcomeId,
					sep = "_"
				),
				".rds"
			)
		)
	)
}

getAuc <- function(
  treat,
  comp,
  strat,
  db,
  anal,
  predictionPopulation,
  analyses,
  mapExposures,
  mapOutcomes,
  analysisPath
) {
  res <- analyses %>%
    dplyr::filter(
      treatment == treat,
      comparator == comp,
      database == db,
      analysis_label == anal
    )

  stratOutcomeId <- mapOutcomes %>%
    dplyr::filter(outcome_name == strat) %>%
    dplyr::select("outcome_id") %>%
    unlist()

  treatmentId <- mapExposures %>%
    dplyr::filter(exposure_name == treat) %>%
    dplyr::select("exposure_id") %>%
    unlist()

  comparatorId <- mapExposures %>%
    dplyr::filter(exposure_name == comp) %>%
    dplyr::select("exposure_id") %>%
    unlist()

  pathList <- file.path(
    analysisPath,
    paste(
      paste(
        "auc",
        predictionPopulation,
        res$analysis_id,
        res$database,
        treatmentId,
        comparatorId,
        stratOutcomeId,
        sep = "_"
      ),
      "rds",
      sep = "."
    )
  )


  aucResultList <- lapply(
    pathList,
    readRDS
  )

  names(aucResultList) <- predictionPopulation

  aucResultList %>%
    dplyr::bind_rows(
      .id = "cohort"
    ) %>%
    return()
}


getCalibration <- function(
  treat,
  comp,
  strat,
  db,
	anal,
	predictionPopulation,
	analyses,
	mapExposures,
	mapOutcomes,
	analysisPath
) {

  res <- analyses %>%
		dplyr::filter(
			treatment == treat,
			comparator == comp,
			database == db,
			analysis_label == anal
		)

	stratOutcomeId <- mapOutcomes %>%
		dplyr::filter(outcome_name == strat) %>%
		dplyr::select("outcome_id") %>%
		unlist()

	treatmentId <- mapExposures %>%
		dplyr::filter(exposure_name == treat) %>%
		dplyr::select("exposure_id") %>%
		unlist()

	comparatorId <- mapExposures %>%
		dplyr::filter(exposure_name == comp) %>%
		dplyr::select("exposure_id") %>%
		unlist()

	pathList <- file.path(
		analysisPath,
		paste(
			paste(
				"calibration",
				predictionPopulation,
				res$analysis_id,
				res$database,
				treatmentId,
				comparatorId,
				stratOutcomeId,
				sep = "_"
			),
			"rds",
			sep = "."
		)
	)


	calibrationResultList <- lapply(
		pathList,
		readRDS
	)

	names(calibrationResultList) <- predictionPopulation

	calibrationResultList %>%
		dplyr::bind_rows(
			.id = "cohort"
		) %>%
		return()
}


combinedPlot <- function(
	cases,
	relative,
	absolute,
	treatment,
	comparator
) {


  cases <- cases %>%
    dplyr::mutate(
      analysisType_estOutcome = paste(
        analysisId,
        runLabel,
        estOutcome,
        sep = "/"
      )
    )

  relative <- relative %>%
    dplyr::mutate(
      analysisType_estOutcome = paste(
        analysisId,
        runLabel,
        estOutcome,
        sep = "/"
      )
    )

  absolute <- absolute %>%
    dplyr::mutate(
      analysisType_estOutcome = paste(
        analysisId,
        runLabel,
        estOutcome,
        sep = "/"
      )
    )

	customColors <- c(
		"#0099FF",
		"#009933",
		"#CC0000",
		"#FF9933",
		"#663399",
		"#CC9966"
	)

	numberOfOutcomes <- length(
		unique(
			cases$estOutcome
		)
	)

	# numberOfAnalyses <- length(
	# 	unique(
	# 		cases$analysisType
	# 	)
	# )

	numberOfPoints <- numberOfOutcomes # * numberOfAnalyses
	if (numberOfPoints > 4) {
		stop("No more than 4 outcomes can be plotted at the same time")
	}

	m <- 0

	if (numberOfPoints == 2) {
		m <- c(-.15, .15)
	} else if (numberOfPoints == 3) {
		m <- c(-.15, 0, .15)
	} else if (numberOfPoints == 4) {
		m <- c(-.15, -.05, .05, .15)
	}

	relative <- relative %>%
		dplyr::mutate(
		  analysisType_estOutcome = factor(
				analysisType_estOutcome,
				levels = sort(
					unique(
						analysisType_estOutcome
					)
				)
			)
		)

	absolute <- absolute %>%
		dplyr::mutate(
		  analysisType_estOutcome = factor(
				analysisType_estOutcome,
				levels = sort(
					unique(
						analysisType_estOutcome
					)
				)
			)
		)

	quickMap <- data.frame(
	  analysisType_estOutcome = levels(relative$analysisType_estOutcome),
		m = m
	)

	cases <-
		reshape2::melt(
			cases,
			id.vars = c(
				"riskStratum",
				"analysisType_estOutcome"
			),
			measure.vars = c(
				"casesComparator",
				"casesTreatment"
			)
		) %>%
		dplyr::mutate(
			variable = ifelse(
				.$variable == "casesComparator",
				comparator,
				treatment
			),
			g = paste(
				.$analysisType_estOutcome,
				.$variable,
				sep = "/"
			),
			value = 100*.$value,
			riskStratum = as.numeric(
				as.factor(
					.$riskStratum
				)
			)
		)


	p1   <-
		plotly::plot_ly(
			data = cases,
			x = ~riskStratum,
			y = ~value,
			color = ~g,
			colors = "Paired",
			type = 'bar',
			hoverinfo = "text",
			hovertext = paste(
				"<b>Outcome:</b>",
				cases$estOutcome,
				"<br><b>Database:</b>",
				cases$database,
			"<br><b>Analysis:</b>",
				cases$analysisType,
				"<br><b>Exposure:</b>",
				cases$variable,
				"<br><b>Event rate:</b>",
				paste0(
					round(cases$value, 2),
					"%"
				)
			),
			legendgroup = ~g
		) %>%
		plotly::layout(
			yaxis = list(
				title = 'Observed events (%)',
				autorange = "reversed"
			),
			xaxis = list(
				title = "Risk stratum"
			),
			barmode = 'group'
		)

	relative <-
		relative %>%
		dplyr::left_join(quickMap) %>%
		dplyr::mutate(
			risk = as.numeric(
				as.factor(
					riskStratum
				)
			),
			analysisType_estOutcome = as.factor(analysisType_estOutcome)
		) %>%
	  dplyr::arrange(analysisType_estOutcome)

	p2 <-
		relative %>%
		dplyr::group_by(analysisType_estOutcome) %>%
		plotly::plot_ly(
			type = "scatter",
			mode = "line+markers",
			x = ~risk + m,
			y = ~estimate,
			line = ~list(color = analysisType_estOutcome, dash = "dash", width = .5),
			color = ~analysisType_estOutcome,
			colors = customColors[1:numberOfPoints],
			error_y = list(
				type = "data",
				array = relative$upper - relative$estimate,
				arrayminus = relative$estimate - relative$lower
			),
			hoverinfo = "text",
			hovertext = paste(
				"<b>Outcome:</b>",
				relative$estOutcome,
				"<br><b>Database:</b>",
				relative$database,
				"<br><b>Analysis:</b>",
				relative$analysisType,
				"<br><b>HR:</b>",
				paste0(
					round(relative$estimate, 2),
					" (",
					paste(
						round(relative$lower, 2),
						round(relative$upper, 2),
						sep = ", "
					),
					")"
				)
			),
			legendgroup = ~estOutcome
		) %>%
		plotly::layout(
			yaxis = list(
				title = "Hazard ratio"
			),
			xaxis = list(
				title = "Risk stratum",
				tickvals = ~risk
			)
		) %>%
		plotly::layout(
			shapes = hline(1)
		)

	absolute <-
		absolute %>%
		dplyr::left_join(quickMap) %>%
		dplyr::mutate(
			estimate = 100 * estimate,
			lower = 100 * lower,
			upper = 100 * upper,
			risk = as.numeric(
				as.factor(
					riskStratum
				),
			analysisType_estOutcome = as.factor(analysisType_estOutcome)
			)
		) |>
	  dplyr::arrange(analysisType_estOutcome)


	p3 <-
		absolute %>%
		plotly::plot_ly(
			type = "scatter",
			mode = "line+markers",
			x = ~risk + m,
			y = ~estimate,
			color = ~analysisType_estOutcome,
			colors = customColors[1:numberOfPoints],
			line = ~list(color = analysisType_estOutcome, dash = "dash", width = .5),
			error_y = list(
				type = "data",
				symmetric = FALSE,
				array = ~(upper-estimate),
				# array = absolute$upper - absolute$estimate,
				# arrayminus = absolute$estimate - absolute$lower
				arrayminus = ~(estimate - lower)
			),
			hoverinfo = "text",
			hovertext = paste(
				"<b>Outcome:</b>",
				absolute$estOutcome,
				"<br><b>Database:</b>",
				absolute$database,
				"<br><b>Analysis:</b>",
				absolute$analysisType,
				"<br><b>Absolute difference:</b>",
				paste0(
					round(absolute$estimate, 4),
					" (",
					paste(
						round(absolute$lower, 4),
						round(absolute$upper, 4),
						sep = ", "
					),
					"}"
				)
			),
			legendgroup = ~estOutcome,
			showlegend = FALSE
		) %>%
		plotly::layout(
			yaxis = list(
				title = "Absolute risk reduction (%)"
			),
			xaxis = list(
				title = "Risk stratum",
				tickvals = ~risk
			)
		)

	plotly::subplot(p1, p2, p3, shareX = TRUE, nrows = 3, titleY = T)


}



hline <- function(y = 0, color = "black") {
	list(
		type = "line",
		x0 = 0,
		x1 = 1,
		xref = "paper",
		y0 = y,
		y1 = y,
		line = list(
			color = color
			# dash = "dash"
		)
	)
}



addInfo <- function(item, infoId) {
	infoTag <- tags$small(
		class = "badge pull-right action-button",
		style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
		type = "button",
		id = infoId,
		"i"
	)

	item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))

	return(item)
}







getNegativeControls <- function(
  treat, comp, strat, db, anal,
  negativeControls
) {
  res <- negativeControls %>%
    dplyr::filter(
      stratOutcome %in% strat &
        treatment %in% treat &
        comparator %in% comp &
        database %in% db &
        analysisType %in% anal
    )
  return(res)
}


plotRiskStratifiedNegativeControls <- function(
  negativeControls,
  positiveControls
) {
  riskStrata <- unique(negativeControls$riskStratum)
  plots <- list()
  for (i in seq_along(riskStrata)) {
    negativeControlsSubset <- negativeControls %>%
      filter(riskStratum == riskStrata[i])

    null <- EmpiricalCalibration::fitNull(
      logRr   = log(negativeControlsSubset$estimate),
      seLogRr = negativeControlsSubset$seLogRr
    )

    positiveControlsSubset <- positiveControls %>%
      dplyr::filter(
        riskStratum == paste0("Q", i)
      )

    plots[[i]] <- EmpiricalCalibration::plotCalibrationEffect(
      logRrNegatives   = log(negativeControlsSubset$estimate),
      seLogRrNegatives = negativeControlsSubset$seLogRr,
      logRrPositives   = log(positiveControlsSubset$estimate),
      seLogRrPositives = positiveControlsSubset$seLogRr,
      null             = null
    )
  }
  gridExtra::grid.arrange(
    grobs = plots,
    nrow  = 1
  )
}


calibrateRiskStrataCis <- function(
  negativeControls,
  positiveControls
) {
  riskStrata <- unique(negativeControls$riskStratum)
  outcomes <- unique(positiveControls$estOutcome)
  ret <- NULL
  for (j in seq_along(outcomes)) {
    for (i in seq_along(riskStrata)) {
      negativeControlsSubset <- negativeControls %>%
        dplyr::filter(
          riskStratum == paste0("Q", i)
        )
      positiveControlsSubset <- positiveControls %>%
        dplyr::filter(
          riskStratum == paste0("Q", i),
          estOutcome == outcomes[j]
        )
      mod <- EmpiricalCalibration::fitSystematicErrorModel(
        logRr =  log(negativeControlsSubset$estimate),
        seLogRr = negativeControlsSubset$seLogRr,
        trueLogRr = rep(0, nrow(negativeControlsSubset))
      )

      ret <- ret %>%
        dplyr::bind_rows(EmpiricalCalibration::calibrateConfidenceInterval(
          logRr = log(positiveControlsSubset$estimate),
          seLogRr = positiveControlsSubset$seLogRr,
          model = mod
        ) %>%
          dplyr::mutate(
            estimate = round(exp(logRr), digits = 2),
            lower = round(exp(logLb95Rr), digits = 2),
            upper = round(exp(logUb95Rr), digits = 2)
          ) %>%
          dplyr::select(
            estimate,
            lower,
            upper
          ) %>%
          dplyr::mutate(
            riskStratum = paste0("Q", i),
            Outcome = outcomes[j]
          )
        )
    }
  }
  return(ret)
}



plotPsDensity <- function(data, riskStratified = FALSE) {

  plot <- ggplot2::ggplot(
    data = data,
    ggplot2::aes(
      x = x,
      y = y
    )
  ) +
    ggplot2::geom_density(
      stat = "identity",
      ggplot2::aes(
        color = exposure_name,
        group = exposure_name,
        fill = exposure_name
      )
    ) +
    ggplot2::ylab(
      label = "Density"
    ) +
    ggplot2::scale_x_continuous(
      name = "Preference score",
      breaks = seq(0, 1, .5)
    ) +
    ggplot2::scale_fill_manual(
      values = scales::alpha(c("#fc8d59", "#91bfdb"), .6)
    ) +
    ggplot2::scale_color_manual(
      values = scales::alpha(c("#fc8d59", "#91bfdb"), .9)
    )

  if (riskStratified) {
    plot <- plot + ggplot2::facet_grid(~riskStratum)
  }
    plot <- plot + ggplot2::theme_classic() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "top",
      legend.text = ggplot2::element_text(
        margin = ggplot2::margin(
          t = 0,
          r = 0.5,
          b = 0,
          l = 0.1,
          unit = "cm"
        )
      )
    )

  return(plot)
}
