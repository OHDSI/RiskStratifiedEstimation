library(dplyr)
library(plotly)
library(RColorBrewer)

if (is.null(.GlobalEnv$shinySettings)) {
  analysisSettingsList <- NULL
} else {
  analysisPath <- .GlobalEnv$shinySettings
}

mapOutcomes <- readRDS(
	file.path(
		analysisPath,
		"map_outcomes.rds"
	)
)

mapExposures <- readRDS(
	file.path(
		analysisPath,
		"map_exposures.rds"
	)
)

overallAnalysisFiles <- list.files(
  analysisPath,
  pattern = "^overall"
)

if (!is.null(overallAnalysisFiles)) {
  overallAnalysis <- TRUE

  overallMappedOverallRelativeResults <- readRDS(
    file.path(
    analysisPath,
    "mappedOverallResults.rds"
    )
  ) %>%
    dplyr::left_join(
      mapOutcomes,
      by = c(
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
        "comparatorId" = "exposure_id"
      )
    ) %>%
    dplyr::select(-"comparatorId") %>%
    dplyr::rename("comparator" = "exposure_name")


  overallIncidence <- readRDS(
    file.path(
      analysisPath,
      "incidenceOverall.rds"
    )
  ) %>%
    dplyr::left_join(
      mapOutcomes,
      by = c(
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
        "comparatorId" = "exposure_id"
      )
    ) %>%
    dplyr::select(-"comparatorId") %>%
    dplyr::rename("comparator" = "exposure_name")
}

hasOverallNegativeControls <- FALSE
if (file.exists(file.path(analysisPath, "mappedOverallResultsNegativeControls.rds"))) {
  hasOverallNegativeControls <- TRUE
  overallNegativeControls <- readRDS(
    file.path(
      analysisPath,
      "mappedOverallResultsNegativeControls.rds"
    )
  ) %>%
    dplyr::left_join(
      mapExposures,
      by = c(
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
if (file.exists(file.path(analysisPath, "negativeControls.rds"))) {
  hasNegativeControls <- TRUE
  negativeControls <- readRDS(
    file.path(
      analysisPath,
      "negativeControls.rds"
    )
  ) %>%
    dplyr::left_join(
      mapOutcomes,
      by = c(
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
        "comparator" = "exposure_id"
      )
    ) %>%
    dplyr::select(-"comparator") %>%
    dplyr::rename(
      "comparator" = "exposure_name"
    )
}

analyses <- readRDS(
	file.path(
		analysisPath,
		"analyses.rds"
	)
) %>%
  dplyr::left_join(
    mapExposures,
    by = c(
     "treatment_id" = "exposure_id"
    )
  ) %>%
  dplyr::rename(
    "treatment" = "exposure_name"
  ) %>%
  dplyr::left_join(
    mapExposures,
    by = c(
      "comparator_id" = "exposure_id"
    )
  ) %>%
  dplyr::rename(
    "comparator" = "exposure_name"
  ) %>%
  dplyr::select(
    -c(
      "treatment_id",
      "comparator_id"
    )
  )

incidence <-
	readRDS(
		file.path(
			analysisPath,
			"incidence.rds"
		)
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
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
			"comparatorId" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparatorId") %>%
	dplyr::rename("comparator" = "exposure_name")

predictionPerformance <-
	readRDS(
		file.path(
			analysisPath,
			"predictionPerformance.rds"
		)
	) %>%
  tibble() %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
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
			"comparatorId" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparatorId") %>%
	dplyr::rename(
		"comparator" = "exposure_name"
	)

mappedOverallAbsoluteResults <-
	readRDS(
		file.path(
			analysisPath,
			"mappedOverallAbsoluteResults.rds"
		)
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
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
			"comparator" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparator") %>%
	dplyr::rename(
		"comparator" = "exposure_name"
	)

mappedOverallRelativeResults <-
	readRDS(
		file.path(
			analysisPath,
			"mappedOverallRelativeResults.rds"
		)
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
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
			"comparator" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparator") %>%
	dplyr::rename(
		"comparator" = "exposure_name"
	)


mappedOverallCasesResults <-
	readRDS(
		file.path(
			analysisPath,
			"mappedOverallCasesResults.rds"
		)
	) %>%
	dplyr::left_join(
		mapOutcomes,
		by = c(
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
			"comparator" = "exposure_id"
		)
	) %>%
	dplyr::select(-"comparator") %>%
	dplyr::rename(
		"comparator" = "exposure_name"
	)


databaseOptions <- unique(
  analyses$database
)

analysisTypeOptions <- unique(
  analyses$analysis_label
)

stratOptions <- unique(
	mappedOverallAbsoluteResults$stratOutcome
)


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
      .$stratOutcome %in% strat & .$estOutcome %in% est & .$treatment %in% treat & .$comparator %in% comp & .$database %in% db & .$analysisType %in% anal
		)

	result$absolute <-
		mappedOverallAbsoluteResults %>%
		dplyr::filter(
			.$stratOutcome %in% strat & .$estOutcome %in% est & .$treatment %in% treat & .$comparator %in% comp & .$database %in% db & .$analysisType %in% anal
		)

	result$cases <-
		mappedOverallCasesResults %>%
		dplyr::filter(
			.$stratOutcome %in% strat & .$estOutcome %in% est & .$treatment %in% treat & .$comparator %in% comp & .$database %in% db & .$analysisType %in% anal
		)

	return(result)


}

getResultsOverall <- function(
  treat, comp, strat, db, anal,
  overallMappedOverallRelativeResults
) {
  result <- overallMappedOverallRelativeResults %>%
    dplyr::filter(
      .$treatment %in% treat & .$comparator %in% comp & .$database %in% db & .$analysisType %in% anal
    )
  if (!missing(strat)) {
    result <- result %>%
      dplyr::filter(
        .$stratOutcome %in% strat
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
			.$stratOutcome %in% strat & .$estOutcome %in% est & .$treatment %in% treat & .$comparator %in% comp & .$database %in% db & .$analysisType %in% anal
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
			.$stratOutcome == strat & .$treatment %in% treat & .$comparator %in% comp & .$database %in% db & .$analysisType %in% anal
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
			.$stratOutcome %in% strat & .$cohort %in% coh & .$treatment %in% treat &
			  .$comparator %in% comp & .$database %in% db
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
			.$treatment == treat,
			.$comparator == comp,
			.$database == db,
			.$analysis_label == anal
		)

	stratOutcomeId <- mapOutcomes %>%
		dplyr::filter(.$outcome_name == strat) %>%
		dplyr::select("outcome_id") %>%
		unlist()

	estOutcomeId <- mapOutcomes %>%
		dplyr::filter(.$outcome_name == est) %>%
		dplyr::select("outcome_id") %>%
		unlist()

	treatmentId <- mapExposures %>%
		dplyr::filter(.$exposure_name == treat) %>%
		dplyr::select("exposure_id") %>%
		unlist()

	comparatorId <- mapExposures %>%
		dplyr::filter(.$exposure_name == comp) %>%
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
			.$treatment == treat,
			.$comparator == comp,
			.$database == db,
			.$analysis_label == anal
		)

	stratOutcomeId <- mapOutcomes %>%
		dplyr::filter(.$outcome_name == strat) %>%
		dplyr::select("outcome_id") %>%
		unlist()

	estOutcomeId <- mapOutcomes %>%
		dplyr::filter(.$outcome_name == est) %>%
		dplyr::select("outcome_id") %>%
		unlist()

	treatmentId <- mapExposures %>%
		dplyr::filter(.$exposure_name == treat) %>%
		dplyr::select("exposure_id") %>%
		unlist()

	comparatorId <- mapExposures %>%
		dplyr::filter(.$exposure_name == comp) %>%
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
			.$treatment == treat,
			.$comparator == comp,
			.$database == db,
			.$analysis_label == anal
		)

	stratOutcomeId <- mapOutcomes %>%
		dplyr::filter(.$outcome_name == strat) %>%
		dplyr::select("outcome_id") %>%
		unlist()

	treatmentId <- mapExposures %>%
		dplyr::filter(.$exposure_name == treat) %>%
		dplyr::select("exposure_id") %>%
		unlist()

	comparatorId <- mapExposures %>%
		dplyr::filter(.$exposure_name == comp) %>%
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
      .$treatment == treat,
      .$comparator == comp,
      .$database == db,
      .$analysis_label == anal
    )

  stratOutcomeId <- mapOutcomes %>%
    dplyr::filter(.$outcome_name == strat) %>%
    dplyr::select("outcome_id") %>%
    unlist()

  treatmentId <- mapExposures %>%
    dplyr::filter(.$exposure_name == treat) %>%
    dplyr::select("exposure_id") %>%
    unlist()

  comparatorId <- mapExposures %>%
    dplyr::filter(.$exposure_name == comp) %>%
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
			.$treatment == treat,
			.$comparator == comp,
			.$database == db,
			.$analysis_label == anal
		)

	stratOutcomeId <- mapOutcomes %>%
		dplyr::filter(.$outcome_name == strat) %>%
		dplyr::select("outcome_id") %>%
		unlist()

	treatmentId <- mapExposures %>%
		dplyr::filter(.$exposure_name == treat) %>%
		dplyr::select("exposure_id") %>%
		unlist()

	comparatorId <- mapExposures %>%
		dplyr::filter(.$exposure_name == comp) %>%
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
)
{

  cases <- cases %>%
    dplyr::mutate(
      analysisType_estOutcome = paste(
        .$analysisType,
        .$estOutcome,
        sep = "/"
      )
    )

  relative <- relative %>%
    dplyr::mutate(
      analysisType_estOutcome = paste(
        .$analysisType,
        .$estOutcome,
        sep = "/"
      )
    )

  absolute <- absolute %>%
    dplyr::mutate(
      analysisType_estOutcome = paste(
        .$analysisType,
        .$estOutcome,
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

	numberOfAnalyses <- length(
		unique(
			cases$analysisType
		)
	)

	numberOfPoints <- numberOfOutcomes * numberOfAnalyses
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
				.$analysisType_estOutcome,
				levels = sort(
					unique(
						.$analysisType_estOutcome
					)
				)
			)
		)

	absolute <- absolute %>%
		dplyr::mutate(
		  analysisType_estOutcome = factor(
				.$analysisType_estOutcome,
				levels = sort(
					unique(
						.$analysisType_estOutcome
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
				"riskStratum"
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
					.$riskStratum
				)
			)
		)

	p2 <-
		relative %>%
		dplyr::group_by(.$analysisType_estOutcome) %>%
		plotly::plot_ly(
			mode = "markers",
			x = ~risk + m,
			y = ~estimate,
			color = ~analysisType_estOutcome,
			colors = customColors[1:numberOfPoints],
			type = "scatter",
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
			estimate = 100*.$estimate,
			lower = 100*.$lower,
			upper = 100*.$upper,
			risk = as.numeric(
				as.factor(
					.$riskStratum
				)
			)
		)

	p3 <-
		absolute %>%
		plotly::plot_ly(
			mode = "markers",
			x = ~risk + m,
			y = ~estimate,
			color = ~analysisType_estOutcome,
			colors = customColors[1:numberOfPoints],
			type = "scatter",
			error_y = list(
				type = "data",
				array = absolute$upper - absolute$estimate,
				arrayminus = absolute$estimate - absolute$lower
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
					round(absolute$estimate, 2),
					" (",
					paste(
						round(absolute$lower, 2),
						round(absolute$upper, 2),
						sep = ", "
					),
					")"
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
			color = color,
			dash = "dash"
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
        .$analysisType,
        .$estOutcome,
        sep = "_"
      )
    )

  relative <- relative %>%
    dplyr::mutate(
      analysisType_estOutcome = paste(
        .$analysisType,
        .$estOutcome,
        sep = "_"
      )
    )

  absolute <- absolute %>%
    dplyr::mutate(
      analysisType_estOutcome = paste(
        .$analysisType,
        .$estOutcome,
        sep = "_"
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

  numberOfAnalyses <- length(
    unique(
      cases$analysisType
    )
  )

  numberOfPoints <- numberOfOutcomes * numberOfAnalyses
  if (numberOfPoints > 5) {
    stop("No more than 5 outcomes can be plotted at the same time")
  }

  m <- 0

  if (numberOfPoints == 2) {
    m <- c(-.15, .15)
  } else if (numberOfPoints == 3) {
    m <- c(-.15, 0, .15)
  } else if (numberOfPoints == 4) {
    m <- c(-.15, -.05, .05, .15)
  } else if (numberOfPoints == 5) {
    m <- c(-.15, -.075, 0, .075, .15)
  }

  relative <- relative %>%
    dplyr::mutate(
      estOutcome = factor(
        .$estOutcome,
        levels = sort(
          unique(
            .$estOutcome
          )
        )
      )
    )

  absolute <- absolute %>%
    dplyr::mutate(
      estOutcome = factor(
        .$estOutcome,
        levels = sort(
          unique(
            .$estOutcome
          )
        )
      )
    )

  quickMap <- data.frame(
    estOutcome = levels(relative$estOutcome),
    m = m
  )

  cases <-
    reshape2::melt(
      cases,
      id.vars = c(
        "riskStratum",
        "database",
        "estOutcome"
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
        .$estOutcome,
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
    plot_ly(
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
          .$riskStratum
        )
      )
    )

  p2 <-
    relative %>%
    dplyr::group_by(.$estOutcome) %>%
    plotly::plot_ly(
      mode = "markers",
      x = ~risk + m,
      y = ~estimate,
      color = ~estOutcome,
      colors = customColors[1:numberOfPoints],
      type = "scatter",
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
      estimate = 100*.$estimate,
      lower = 100*.$lower,
      upper = 100*.$upper,
      risk = as.numeric(
        as.factor(
          .$riskStratum
        )
      )
    )

  p3 <-
    absolute %>%
    plotly::plot_ly(
      mode = "markers",
      x = ~risk + m,
      y = ~estimate,
      color = ~estOutcome,
      colors = customColors[1:numberOfPoints],
      type = "scatter",
      error_y = list(
        type = "data",
        array = absolute$upper - absolute$estimate,
        arrayminus = absolute$estimate - absolute$lower
      ),
      hoverinfo = "text",
      hovertext = paste(
        "<b>Outcome:</b>",
        absolute$estOutcome,
        "<br><b>Database:</b>",
        absolute$database,
        "<br><b>Absolute difference:</b>",
        paste0(
          round(absolute$estimate, 2),
          " (",
          paste(
            round(absolute$lower, 2),
            round(absolute$upper, 2),
            sep = ", "
          ),
          ")"
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

