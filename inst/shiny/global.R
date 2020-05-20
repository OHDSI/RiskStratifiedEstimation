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

analyses <- readRDS(
  file.path(
    analysisPath,
    "analyses.rds"
  )
)

incidence <-
  readRDS(
    file.path(
      analysisPath,
      "incidence.rds"
    )
  ) %>%
  left_join(
    mapOutcomes,
    by = c(
      "estOutcome" = "outcome_id"
    )
  ) %>%
  select(-estOutcome) %>%
  rename(
    "estOutcome" = "outcome_name"
  ) %>%
  left_join(
    mapOutcomes,
    by = c(
      "stratOutcome" = "outcome_id"
    )
  ) %>%
  select(-stratOutcome) %>%
  rename(
    "stratOutcome" = "outcome_name"
  ) %>%
  left_join(
    mapExposures,
    by = c(
      "treatmentId" = "exposure_id"
    )
  ) %>%
  select(-treatmentId) %>%
  rename(
    "treatment" = "exposure_name"
  ) %>%
  left_join(
    mapExposures,
    by = c(
      "comparatorId" = "exposure_id"
    )
  ) %>%
  select(-comparatorId) %>%
  rename("comparator" = "exposure_name")

predictionPerformance <-
  readRDS(
    file.path(
      analysisPath,
      "predictionPerformance.rds"
    )
  ) %>%
  left_join(
    mapOutcomes,
    by = c(
      "stratOutcome" = "outcome_id"
    )
  ) %>%
  select(-stratOutcome) %>%
  rename(
    "stratOutcome" = "outcome_name"
  ) %>%
  left_join(
    mapExposures,
    by = c(
      "treatmentId" = "exposure_id"
    )
  ) %>%
  select(-treatmentId) %>%
  rename(
    "treatment" = "exposure_name"
  ) %>%
  left_join(
    mapExposures,
    by = c(
      "comparatorId" = "exposure_id"
    )
  ) %>%
  select(-comparatorId) %>%
  rename(
    "comparator" = "exposure_name"
  )

mappedOverallAbsoluteResults <-
  readRDS(
    file.path(
      analysisPath,
      "mappedOverallAbsoluteResults.rds"
    )
  ) %>%
  left_join(
    mapOutcomes,
    by = c(
      "estOutcome" = "outcome_id"
    )
  ) %>%
  select(-estOutcome) %>%
  rename(
    "estOutcome" = "outcome_name"
  ) %>%
  left_join(
    mapOutcomes,
    by = c(
      "stratOutcome" = "outcome_id"
    )
  ) %>%
  select(-stratOutcome) %>%
  rename(
    "stratOutcome" = "outcome_name"
  ) %>%
  left_join(
    mapExposures,
    by = c(
      "treatment" = "exposure_id"
    )
  ) %>%
  select(-treatment) %>%
  rename(
    "treatment" = "exposure_name"
  ) %>%
  left_join(
    mapExposures,
    by = c(
      "comparator" = "exposure_id"
    )
  ) %>%
  select(-comparator) %>%
  rename(
    "comparator" = "exposure_name"
  )

mappedOverallRelativeResults <-
  readRDS(
    file.path(
      analysisPath,
      "mappedOverallRelativeResults.rds"
    )
  ) %>%
  left_join(
    mapOutcomes,
    by = c(
      "estOutcome" = "outcome_id"
    )
  ) %>%
  select(-estOutcome) %>%
  rename(
    "estOutcome" = "outcome_name"
  ) %>%
  left_join(
    mapOutcomes,
    by = c(
      "stratOutcome" = "outcome_id"
    )
  ) %>%
  select(-stratOutcome) %>%
  rename(
    "stratOutcome" = "outcome_name"
  ) %>%
  left_join(
    mapExposures,
    by = c(
      "treatment" = "exposure_id"
    )
  ) %>%
  select(-treatment) %>%
  rename(
    "treatment" = "exposure_name"
  ) %>%
  left_join(
    mapExposures,
    by = c(
      "comparator" = "exposure_id"
    )
  ) %>%
  select(-comparator) %>%
  rename(
    "comparator" = "exposure_name"
  )


mappedOverallCasesResults <-
  readRDS(
    file.path(
      analysisPath,
      "mappedOverallCasesResults.rds"
    )
  ) %>%
  left_join(
    mapOutcomes,
    by = c(
      "estOutcome" = "outcome_id"
    )
  ) %>%
  select(-estOutcome) %>%
  rename(
    "estOutcome" = "outcome_name"
  ) %>%
  left_join(
    mapOutcomes,
    by = c(
      "stratOutcome" = "outcome_id"
    )
  ) %>%
  select(-stratOutcome) %>%
  rename(
    "stratOutcome" = "outcome_name"
  ) %>%
  left_join(
    mapExposures,
    by = c(
      "treatment" = "exposure_id"
    )
  ) %>%
  select(-treatment) %>%
  rename(
    "treatment" = "exposure_name"
  ) %>%
  left_join(
    mapExposures,
    by = c(
      "comparator" = "exposure_id"
    )
  ) %>%
  select(-comparator) %>%
  rename(
    "comparator" = "exposure_name"
  )

databaseOptions <- unique(
  mappedOverallAbsoluteResults$database
)

analysisTypeOptions <- unique(
  mappedOverallAbsoluteResults$analysisType
)

stratOptions <- unique(
  mappedOverallAbsoluteResults$stratOutcome
)


getResults <- function(treat, comp, strat, est, db, anal,
                       mappedOverallRelativeResults,
                       mappedOverallAbsoluteResults,
                       mappedOverallCasesResults) {

  result <- list()

  result$relative <-
    mappedOverallRelativeResults %>%
    filter(
      stratOutcome %in% strat & estOutcome %in% est & treatment %in% treat & comparator %in% comp & database %in% db & analysisType %in% anal
    )

  result$absolute <-
    mappedOverallAbsoluteResults %>%
    filter(
      stratOutcome %in% strat & estOutcome %in% est & treatment %in% treat & comparator %in% comp & database %in% db & analysisType %in% anal
    )

  result$cases <-
    mappedOverallCasesResults %>%
    filter(
      stratOutcome %in% strat & estOutcome %in% est & treatment %in% treat & comparator %in% comp & database %in% db & analysisType %in% anal
    )

  return(result)


}


getIncidence <- function(treat,
                         comp,
                         strat,
                         est,
                         db,
                         anal,
                         incidence) {
  incidence %>%
    filter(
      stratOutcome %in% strat & estOutcome %in% est & treatment %in% treat & comparator %in% comp & database %in% db & analysisType %in% anal
    ) %>%
    return()
}


getPredictionPerformance <- function(treat,
                                     comp,
                                     strat,
                                     coh,
                                     db,
                                     predictionPerformance) {

  predictionPerformance %>%
    filter(
      stratOutcome %in% strat & cohort %in% coh & treatment %in% treat & comparator %in% comp & database %in% db
    ) %>%
    return()

}

getBalance <- function(treat,
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
    left_join(
      mapExposures,
      by = c("treatment_id" = "exposure_id")
    ) %>%
    rename(
      "treatment_name" = "exposure_name"
    ) %>%
    left_join(
      mapExposures,
      by = c("comparator_id" = "exposure_id")
    ) %>%
    rename(
      "comparator_name" = "exposure_name"
    ) %>%
    filter(
      treatment_name == treat,
      comparator_name == comp,
      database == db,
      analysis_type == anal
    )

  stratOutcomeId <- mapOutcomes %>%
    filter(outcome_name == strat) %>%
    select(outcome_id) %>%
    unlist()

  estOutcomeId <- mapOutcomes %>%
    filter(outcome_name == est) %>%
    select(outcome_id) %>%
    unlist()

  treatmentId <- mapExposures %>%
    filter(exposure_name == treat) %>%
    select(exposure_id) %>%
    unlist()

  comparatorId <- mapExposures %>%
    filter(exposure_name == comp) %>%
    select(exposure_id) %>%
    unlist()

  readRDS(
    file.path(
      analysisPath,
      paste0(
        paste(
          "balance",
          res$analysis_id,
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
    left_join(
      mapExposures,
      by = c("treatment_id" = "exposure_id")
    ) %>%
    rename(
      "treatment_name" = "exposure_name"
    ) %>%
    left_join(
      mapExposures,
      by = c("comparator_id" = "exposure_id")
    ) %>%
    rename(
      "comparator_name" = "exposure_name"
    ) %>%
    filter(
      treatment_name == treat,
      comparator_name == comp,
      database == db,
      analysis_type == anal
    )

  stratOutcomeId <- mapOutcomes %>%
    filter(outcome_name == strat) %>%
    select(outcome_id) %>%
    unlist()

  estOutcomeId <- mapOutcomes %>%
    filter(outcome_name == est) %>%
    select(outcome_id) %>%
    unlist()

  treatmentId <- mapExposures %>%
    filter(exposure_name == treat) %>%
    select(exposure_id) %>%
    unlist()

  comparatorId <- mapExposures %>%
    filter(exposure_name == comp) %>%
    select(exposure_id) %>%
    unlist()

  readRDS(
    file.path(
      analysisPath,
      paste0(
        paste(
          "psDensity",
          res$analysis_id,
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


getAuc <- function(treat,
                   comp,
                   strat,
                   db,
                   anal,
                   predictionPopulation,
                   analyses,
                   mapExposures,
                   mapOutcomes,
                   analysisPath) {

  res <- analyses %>%
    left_join(
      mapExposures,
      by = c("treatment_id" = "exposure_id")
    ) %>%
    rename(
      "treatment_name" = "exposure_name"
    ) %>%
    left_join(
      mapExposures,
      by = c("comparator_id" = "exposure_id")
    ) %>%
    rename(
      "comparator_name" = "exposure_name"
    ) %>%
    filter(
      treatment_name == treat,
      comparator_name == comp,
      database == db,
      analysis_type == anal
    )

  stratOutcomeId <- mapOutcomes %>%
    filter(outcome_name == strat) %>%
    select(outcome_id) %>%
    unlist()

  treatmentId <- mapExposures %>%
    filter(exposure_name == treat) %>%
    select(exposure_id) %>%
    unlist()

  comparatorId <- mapExposures %>%
    filter(exposure_name == comp) %>%
    select(exposure_id) %>%
    unlist()

  readRDS(
    file.path(
      analysisPath,
      paste0(
        paste(
          "auc",
          predictionPopulation,
          res$analysis_id,
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


getCalibration <- function(treat,
                           comp,
                           strat,
                           db,
                           anal,
                           predictionPopulation,
                           analyses,
                           mapExposures,
                           mapOutcomes,
                           analysisPath) {

  res <- analyses %>%
    left_join(
      mapExposures,
      by = c("treatment_id" = "exposure_id")
    ) %>%
    rename(
      "treatment_name" = "exposure_name"
    ) %>%
    left_join(
      mapExposures,
      by = c("comparator_id" = "exposure_id")
    ) %>%
    rename(
      "comparator_name" = "exposure_name"
    ) %>%
    filter(
      treatment_name == treat,
      comparator_name == comp,
      database == db,
      analysis_type == anal
    )

  stratOutcomeId <- mapOutcomes %>%
    filter(outcome_name == strat) %>%
    select(outcome_id) %>%
    unlist()

  treatmentId <- mapExposures %>%
    filter(exposure_name == treat) %>%
    select(exposure_id) %>%
    unlist()

  comparatorId <- mapExposures %>%
    filter(exposure_name == comp) %>%
    select(exposure_id) %>%
    unlist()

  readRDS(
    file.path(
      analysisPath,
      paste0(
        paste(
          "calibration",
          predictionPopulation,
          res$analysis_id,
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

combinedPlot <- function(cases,
                         relative,
                         absolute,
                         treatment,
                         comparator) {


  customColors <- c(
    "#0099FF",
    "#009933",
    "#CC0000",
    "#FF9933",
    "#663399",
    "#CC9966"
  )

  nOutcomes <- length(
    unique(
      cases$estOutcome
    )
  )

  if (nOutcomes > 5) {
    stop("No more than 5 outcomes can be plotted at the same time")
  }

  m <- 0

  if ( nOutcomes == 2) {
    m <- c(-.15, .15)
  } else if (nOutcomes == 3) {
    m <- c(-.15, 0, .15)
  } else if (nOutcomes == 4) {
    m <- c(-.15, -.05, .05, .15)
  } else if (nOutcomes == 5) {
    m <- c(-.15, -.075, 0, .075, .15)
  }

  relative <- relative %>%
    mutate(
      estOutcome = factor(
        estOutcome,
        levels = sort(
          unique(
            estOutcome
          )
        )
      )
    )

  absolute <- absolute %>%
    mutate(
      estOutcome = factor(
        estOutcome,
        levels = sort(
          unique(
            estOutcome
          )
        )
      )
    )

  quickMap <- data.frame(
    estOutcome = levels(relative$estOutcome),
    m = m
  )

  cases <-
    reshape::melt(
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
    mutate(
      variable = ifelse(
        variable == "casesComparator",
        comparator,
        treatment
      ),
      g = paste(
        estOutcome,
        variable,
        sep = "/"
      ),
      value = 100*value,
      riskStratum = as.numeric(
        as.factor(
          riskStratum
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
    left_join(quickMap) %>%
    mutate(
      risk = as.numeric(
        as.factor(
          riskStratum
        )
      )
    )

  p2 <-
    relative %>%
    group_by(estOutcome) %>%
    plot_ly(
      mode = "markers",
      x = ~risk + m,
      y = ~estimate,
      color = ~estOutcome,
      colors = customColors[1:nOutcomes],
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
        tickformat = ',d'
      )
    ) %>%
    layout(
      shapes = hline(1)
    )

  absolute <-
    absolute %>%
    left_join(quickMap) %>%
    mutate(
      estimate = 100*estimate,
      lower = 100*lower,
      upper = 100*upper,
      risk = as.numeric(
        as.factor(
          riskStratum
        )
      )
    )

  p3 <-
    absolute %>%
    # group_by(estOutcome) %>%
    plot_ly(
      mode = "markers",
      x = ~risk + m,
      y = ~estimate,
      color = ~estOutcome,
      colors = customColors[1:nOutcomes],
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
    layout(
      yaxis = list(
        title = "Absolute risk reduction (%)"
      ),
      xaxis = list(
        title = "Risk stratum",
        tickformat = ',d'
      )
    )

  subplot(p1, p2, p3, shareX = TRUE, nrows = 3, titleY = T)


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
  infoTag <- tags$small(class = "badge pull-right action-button",
                        style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
                        type = "button",
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}
