library(dplyr)

if(is.null(.GlobalEnv$shinySettings)){
  analysisSettingsList <- NULL
} else{
  analysisPath <- .GlobalEnv$shinySettings
}

mapOutcomes <- readRDS(
  file.path(
    analysisPath,
    "mapOutcomes.rds"
  )
)

mapTreatments <- readRDS(
  file.path(
    analysisPath,
    "mapTreatments.rds"
  )
)

balance <- readRDS(
  file.path(
    analysisPath,
    "balance.rds"
  )
)

psDensity <- readRDS(
  file.path(
    analysisPath,
    "psDensity.rds"
  )
)

mappedOverallAbsoluteResults <-
  readRDS(
    file.path(
      analysisPath,
      "mappedOverallAbsoluteResults.rds"
    )
  ) %>%
  left_join(mapOutcomes, by = c("estOutcome" = "idNumber")) %>%
  select(-estOutcome) %>%
  rename("estOutcome" = "label") %>%
  left_join(mapOutcomes, by = c("stratOutcome" = "idNumber")) %>%
  select(-stratOutcome) %>%
  rename("stratOutcome" = "label") %>%
  left_join(mapTreatments, by = c("treatment" = "idNumber")) %>%
  select(-treatment) %>%
  rename("treatment" = "label") %>%
  left_join(mapTreatments, by = c("comparator" = "idNumber")) %>%
  select(-comparator) %>%
  rename("comparator" = "label")

mappedOverallRelativeResults <-
  readRDS(
    file.path(
      analysisPath,
      "mappedOverallRelativeResults.rds"
    )
  ) %>%
  left_join(mapOutcomes, by = c("estOutcome" = "idNumber")) %>%
  select(-estOutcome) %>%
  rename("estOutcome" = "label") %>%
  left_join(mapOutcomes, by = c("stratOutcome" = "idNumber")) %>%
  select(-stratOutcome) %>%
  rename("stratOutcome" = "label") %>%
  left_join(mapTreatments, by = c("treatment" = "idNumber")) %>%
  select(-treatment) %>%
  rename("treatment" = "label") %>%
  left_join(mapTreatments, by = c("comparator" = "idNumber")) %>%
  select(-comparator) %>%
  rename("comparator" = "label")


mappedOverallCasesResults <-
  readRDS(
    file.path(
      analysisPath,
      "mappedOverallCasesResults.rds"
    )
  ) %>%
  left_join(mapOutcomes, by = c("estOutcome" = "idNumber")) %>%
  select(-estOutcome) %>%
  rename("estOutcome" = "label") %>%
  left_join(mapOutcomes, by = c("stratOutcome" = "idNumber")) %>%
  select(-stratOutcome) %>%
  rename("stratOutcome" = "label") %>%
  left_join(mapTreatments, by = c("treatment" = "idNumber")) %>%
  select(-treatment) %>%
  rename("treatment" = "label") %>%
  left_join(mapTreatments, by = c("comparator" = "idNumber")) %>%
  select(-comparator) %>%
  rename("comparator" = "label")

databaseOptions <- unique(
  mappedOverallAbsoluteResults$database
)

analysisTypeOptions <- unique(
  mappedOverallAbsoluteResults$analysisType
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

getBalance <- function(treat, comp, strat, est, db, anal, balance) {
  balance %>%
    filter(database == db & analysisType == anal) %>%
    left_join(mapOutcomes, by = c("estOutcome" = "idNumber")) %>%
    select(-estOutcome) %>%
    rename("estOutcome" = "label") %>%
    left_join(mapOutcomes, by = c("stratOutcome" = "idNumber")) %>%
    select(-stratOutcome) %>%
    rename("stratOutcome" = "label") %>%
    left_join(mapTreatments, by = c("treatmentId" = "idNumber")) %>%
    select(-treatmentId) %>%
    rename("treatment" = "label") %>%
    left_join(mapTreatments, by = c("comparatorId" = "idNumber")) %>%
    select(-comparatorId) %>%
    rename("comparator" = "label") %>%
    filter(
      stratOutcome == strat & estOutcome == est & treatment == treat & comparator == comp
    ) %>%
    return()
}


getPsDensity <- function(treat, comp, strat, est, db, anal, psDensity) {
  psDensity %>%
    filter(database == db & analysisType == anal) %>%
    left_join(mapOutcomes, by = c("estOutcome" = "idNumber")) %>%
    select(-estOutcome) %>%
    rename("estOutcome" = "label") %>%
    left_join(mapOutcomes, by = c("stratOutcome" = "idNumber")) %>%
    select(-stratOutcome) %>%
    rename("stratOutcome" = "label") %>%
    left_join(mapTreatments, by = c("comparatorId" = "idNumber")) %>%
    select(-comparatorId) %>%
    rename("comparator" = "label") %>%
    left_join(mapTreatments, by = c("treatmentId" = "idNumber")) %>%
    select(-treatmentId) %>%
    rename("treatment" = "label") %>%
    filter(
      stratOutcome == strat & estOutcome == est & treatment == treat & comparator == comp
    ) %>%
    return()
}

combinedPlot <- function(cases, relative, absolute, treatment, comparator) {

  cases <-
    reshape::melt(
      cases,
      id.vars = c("riskStratum", "database", "estOutcome"),
      measure.vars = c("casesComparator", "casesTreatment")
    ) %>%
    dplyr::mutate(
      variable = ifelse(variable == "casesComparator", comparator, treatment)
    )


  cases$test <- file.path(cases$database, cases$estOutcome, cases$variable)

  casesPlot <-
    ggplot2::ggplot(
      data = cases,
      ggplot2::aes(x = riskStratum, y = value * 100)
    ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = ggplot2::position_dodge(),
      ggplot2::aes(fill = test),
      width = 0.5
    ) +
    ggplot2::xlab("Risk Stratum") +
    ggplot2::ylab("Outcome Rate (%)") +
    ggplot2::geom_hline(yintercept = 0, size = 0.8) +
    ggplot2::scale_fill_brewer(palette = "Paired") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      legend.direction = "horizontal",
      legend.position = "top"
    ) +
    ggplot2::scale_y_reverse()

  relative$test <- file.path(relative$database, relative$estOutcome)

  rrrPlot <-
    ggplot2::ggplot(
      relative,
      ggplot2::aes(x = riskStratum, y = estimate, group = test, color = test)
    ) +
    ggplot2::geom_point(
      size = 2.5,
      position = ggplot2::position_dodge(width = 0.3)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower, ymax = upper),
      width = 0,
      position = ggplot2::position_dodge(width = 0.3)
    ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", size = 0.8) +
    ggplot2::xlab("Risk Stratum") +
    ggplot2::ylab("Hazard Ratio") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(
      values = c("#0099FF", "#009933", "#CC0000", "#FF9933", "#663399", "#CC9966")
    ) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "none",
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank()
    )

  absolute$test <- paste(absolute$database, absolute$estOutcome, sep = "/")

  arrPlot <-
    ggplot2::ggplot(
      absolute,
      ggplot2::aes(x = riskStratum, y = estimate * 100,
                   group = test,
                   color = test)
    ) +
    ggplot2::geom_point(
      size = 2.5,
      position = ggplot2::position_dodge(width = 0.3)
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower * 100, ymax = upper * 100),
      width = 0,
      position = ggplot2::position_dodge(width = 0.3)
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.8) +
    ggplot2::xlab("Risk stratum") + ggplot2::ylab("Absolute Risk Reduction (%)") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(
      values = c("#0099FF", "#009933", "#CC0000", "#FF9933", "#663399", "#CC9966")
    ) +
    ggplot2::theme(
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.title = ggplot2::element_blank()
    )

  ggpubr::ggarrange(casesPlot, rrrPlot, arrPlot, nrow = 3, align = "v")
}

