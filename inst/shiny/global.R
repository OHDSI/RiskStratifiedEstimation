library(dplyr)

analysisDir <- file.path(
  analysisSettings$saveDirectory,
  analysisSettings$analysisId,
  "Shiny"
)
mapOutcomes <- readRDS(
  file.path(
    analysisDir,
    "data",
    "mapOutcomes.rds"
  )
)

mapTreatments <- readRDS(
  file.path(
    analysisDir,
    "data",
    "mapTreatments.rds"
  )
)

mappedOverallAbsoluteResults <- readRDS(
  file.path(
    analysisDir,
    "data",
    "mappedOverallAbsoluteResults.rds"
  )
)
mappedOverallRelativeResults <- readRDS(
  file.path(
    analysisDir,
    "data",
    "mappedOverallRelativeResults.rds"
  )
)
mappedOverallCasesResults <- readRDS(
  file.path(
    analysisDir,
    "data",
    "mappedOverallCasesResults.rds"
  )
)

predictionOutcomes <- unique(mappedOverallAbsoluteResults$stratOutcome)

getResults <- function(treat, comp, strat, est, db, anal) {

  res <- list()

  res$relative <-
    readRDS(
      file.path(
        analysisDir,
        "data",
        "mappedOverallRelativeResults.rds"
      )
    ) %>%
    filter(
      stratOutcome == strat & estOutcome %in% est & analysis == anal & treatment == treat & comparator == comp & database == db
    )

  res$absolute <-
    readRDS(
      file.path(
        analysisDir,
        "data",
        "mappedOverallAbsoluteResults.rds"
      )
    ) %>%
    filter(
      stratOutcome == strat & estOutcome %in% est & analysis == anal & treatment == treat & comparator == comp & database == db
    )

  res$cases <- readRDS(
    file.path(
     analysisDir,
     "data",
     "mappedOverallCasesResults.rds"
    )
  ) %>%
    filter(
      stratOutcome == strat & estOutcome %in% est & analysis == anal & treatment == treat & comparator == comp & database == db
    )

  return(res)


}



combinedPlot <- function(cases, relative, absolute, treatment, comparator) {

  cases <- reshape::melt(cases, id.vars = c("riskStratum", "database", "estOutcome"),
                         measure.vars = c("casesComparator", "casesTreatment")) %>% mutate(variable = ifelse(variable ==
                                                                                                               "casesComparator", comparator, treatment))


  cases$test <- file.path(cases$database, cases$estOutcome, cases$variable)

  casesPlot <- ggplot2::ggplot(data = cases, ggplot2::aes(x = riskStratum, y = value *
                                                            100)) + ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(),
                                                                                      ggplot2::aes(fill = test), width = 0.5) + ggplot2::xlab("Risk Stratum") + ggplot2::ylab("Outcome Rate (%)") +
    ggplot2::geom_hline(yintercept = 0, size = 0.8) + ggplot2::scale_fill_brewer(palette = "Paired") +
    ggplot2::theme_minimal() + ggplot2::theme(legend.title = ggplot2::element_blank(),
                                              axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                                              legend.direction = "horizontal", legend.position = "top") + ggplot2::scale_y_reverse()

  relative$test <- file.path(relative$database, relative$estOutcome)

  rrrPlot <- ggplot2::ggplot(relative, ggplot2::aes(x = riskStratum, y = estimate, group = test,
                                                    color = test)) + ggplot2::geom_point(size = 2.5, position = ggplot2::position_dodge(w = 0.3)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0, position = ggplot2::position_dodge(w = 0.3)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", size = 0.8) + ggplot2::xlab("Risk Stratum") +
    ggplot2::ylab("Hazard Ratio") + ggplot2::theme_minimal() + ggplot2::scale_color_manual(values = c("#0099FF",
                                                                                                      "#009933", "#CC0000", "#FF9933", "#663399", "#CC9966")) + ggplot2::theme(legend.title = ggplot2::element_blank(),
                                                                                                                                                                               legend.position = "none", axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())

  absolute$test <- paste(absolute$database, absolute$estOutcome, sep = "/")

  arrPlot <- ggplot2::ggplot(absolute, ggplot2::aes(x = riskStratum, y = estimate * 100,
                                                    group = test, color = test)) + ggplot2::geom_point(size = 2.5, position = ggplot2::position_dodge(w = 0.3)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower * 100, ymax = upper * 100), width = 0,
                           position = ggplot2::position_dodge(w = 0.3)) + ggplot2::geom_hline(yintercept = 0,
                                                                                              linetype = "dashed", size = 0.8) + ggplot2::xlab("Risk stratum") + ggplot2::ylab("Absolute Risk Reduction (%)") +
    ggplot2::theme_minimal() + ggplot2::scale_color_manual(values = c("#0099FF", "#009933",
                                                                      "#CC0000", "#FF9933", "#663399", "#CC9966")) + ggplot2::theme(legend.direction = "horizontal",
                                                                                                                                    legend.position = "bottom", legend.title = ggplot2::element_blank())

  ggpubr::ggarrange(casesPlot, rrrPlot, arrPlot, nrow = 3, align = "v")
}

