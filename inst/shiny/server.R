library(dplyr)

shiny::shinyServer(
  function(
    input,
    output,
    session
  ) {
    shiny::observe(
      {
        filteredEstimationOutcomes <- mappedOverallRelativeResults %>%
          dplyr::filter(
            stratOutcome == stratOutcome
          ) %>%
          dplyr::select(
            estOutcome
          )


        shiny::updateSelectInput(
          session = session,
          inputId = "estOutcome",
          choices = unique(filteredEstimationOutcomes)
        )
      }
    )

    resultSubset <- shiny::reactive(
      {
        results <- getResults(
          treat = input$treatment,
          comp = input$comparator,
          strat = input$stratOutcome,
          est = input$estOutcome,
          anal = input$analysis,
          db = input$database,
          mappedOverallRelativeResults = mappedOverallRelativeResults,
          mappedOverallAbsoluteResults = mappedOverallAbsoluteResults,
          mappedOverallCasesResults = mappedOverallCasesResults
        )
        return(results)
      }
    )

    overallResultSubset <- shiny::reactive(
      {
        results <- getResultsOverall(
          treat = input$treatment,
          comp = input$comparator,
          strat = input$stratOutcome,
          anal = input$analysis,
          db = input$database,
          overallMappedOverallRelativeResults = overallMappedOverallRelativeResults
        )
        return(results)
      }
    )

    if (hasNegativeControls) {
      negativeControlsSubset <- shiny::reactive(
        {
          results <- getNegativeControls(
            treat = input$treatment,
            comp = input$comparator,
            strat = input$stratOutcome,
            anal = input$analysis,
            db = input$database,
            negativeControls = negativeControls
          )
        }
      )
    }

    if (hasOverallNegativeControls) {
      overallNegativeControlsSubset <- shiny::reactive(
        {
          results <- getResultsOverall(
            treat = input$treatment,
            comp = input$comparator,
            anal = input$analysis,
            db = input$database,
            overallMappedOverallRelativeResults = overallNegativeControls
          )
          return(results)
        }
      )
    }


    incidenceSubset <- shiny::reactive(
      {
        res <- getIncidence(
          treat = input$treatment,
          comp = input$comparator,
          strat = input$stratOutcome,
          est = input$estOutcome,
          anal = input$analysis,
          db = input$database,
          incidence = incidence
        )

        return(res)

      }
    )

    overallIncidenceSubset <- shiny::reactive(
      {
        res <- getIncidenceOverall(
          treat = input$treatment,
          comp = input$comparator,
          strat = input$stratOutcome,
          anal = input$analysis,
          db = input$database,
          incidence = overallIncidence
        )

        return(res)

      }
    )

    output$overallIncidence <- DT::renderDataTable(
      {

        res <- overallIncidenceSubset()

        treatment <- res$treatment[1]
        comparator <- res$comparator[1]
        outcome <- res$stratOutcome[1]

        table <- res %>%
          dplyr::select(
            stratOutcome,
            analysisType,
            treatmentPersons,
            treatmentDays,
            treatmentOutcomes,
            comparatorPersons,
            comparatorDays,
            comparatorOutcomes
          ) %>%
          dplyr::mutate(
            treatmentDays = treatmentDays / 365.25,
            comparatorDays = comparatorDays / 365.25
          )

        table <-
          DT::datatable(
            table,
            colnames = c(
              "Outcome",
              "Analysis",
              "Treatment subjects",
              "Treatment years",
              "Treatment events",
              "Comparator subjects",
              "Comparator years",
              "Comparator events"
            ),
            caption = htmltools::tags$caption(
              style = "caption-side: top; text-align: left;",
              "Table 1: Number of subjects, follow-up time (in years), event rates in the treatment",
              htmltools::em(
                paste0(
                  "(",
                  treatment,
                  ")"
                )
              ),
              "and the comparator",
              htmltools::em(
                paste0(
                  "(",
                  comparator,
                  ")"
                )
              ),
              "groups within strata of predicted risk",
              htmltools::em(
                paste0(
                  "(",
                  outcome,
                  ")"
                )
              )
            )
          ) %>%
          DT::formatCurrency(
            columns =  "treatmentPersons",
            currency = "",
            interval = 3,
            mark = ",",
            digits = 0
          ) %>%
          DT::formatCurrency(
            "comparatorPersons",
            currency = "",
            interval = 3,
            mark = ",",
            digits = 0
          ) %>%
          DT::formatCurrency(
            "treatmentDays",
            currency = "",
            interval = 3,
            mark = ",",
            digits = 0
          ) %>%
          DT::formatCurrency(
            "comparatorDays",
            currency = "",
            interval = 3,
            mark = ",",
            digits = 0
          ) %>%
          DT::formatCurrency(
            "treatmentOutcomes",
            currency = "",
            interval = 3,
            mark = ",",
            digits = 0
          ) %>%
          DT::formatCurrency(
            "comparatorOutcomes",
            currency = "",
            interval = 3,
            mark = ",",
            digits = 0
          )
        return(table)
      }
    )


    output$mainTableIncidence <- DT::renderDataTable(
      {
        res <- incidenceSubset()
        treatment <- res$treatment[1]
        comparator <- res$comparator[1]
        outcome <- res$stratOutcome[1]

        table <- res %>%
          dplyr::select(
            estOutcome,
            analysisType,
            riskStratum,
            treatmentPersons,
            treatmentDays,
            treatmentOutcomes,
            comparatorPersons,
            comparatorDays,
            comparatorOutcomes
          ) %>%
          dplyr::mutate(
            treatmentDays = treatmentDays / 365.25,
            comparatorDays = comparatorDays / 365.25
          )

        table <-
          DT::datatable(
            table,
            colnames = c(
              "Outcome",
              "Analysis",
              "Risk stratum",
              "Treatment subjects",
              "Treatment years",
              "Treatment events",
              "Comparator subjects",
              "Comparator years",
              "Comparator events"
            ),
            caption = htmltools::tags$caption(
              style = "caption-side: top; text-align: left;",
              "Table 1: Number of subjects, follow-up time (in years), event rates in the treatment",
              htmltools::em(
                paste0(
                  "(",
                  treatment,
                  ")"
                )
              ),
              "and the comparator",
              htmltools::em(
                paste0(
                  "(",
                  comparator,
                  ")"
                )
              ),
              "groups within strata of predicted risk",
              htmltools::em(
                paste0(
                  "(",
                  outcome,
                  ")"
                )
              )
            )
          ) %>%
          DT::formatCurrency(
            columns =  "treatmentPersons",
            currency = "",
            interval = 3,
            mark = ",",
            digits = 0
          ) %>%
          DT::formatCurrency(
            "comparatorPersons",
            currency = "",
            interval = 3,
            mark = ",",
            digits = 0
          ) %>%
          DT::formatCurrency(
            "treatmentDays",
            currency = "",
            interval = 3,
            mark = ",",
            digits = 0
          ) %>%
          DT::formatCurrency(
            "comparatorDays",
            currency = "",
            interval = 3,
            mark = ",",
            digits = 0
          ) %>%
          DT::formatCurrency(
            "treatmentOutcomes",
            currency = "",
            interval = 3,
            mark = ",",
            digits = 0
          ) %>%
          DT::formatCurrency(
            "comparatorOutcomes",
            currency = "",
            interval = 3,
            mark = ",",
            digits = 0
          )

        return(table)

      }
    )

    output$overallResults <- DT::renderDataTable(
      {
        res <- overallResultSubset()

        # table <- res
        table <- res %>%
          dplyr::mutate(
            estimate = round(
              estimate,
              digits = 2
            ),
            lower = round(
              lower,
              digits = 2
            ),
            upper = round(
              upper,
              digits = 2
            )
          ) %>%
          dplyr::select(
            database,
            analysisType,
            treatment,
            comparator,
            stratOutcome,
            estimate,
            lower,
            upper
          )

        tableColNames <-  c(
          "Database",
          "Analysis",
          "Treatment",
          "Comparator",
          "Outcome",
          "Hazard ratio",
          "Lower",
          "Upper"
        )

        if (hasOverallNegativeControls) {
          ncs <- overallNegativeControlsSubset() %>%
            dplyr::mutate(logRr = log(estimate))
          dat <- overallResultSubset() %>%
            dplyr::mutate(logRr = log(estimate))
          mod <- EmpiricalCalibration::fitSystematicErrorModel(
            logRr =  ncs$logRr,
            seLogRr = ncs$seLogRr,
            trueLogRr = rep(0, nrow(ncs))
          )
          table <- table %>%
            dplyr::bind_cols(
              EmpiricalCalibration::calibrateConfidenceInterval(
                logRr = dat$logRr,
                seLogRr = dat$seLogRr,
                model = mod
              ) %>%
                dplyr::mutate(
                  calibratedEstimate = round(exp(logRr), digits = 2),
                  calibratedLower = round(exp(logLb95Rr), digits = 2),
                  calibratedUpper = round(exp(logUb95Rr), digits = 2)
                ) %>%
                dplyr::select(
                  calibratedEstimate,
                  calibratedLower,
                  calibratedUpper
                )
            )
          tableColNames <- c(
            tableColNames,
            "Calibrated HR",
            "Calibrated lower",
            "Calibrated upper"
          )

        }

        table <- DT::datatable(
          data = table,
          colnames = tableColNames,
          caption = htmltools::tags$caption(
            style = "caption-side: top; text-align: left;",
            "Table 2: Overall hazard ratios comparing treatment",
            htmltools::em(
              paste(
                paste0(
                  "(",
                  input$treatment,
                  ")"
                )
              ),
              "to comparator",
              htmltools::em(
                paste0(
                  "(",
                  input$comparator,
                  ")"
                )
              )
            )
          )
        )

        return(table)
      }
    )

    output$mainTableRelative <- DT::renderDataTable(
      {
        res <- resultSubset()
        treatment <- res$relative$treatment[1]
        comparator <- res$relative$comparator[1]
        outcome <- res$relative$stratOutcome[1]
        analysisType <- res$relative$analysisType[1]

        table <- res$relative %>%
          dplyr::mutate(
            combined = paste(
              round(estimate, 2),
              paste0(
                "(",
                round(lower, 2),
                ", ",
                round(upper, 2),
                ")"
              )
            )
          ) %>%
          dplyr::rename(
            Outcome = estOutcome
          ) %>%
          dplyr::select(
            Outcome,
            riskStratum,
            analysisType,
            combined
          ) %>%
          tidyr::spread(riskStratum, combined)

        if (hasNegativeControls) {
          ncs <- negativeControlsSubset() %>%
            dplyr::mutate(logRr = log(estimate))
          tmp <- calibrateRiskStrataCis(
            negativeControls = ncs,
            positiveControls = res$relative %>%
              dplyr::mutate(
                logRr = log(estimate)
              )
          ) %>%
            dplyr::mutate(
              combined = paste(
                round(estimate, 2),
                paste0(
                  "(",
                  round(lower, 2),
                  ", ",
                  round(upper, 2),
                  ") [CAL]"
                )
              ),
              analysisType = analysisType
            ) %>%
            dplyr::select(
              Outcome,
              analysisType,
              riskStratum,
              combined
            ) %>%
            tidyr::spread(riskStratum, combined)

          table <- table %>%
            dplyr::bind_rows(
              tmp
            )
        }

        table %>%
          DT::datatable(
            caption = htmltools::tags$caption(
              style = "caption-side: top; text-align: left;",
              "Table 2: Hazard ratios comparing treatment",
              htmltools::em(
                paste0(
                  "(",
                  treatment,
                  ")"
                )
              ),
              "to comparator",
              htmltools::em(
                paste0(
                  "(",
                  comparator,
                  ")"
                )
              ),
              "within strata of predicted risk",
              htmltools::em(
                paste0(
                  "(",
                  outcome,
                  ")"
                )
              )

            )
          )

        return(table)

      }
    )

    output$mainTableAbsolute <- DT::renderDataTable(
      {

        res <- resultSubset()

        treatment <- res$absolute$treatment[1]
        comparator <- res$absolute$comparator[1]
        outcome <- res$absolute$stratOutcome[1]

        table <-
          res$absolute %>%
          dplyr::mutate(
            combined = paste(
              round(100*estimate, 2),
              paste0(
                "(",
                round(100*lower, 2),
                ", ",
                round(100*upper, 2),
                ")"
              )
            )
          ) %>%
          dplyr::rename(
            Outcome = estOutcome
          ) %>%
          dplyr::select(
            Outcome,
            riskStratum,
            combined,
            analysisType
          ) %>%
          tidyr::spread(riskStratum, combined) %>%
          DT::datatable(
            caption = htmltools::tags$caption(
              style = "caption-side: top; text-align: left;",
              "Table 3: Absolute risk reduction (%) when comparing treatment",
              htmltools::em(
                paste0(
                  "(",
                  treatment,
                  ")"
                )
              ),
              "to comparator",
              htmltools::em(
                paste0(
                  "(",
                  comparator,
                  ")"
                )
              ),
              "within strata of predicted risk",
              htmltools::em(
                paste0(
                  "(",
                  outcome,
                  ")"
                )
              )
            )
          )


        return(table)

      }
    )


    output$combinedPlot <- plotly::renderPlotly(
      {

        res <- resultSubset()

        plot <-
          combinedPlot(
            cases = res$cases,
            relative = res$relative,
            absolute = res$absolute,
            treatment = input$treatment,
            comparator = input$comparator
          )
        return(plot)

      }
    )

    shiny::observe(
      {
        x <- input$estOutcome

        shiny::updateSelectInput(
          session = session,
          inputId = "estOutcomeEstimation",
          choices = x
        )
      }
    )


    balanceSubset <- shiny::reactive(
      {
        res <- getBalance(
          treat = input$treatment,
          comp = input$comparator,
          strat = input$stratOutcome,
          anal = input$analysis,
          est = input$estOutcomeEstimation,
          db = input$database,
          analyses = analyses,
          mapExposures = mapExposures,
          mapOutcomes = mapOutcomes,
          analysisPath = analysisPath)
        return(res)
      }
    )

    output$balanceTable <- DT::renderDataTable(
      {
        res <- balanceSubset() %>%
          dplyr::mutate(
            afterMatchingStdDiff = abs(
              round(
                .$afterMatchingStdDiff,
                2
              )
            ),
            beforeMatchingStdDiff = abs(
              round(
                .$beforeMatchingStdDiff,
                2
              )
            )
          ) %>%
          dplyr::select(
            c(
              "covariateId",
              "riskStratum",
              "covariateName",
              "beforeMatchingStdDiff",
              "afterMatchingStdDiff"
            )
          )

        DT::datatable(
          res,
          colnames = c(
            "Covariate Id",
            "Risk stratum",
            "Covariate name",
            "Before",
            "After"
          ),
          options = list(
            order = list(
              5,
              "desc"
            )
          )
        ) %>%
          return()
      }
    )

    overallBalanceSubset <- shiny::reactive(
      {
        res <- getBalance(
          treat = input$treatment,
          comp = input$comparator,
          strat = input$stratOutcome,
          anal = input$analysis,
          est = input$estOutcomeEstimation,
          db = input$database,
          analyses = analyses,
          mapExposures = mapExposures,
          mapOutcomes = mapOutcomes,
          analysisPath = analysisPath,
          isOverall = TRUE
        )
        return(res)
      }
    )

    psDensitySubset <- shiny::reactive(
      {
        res <- getPsDensity(
          treat = input$treatment,
          comp = input$comparator,
          strat = input$stratOutcome,
          anal = input$analysis,
          est = input$estOutcomeEstimation,
          db = input$database,
          analyses = analyses,
          mapExposures = mapExposures,
          mapOutcomes = mapOutcomes,
          analysisPath = analysisPath)
        return(res)
      }
    )

    output$overallBalanceTable <- DT::renderDT(
      {
        res <- overallBalanceSubset() %>%
          dplyr::mutate(
            afterMatchingStdDiff = abs(
              round(
                .$afterMatchingStdDiff,
                2
              )
            ),
            beforeMatchingStdDiff = abs(
              round(
                .$beforeMatchingStdDiff,
                2
              )
            )
          ) %>%
          dplyr::select(
            c(
              "covariateId",
              "covariateName",
              "beforeMatchingStdDiff",
              "afterMatchingStdDiff"
            )
          )

        DT::datatable(
          res,
          colnames = c(
            "Covariate Id",
            "Covariate name",
            "Before",
            "After"
          ),
          options = list(
            order = list(
              4,
              "desc"
            )
          )
        ) %>%
          return()
      }
    )


    output$evaluationPlotPs <- shiny::renderPlot(
      {
        psDensitySubset() %>%
          dplyr::mutate(
            treatment = ifelse(
              treatment == 1,
              treatmentId,
              comparatorId
            )
          ) %>%
          dplyr::left_join(
            mapExposures,
            by = c("treatment" = "exposure_id")
          ) %>%
          plotPsDensity(riskStratified = TRUE)
      }
    )

    overallPsDensitySubset <- reactive(
      {
        res <- getPsDensityOverall(
          treat = input$treatment,
          comp = input$comparator,
          strat = input$stratOutcome,
          anal = input$analysis,
          db = input$database,
          analyses = analyses,
          mapExposures = mapExposures,
          mapOutcomes = mapOutcomes,
          analysisPath = analysisPath)
        return(res)
      }
    )
    output$overallEvaluationPlotPs <- shiny::renderPlot(
      {

        overallPsDensitySubset() %>%
          dplyr::left_join(
            mapExposures,
            by = c("treatment" = "exposure_id")
          ) %>%
          plotPsDensity()
      }
    )

    output$evaluationPlotBalance <- shiny::renderPlot(
      {
        balanceSubset() %>%
          CohortMethod::plotCovariateBalanceScatterPlot(
            beforeLabel = "Before stratification",
            afterLabel = "After stratification"
          ) +
          ggplot2::facet_grid(
            ~riskStratum
          )
      }
    )

    output$overallEvaluationPlotBalance <- shiny::renderPlot(
      {
        overallBalanceSubset() %>%
          CohortMethod::plotCovariateBalanceScatterPlot(
            beforeLabel = "Before stratification",
            afterLabel = "After stratification"
          )
      }
    )

    calibrationSubset <- shiny::reactive(
      {
        res <- getCalibration(
          treat = input$treatment,
          comp = input$comparator,
          strat = input$stratOutcome,
          db = input$database,
          anal = input$analysis,
          predictionPopulation = input$predictionPopulation,
          analyses = analyses,
          mapExposures = mapExposures,
          mapOutcomes = mapOutcomes,
          analysisPath = analysisPath)
        return(res)
      }
    )

    output$calibrationPlot <- shiny::renderPlot(
      {
        calibrationSubset() %>%
          dplyr::mutate(
            cohort = factor(
              .$cohort,
              levels = c(
                "Matched",
                "EntirePopulation",
                "Treatment",
                "Comparator"
              )
            )
          ) %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = averagePredictedProbability,
              y = observedIncidence,
              ymin = lower,
              ymax = upper
            )
          ) +
          ggplot2::geom_point(
            size = 2,
            color = "black"
          ) +
          ggplot2::geom_errorbar() +
          ggplot2::geom_line(
            colour = 'darkgrey'
          ) +
          ggplot2::geom_abline(
            intercept = 0,
            slope = 1,
            linetype = 5,
            size = 0.4,
            show.legend = TRUE
          ) +
          ggplot2::scale_x_continuous(
            name = "Average Predicted Probability"
          ) +
          ggplot2::scale_y_continuous(
            name = "Observed Fraction With Outcome"
          ) +
          ggplot2::facet_grid(
            ~cohort
          ) +
          ggplot2::theme_bw()

      }
    )

    aucSubset <- shiny::reactive(
      {
        res <- getAuc(
          treat = input$treatment,
          comp = input$comparator,
          strat = input$stratOutcome,
          db = input$database,
          anal = input$analysis,
          predictionPopulation = input$predictionPopulation,
          analyses = analyses,
          mapExposures = mapExposures,
          mapOutcomes = mapOutcomes,
          analysisPath = analysisPath)
        return(res)
      }
    )

    predictionPerformanceSubset <- shiny::reactive(
      {
        res <- getPredictionPerformance(
          treat = input$treatment,
          comp = input$comparator,
          strat = input$stratOutcome,
          coh = input$predictionPopulation,
          db = input$database,
          predictionPerformance = predictionPerformance
        )
        return(res)
      }
    )

    output$discriminationPlot <- shiny::renderPlot(
      {
        plot <-
          aucSubset() %>%
          dplyr::mutate(
            cohort = factor(
              .$cohort,
              levels = c(
                "Matched",
                "EntirePopulation",
                "Treatment",
                "Comparator"
              )
            )
          ) %>%
          ggplot2::ggplot(
            ggplot2::aes(
              x = fpRate,
              y = sens
            )
          ) +
          ggplot2::geom_abline(
            intercept = 0,
            slope = 1
          ) +
          ggplot2::geom_area(
            color = grDevices::rgb(
              red = 0,
              green = 0,
              blue = 0.8,
              alpha = 0.8
            ),
            fill = grDevices::rgb(
              red = 0,
              green = 0,
              blue = 0.8,
              alpha = 0.4
            )
          ) +
          ggplot2::scale_x_continuous(
            name = "1 - specificity"
          ) +
          ggplot2::scale_y_continuous(
            name = "Sensitivity"
          ) +
          ggplot2::theme_bw()

        labels <- predictionPerformanceSubset() %>%
          dplyr::mutate(
            cohort = factor(
              .$cohort,
              levels = c(
                "Matched",
                "EntirePopulation",
                "Treatment",
                "Comparator"
              )
            )
          ) %>%
          dplyr::select(
            AUROC,
            cohort
          ) %>%
          dplyr::mutate(
            AUROC = paste(
              "AUC:",
              paste0(
                round(
                  100*AUROC,
                  digits = 2
                ),
                "%"
              )
            )
          )

        plot <- plot +
          ggplot2::facet_grid(
            ~cohort
          ) +
          ggplot2::geom_label(
            data = labels,
            x = .05,
            y = .05,
            hjust = "left",
            vjust = "top",
            alpha = 0.8,
            ggplot2::aes(
              label = AUROC
            ),
            size = 5.5
          )

        return(plot)

      }
    )

    output$overallNegativeControlsPlot <- shiny::renderPlot(
      {
        dat <- overallResultSubset() %>%
          dplyr::mutate(logRr = log(estimate))
        ncs <- overallNegativeControlsSubset()
        null <- EmpiricalCalibration::fitNull(
          logRr = ncs$logRr,
          seLogRr = ncs$seLogRr
        )

        EmpiricalCalibration::plotCalibrationEffect(
          logRrNegatives = ncs$logRr,
          seLogRrNegatives = ncs$seLogRr,
          logRrPositives = dat$logRr,
          seLogRrPositives = dat$seLogRr,
          null = null
        )
      }
    )

    output$negativeControlsPlot <- shiny::renderPlot(
      {
        dat <- resultSubset()
        relative <- dat$relative %>%
          dplyr::mutate(logRr = log(estimate))
        ncs <- negativeControlsSubset()
        plotRiskStratifiedNegativeControls(
          negativeControls = ncs,
          positiveControls = relative
        )
      }
    )


    observeEvent(
      input$testInfo,
      {
        targetHtmlFile <- file.path(
          pathToHtml,
          paste(
            input$database,
            "html",
            sep = "."
          )
        )
        if (!file.exists(targetHtmlFile)) {
          shinyalert::shinyalert(
            title = "Database description",
            text = "Not available",
            size = "l",
          closeOnClickOutside = TRUE,
          confirmButtonCol = "#3B5866"
          )
        } else {
          shinyalert::shinyalert(
            title = "Database description",
            shiny::includeHTML(targetHtmlFile),
            type = "",
            html = TRUE,
            size = "l",
            closeOnClickOutside = TRUE,
            confirmButtonCol = "#3B5866"
          )
        }
      }
    )
  }
)
