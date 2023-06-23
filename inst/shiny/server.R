library(dplyr)

shiny::shinyServer(
  function(
    input,
    output,
    session
  ) {

    shiny::observe(
      {
        estOutcomes <- input$estOutcome
        shiny::updateSelectInput(
          inputId = "estOutcomeEstimation",
          choices = estOutcomes,
          selected = estOutcomes[1]
        )
      }
    )

    currentAnalysis <- shiny::reactive(
      {
        print("PAOK")
        print(input$database)
        print(input$treatment)
        print(input$comparator)
        result <- analyses %>%
          getAnalysis(
            database = input$database,
            treatmentCohort = input$treatment,
            comparatorCohort = input$comparator
          )
        print(result)
        return(result)
      }
    )

    currentRunLabel <- shiny::reactive(
      {
        analysis <- currentAnalysis()
        # message(paste("analysis", analysis))
        # message(paste("stratificationOutcome", input$stratOutcome))
        # message(paste("riskStratMethod", input$riskStratificationMethod))
        # message(paste("psAdjustmentMethod", input$psAdjsutmentMethod))
        result <- analyses %>%
          getRunLabel(
            analysisId = analysis,
            stratificationOutcome = input$stratOutcome,
            riskStratificationMethod = input$riskStratificationMethod,
            psAdjsutmentMethod = input$psAdjsutmentMethod
          )

        return(result)
      }
    )


    shiny::observe(
      {
        newChoicesStratOutcome <- analyses %>%
          dplyr::filter(
            analysisId == currentAnalysis(),
          ) %>%
          dplyr::distinct(stratificationOutcome) %>%
          dplyr::pull()

        shiny::updateSelectInput(
          inputId = "stratOutcome",
          choices = newChoicesStratOutcome
        )
      }
    )

    shiny::observe(
      {
        estOutcomes <- analyses %>%
          dplyr::filter(
            analysisId == currentAnalysis(),
            runLabel == currentRunLabel()
          ) %>%
          dplyr::pull(estimationOutcomes)
        newChoices <- getOutcomeNames(estOutcomes, mapOutcomes)
        shiny::updateSelectInput(
          inputId = "estOutcome",
          choices = newChoices,
          selected = stratIsEst(newChoices, input$stratOutcome)
        )
      }
    )

    shiny::observe(
      {
        newChoices <- analyses %>%
          dplyr::filter(
            analysisId == currentAnalysis(),
            runLabel == currentRunLabel()
          ) %>%
          dplyr::pull(riskStratificationMethodLabel)
        shiny::updateSelectInput(
          inputId = "riskStratificationMethod",
          choices = newChoices,
          selected = newChoices[1]
        )
      }
    )

    shiny::observe(
      {
        newChoices <- analyses %>%
          dplyr::filter(
            analysisId == currentAnalysis(),
            runLabel == currentRunLabel()
          ) %>%
          dplyr::pull(psAdjustmentMethodLabel)
        shiny::updateSelectInput(
          inputId = "psAdjsutmentMethod",
          choices = newChoices,
          selected = newChoices[1]
        )
      }
    )

    currentOverallIncidence <- shiny::reactive(
      {
        result <- overallIncidence %>%
          dplyr::filter(
            analysisId == currentAnalysis(),
            runLabel == currentRunLabel()
          )
        return(result)
      }
    )

    output$overallIncidence <- DT::renderDataTable(
      {
        res <- currentOverallIncidence()

        treatment <- res$treatment[1]
        comparator <- res$comparator[1]
        outcome <- res$stratOutcome[1]

        table <- res %>%
          dplyr::select(
            stratOutcome,
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

    currentIncidence <- shiny::reactive(
      {
        result <- incidence %>%
          dplyr::filter(
            analysisId == currentAnalysis(),
            runLabel == currentRunLabel(),
            estOutcome %in% input$estOutcome
          )
        return(result)
      }
    )
    output$mainTableIncidence <- DT::renderDataTable(
      {
        res <- currentIncidence()

        treatment <- res$treatment[1]
        comparator <- res$comparator[1]
        outcome <- res$stratOutcome[1]

        table <- res %>%
          dplyr::select(
            estOutcome,
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

    currentOverallResults <- shiny::reactive(
      {
        result <- overallMappedOverallRelativeResults %>%
          dplyr::filter(
            analysisId == currentAnalysis(),
            runLabel == currentRunLabel(),
            stratOutcome == input$stratOutcome
          )
        return(result)
      }
    )

    if (hasNegativeControls) {
      currentNegativeControls <- shiny::reactive(
        {
          result <- negativeControls %>%
            dplyr::filter(
              analysisId == currentAnalysis(),
              runLabel == currentRunLabel()
              # estOutcome %in% input$estOutcome
            )
          return(result)
        }
      )
    }

    if (hasOverallNegativeControls) {
      currentOverallNegativeControls <- shiny::reactive(
        {
          result <- overallNegativeControls %>%
            dplyr::filter(
              analysisId == currentAnalysis(),
              runLabel == currentRunLabel()
            )
          return(result)
        }
      )
    }

    output$overallResults <- DT::renderDataTable(
      {
        res <- currentOverallResults()

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
            treatment,
            comparator,
            stratOutcome,
            estimate,
            lower,
            upper
          )

        tableColNames <-  c(
          "Database",
          "Treatment",
          "Comparator",
          "Outcome",
          "Hazard ratio",
          "Lower",
          "Upper"
        )

        if (hasOverallNegativeControls) {
          ncs <- currentOverallNegativeControls() %>%
            dplyr::mutate(logRr = log(estimate))
          dat <- currentOverallResults() %>%
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

    currentOverallPsDensity <- shiny::reactive(
      {
        fileLocation <- constructFileName(
          prefix = "overall_psDensity",
          analysisPath = analysisPath,
          analysisId = currentAnalysis(),
          runLabel = currentRunLabel()
        )

        result <- readRDS(fileLocation)
        return(result)
      }
    )
    output$overallEvaluationPlotPs <- shiny::renderPlot(
      {
        currentOverallPsDensity() %>%
          dplyr::left_join(
            mapExposures,
            by = c("treatment" = "exposure_id")
          ) %>%
          plotPsDensity()
      }
    )

    currentOverallBalance <- shiny::reactive(
      {
        fileLocation <- constructFileName(
          prefix = "overall_balance",
          analysisPath = analysisPath,
          analysisId = currentAnalysis(),
          runLabel = currentRunLabel()
        )

        result <- readRDS(fileLocation)
        return(result)
      }
    )
    output$overallEvaluationPlotBalance <- shiny::renderPlot(
      {
        currentOverallBalance() %>%
          CohortMethod::plotCovariateBalanceScatterPlot(
            beforeLabel = "Before stratification",
            afterLabel = "After stratification"
          )
      }
    )
    output$overallBalanceTable <- DT::renderDT(
      {
        res <- currentOverallBalance() %>%
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

    output$overallNegativeControlsPlot <- shiny::renderPlot(
      {
        dat <- currentOverallResults() %>%
          dplyr::mutate(logRr = log(estimate))
        ncs <- currentOverallNegativeControls()
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

    currentRelativeResults <- shiny::reactive(
      {
        result <- mappedOverallRelativeResults %>%
          dplyr::filter(
            analysisId == currentAnalysis(),
            runLabel == currentRunLabel(),
            estOutcome %in% input$estOutcome
          )
        return(result)
      }
    )

    output$mainTableRelative <- DT::renderDataTable(
      {

        res <- currentRelativeResults()
        treatment <- res$treatment[1]
        comparator <- res$comparator[1]
        outcome <- res$stratOutcome[1]

        table <- res %>%
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
            combined
          ) %>%
          tidyr::spread(riskStratum, combined)

        if (hasNegativeControls) {
          ncs <- currentNegativeControls() %>%
            dplyr::mutate(logRr = log(estimate))
          tmp <- calibrateRiskStrataCis(
            negativeControls = ncs,
            positiveControls = res %>%
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
              )
            ) %>%
            dplyr::select(
              Outcome,
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

    currentAbsoluteResults <- shiny::reactive(
      {
        result <- mappedOverallAbsoluteResults %>%
          dplyr::filter(
            analysisId == currentAnalysis(),
            runLabel == currentRunLabel(),
            estOutcome %in% input$estOutcome
          )
        return(result)
      }
    )

    output$mainTableAbsolute <- DT::renderDataTable(
      {
        res <- currentAbsoluteResults()

        treatment <- res$treatment[1]
        comparator <- res$comparator[1]
        outcome <- res$stratOutcome[1]

        table <-
          res %>%
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

    currentCases <- shiny::reactive(
      {
        result <- mappedOverallCasesResults %>%
          dplyr::filter(
            analysisId == currentAnalysis(),
            runLabel == currentRunLabel(),
            estOutcome  %in% input$estOutcome
          )
        return(result)
      }
    )

    output$combinedPlot <- plotly::renderPlotly(
      {
        plot <-
          combinedPlot(
            cases = currentCases(),
            relative = currentRelativeResults(),
            absolute = currentAbsoluteResults(),
            treatment = input$treatment,
            comparator = input$comparator
          )
        return(plot)
      }
    )
    output$negativeControlsPlot <- shiny::renderPlot(
      {
        relative <- currentRelativeResults() %>%
          dplyr::mutate(logRr = log(estimate))
        ncs <- currentNegativeControls()
        plotRiskStratifiedNegativeControls(
          negativeControls = ncs,
          positiveControls = relative
        )
      }
    )

    currentPsDensity <- shiny::reactive(
      {
        fileLocation <- constructFileName(
          prefix = "psDensity",
          analysisPath = analysisPath,
          analysisId = currentAnalysis(),
          runLabel = currentRunLabel(),
          estOutcome = input$estOutcomeEstimation,
          mapOutcomes = mapOutcomes
        )

        if (file.exists(fileLocation)) {
          result <- readRDS(fileLocation)
        } else {
          result <- NULL
        }
        return(result)
      }
    )

    output$evaluationPlotPs <- shiny::renderPlot(
      {
        currentPsDensity() %>%
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

    currentCovariateBalance <- shiny::reactive(
      {
        fileLocation <- constructFileName(
          prefix = "balance",
          analysisPath = analysisPath,
          analysisId = currentAnalysis(),
          runLabel = currentRunLabel(),
          estOutcome = input$estOutcomeEstimation,
          mapOutcomes = mapOutcomes
        )

        if (file.exists(fileLocation)) {
          result <- readRDS(fileLocation)
        } else {
          result <- NULL
        }
        return(result)
      }
    )

    output$evaluationPlotBalance <- shiny::renderPlot(
      {
        currentCovariateBalance() %>%
          CohortMethod::plotCovariateBalanceScatterPlot(
            beforeLabel = "Before stratification",
            afterLabel = "After stratification"
          ) +
          ggplot2::facet_grid(
            ~riskStratum
          )
      }
    )
  }
)
