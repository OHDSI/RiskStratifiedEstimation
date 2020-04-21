library(dplyr)
shiny::shinyServer(function(input, output, session) {

  shiny::observe({
    stratificationOutcome <- input$stratOutcome
    filteredEstimationOutcomes <- mappedOverallRelativeResults %>%
      dplyr::filter(stratOutcome == stratificationOutcome) %>%
      dplyr::select(estOutcome)


    shiny::updateSelectInput(session = session, inputId = "estOutcome", choices = unique(filteredEstimationOutcomes))
  })

  resultSubset <- shiny::reactive({

    results <- getResults(treat = input$treatment, comp = input$comparator, strat = input$stratOutcome,
                          est = input$estOutcome, anal = input$analysis, db = input$database)

    return(results)

  })

  output$mainTableRelative <- DT::renderDataTable({

    res <- resultSubset()

    table <- res$relative %>%
      dplyr::select(
        database,
        stratOutcome,
        estOutcome,
        riskStratum,
        estimate,
        lower,
        upper
      ) %>%
      DT::datatable() %>%
      DT::formatRound(
        columns = c("estimate","lower", "upper"),
        digits = 2
      )

    return(table)

  })

  output$mainTableAbsolute <- DT::renderDataTable({

    res <- resultSubset()

    table <- res$absolute %>%
      dplyr::select(database,
                    stratOutcome,
                    estOutcome,
                    riskStratum,
                    estimate,
                    lower,
                    upper) %>%
      dplyr::mutate(estimate = 100 * estimate, lower = 100 * lower, upper = 100 * upper) %>%
      DT::datatable() %>%
      DT::formatRound(columns = c("estimate", "lower", "upper"), digits = 2)

    return(table)

  })

  output$combinedPlot <- shiny::renderPlot({

    res <- resultSubset()

    plot <- combinedPlot(cases = res$cases, relative = res$relative, absolute = res$absolute,
                         treatment = input$treatment, comparator = input$comparator)
    return(plot)

  }, height = function() {
    0.45 * session$clientData$output_combinedPlot_width
  })

  shiny::observe({
    x <- input$estOutcome

    shiny::updateSelectInput(
      session = session,
      inputId = "estOutcomeEstimation",
      choices = x
    )
  })

  output$evaluationPlot <- shiny::renderPlot({
    stratIdNumber <- mapOutcomes %>%
      dplyr::filter(label == input$stratOutcome) %>%
      dplyr::select(idNumber) %>%
      unlist()
    estIdNumber <- mapOutcomes %>%
      dplyr::filter(label == input$estOutcomeEstimation) %>%
      dplyr::select(idNumber) %>%
      unlist()
    if(input$evaluateEstimation == "Propensity scores"){
      plotList <- readRDS(
        file.path(
          analysisDir,
          "data",
          "Estimation",
          stratIdNumber,
          paste0(
            paste(
              "psPlotList",
              estIdNumber,
              sep = "_"
            ),
            ".rds"
          )
        )
      )
    }
    else{
      plotList <- readRDS(
        file.path(
          analysisDir,
          "data",
          "Estimation",
          stratIdNumber,
          paste0(
            paste(
              "balancePlotList",
              estIdNumber,
              sep = "_"
            ),
            ".rds"
          )
        )
      )
    }
    plot <- ggpubr::ggarrange(
      plotlist = plotList
    )
    return(plot)
  })

  output$calibrationPlot <- shiny::renderPlot({
    stratIdNumber <- mapOutcomes %>%
      dplyr::filter(label == input$stratOutcome) %>%
      dplyr::select(idNumber) %>%
      unlist()
    if (input$cohort == "Comparator") {
      evaluation <- readRDS(
        file.path(
          analysisDir,
          "data",
          "Prediction",
          stratIdNumber,
          "Comparator",
          "performanceEvaluation.rds"
        )
      )
    } else if (input$cohort == "Treatment") {
      evaluation <- readRDS(
        file.path(
          analysisDir,
          "data",
          "Prediction",
          stratIdNumber,
          "Treatment",
          "performanceEvaluation.rds"
        )
      )
    } else if (input$cohort == "Matched") {
      evaluation <- readRDS(
        file.path(
          analysisDir,
          "data",
          "Prediction",
          stratIdNumber,
          "Matched",
          "performanceEvaluation.rds"
        )
      )
    } else {
      evaluation <- readRDS(
        file.path(
          analysisDir,
          "data",
          "Prediction",
          stratIdNumber,
          "Treatment",
          "performanceEvaluation.rds"
        )
      )
    }

    plot <- PatientLevelPrediction::plotSparseCalibration2(
      evaluation = evaluation,
      type = "validation"
    )
    return(plot)
  })

  output$discriminationPlot <- shiny::renderPlot({
    stratIdNumber <- mapOutcomes %>%
      dplyr::filter(label == input$stratOutcome) %>%
      dplyr::select(idNumber) %>%
      unlist()

    if (input$cohort == "Comparator") {
      prediction <- readRDS(
        file.path(
          analysisDir,
          "data",
          "Prediction",
          stratIdNumber,
          "Comparator",
          "prediction.rds"
        )
      )
    } else if (input$cohort == "Treatment") {
      prediction <- readRDS(
        file.path(
          analysisDir,
          "data",
          "Prediction",
          stratIdNumber,
          "Treatment",
          "prediction.rds"
        )
      )
    } else if (input$cohort == "Matched") {
      prediction <- readRDS(
        file.path(
          analysisDir,
          "data",
          "Prediction",
          stratIdNumber,
          "Matched",
          "prediction.rds"
        )
      )
    } else {
      prediction <- readRDS(
        file.path(
          analysisDir,
          "data",
          "Prediction",
          stratIdNumber,
          "Treatment",
          "prediction.rds"
        )
      )
    }

    plot <- PatientLevelPrediction::plotRoc(
      prediction = prediction
    )
    return(plot)

  })


})
