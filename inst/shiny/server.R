library(dplyr)
shiny::shinyServer(function(input, output, session) {

  shiny::observe({
    stratificationOutcome <- input$stratOutcome
    filteredEstimationOutcomes <- readRDS("data/mappedOverallRelativeResults.rds") %>%
      dplyr::filter(stratOutcome == stratificationOutcome) %>% dplyr::select(estOutcome)


    shiny::updateSelectInput(session = session, inputId = "estOutcome", choices = unique(filteredEstimationOutcomes))
  })

  resultSubset <- shiny::reactive({

    results <- getResults(treat = input$treatment, comp = input$comparator, strat = input$stratOutcome,
                          est = input$estOutcome, anal = input$analysis, db = input$database)

    return(results)

  })

  output$mainTableRelative <- DT::renderDataTable({

    res <- resultSubset()

    table <- res$relative %>% dplyr::select(database, stratOutcome, estOutcome, riskStratum,
                                            estimate, lower, upper) %>% DT::datatable() %>% DT::formatRound(columns = c("estimate",
                                                                                                                        "lower", "upper"), digits = 2)

    return(table)

  })

  output$mainTableAbsolute <- DT::renderDataTable({

    res <- resultSubset()

    table <- res$absolute %>% dplyr::select(database, stratOutcome, estOutcome, riskStratum,
                                            estimate, lower, upper) %>% dplyr::mutate(estimate = 100 * estimate, lower = 100 *
                                                                                        lower, upper = 100 * upper) %>% DT::datatable() %>% DT::formatRound(columns = c("estimate",
                                                                                                                                                                        "lower", "upper"), digits = 2)

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
})
