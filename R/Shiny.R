#' Creates the server function of the shiny application.
#'
#' Creates the server function of the shiny application developed for the assessment of the final result
#'
#' @param analysisSettings           An R object of type \code{analysisSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createAnalysisSettings}}.
#'
#'@return                            The ui.R script
#'
#'@export

createServer <- function(analysisSettings){
  formatR::tidy_source(
    text = "library(dplyr)
shiny::shinyServer(function(input, output, session) {

    shiny::observe({
    stratificationOutcome <- input$stratOutcome
    filteredEstimationOutcomes <- readRDS(\"data/mappedOverallRelativeResults.rds\") %>%
    dplyr::filter(stratOutcome == stratificationOutcome) %>% dplyr::select(estOutcome)


    shiny::updateSelectInput(session = session, inputId = \"estOutcome\", choices = unique(filteredEstimationOutcomes))
    })

    resultSubset <- shiny::reactive({

    results <- getResults(treat = input$treatment, comp = input$comparator,
    strat = input$stratOutcome, est = input$estOutcome, anal = input$analysis,
    db = input$database)

    return(results)

    })

    output$mainTableRelative <- DT::renderDataTable({

    res <- resultSubset()

    table <- res$relative %>% dplyr::select(database, stratOutcome, estOutcome,
    riskStratum, estimate, lower, upper) %>% DT::datatable() %>% DT::formatRound(columns = c(\"estimate\",
    \"lower\", \"upper\"), digits = 2)

    return(table)

    })

    output$mainTableAbsolute <- DT::renderDataTable({

    res <- resultSubset()

    table <- res$absolute %>% dplyr::select(database, stratOutcome, estOutcome,
    riskStratum, estimate, lower, upper) %>% dplyr::mutate(estimate = 100 *
    estimate, lower = 100 * lower, upper = 100 * upper) %>% DT::datatable() %>%
    DT::formatRound(columns = c(\"estimate\", \"lower\", \"upper\"), digits = 2)

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
})",
    output = TRUE,
    file = file.path(analysisSettings$saveDirectory,
                     analysisSettings$analysisId,
                     "shiny",
                     "server.R")
  )
}



#' Creates the ui of the shiny application.
#'
#' Creates the ui of the shiny application developed for the assessment of the final result
#'
#' @param analysisSettings           An R object of type \code{analysisSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createAnalysisSettings}}.
#'
#'@return                            The ui.R script
#'
#'@export

createUI <- function(analysisSettings){
  formatR::tidy_source(
    text =
      "shiny::shinyUI(
  shinydashboard::dashboardPage(
    skin = \"black\",
    shinydashboard::dashboardHeader(
      title = \"RiskStratifiedEstimation\"
    ),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shiny::selectInput(\"treatment\",
                           \"Treatment\",
                           unique(mapTreatments$label),
                           selected = unique(mapTreatments$label)[1]),
        shiny::selectInput(\"comparator\",
                           \"Comparator\",
                           unique(mapTreatments$label),
                           selected = unique(mapTreatments$label)[2]),
        shiny::selectInput(\"stratOutcome\",
                           \"Stratification Outcome\",
                           unique(mapOutcomes$label),
                           \"Total cardiovascular disease\"),
        shiny::selectInput(\"estOutcome\",
                           \"Estimation outcome (max: 6)\",
                           unique(mapOutcomes$label),
                           selected = unique(mapOutcomes$label)[1],
                           multiple = TRUE,
                           selectize = TRUE),
        shiny::checkboxGroupInput(\"database\",
                                  \"Database\",
                                  unique(mappedOverallAbsoluteResults$database),
                                  unique(mappedOverallAbsoluteResults$database)[1]),
        shiny::checkboxGroupInput(\"analysis\",
                                  \"Analysis\",
                                  unique(mappedOverallAbsoluteResults$analysis),
                                  \"stratifyByPs\")
      )
    ),
    shinydashboard::dashboardBody(
      shiny::tabsetPanel(id = \"relativePanel\",
                         shiny::tabPanel(\"Relative\",
                                         DT::dataTableOutput(\"mainTableRelative\")),
                         shiny::tabPanel(\"Absolute\",
                                         DT::dataTableOutput(\"mainTableAbsolute\")),
                         shiny::tabPanel(\"Plot\",
                                         shiny::plotOutput(\"combinedPlot\", height = \"600px\")))
    )
  )
)",
    output = TRUE,
    file = file.path(analysisSettings$saveDirectory,
                     analysisSettings$analysisId,
                     "shiny",
                     "ui.R")
  )
}





#' Creates the global function of the shiny application.
#'
#' Creates the global function of the shiny application developed for the assessment of the final result
#'
#' @param analysisSettings           An R object of type \code{analysisSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createAnalysisSettings}}.
#'
#'@return                            The ui.R script
#'
#'@export

createGlobal <- function(analysisSettings){

  formatR::tidy_source(
    text =
      "library(dplyr)

    mapOutcomes <- readRDS(\"./data/mapOutcomes.rds\")
    mapTreatments <- readRDS(\"./data/mapTreatments.rds\")
    mappedOverallAbsoluteResults <- readRDS(\"./data/mappedOverallAbsoluteResults.rds\")
    mappedOverallRelativeResults <- readRDS(\"./data/mappedOverallRelativeResults.rds\")
    mappedOverallCasesResults <- readRDS(\"./data/mappedOverallCasesResults.rds\")

    getResults <- function(treat, comp, strat, est, db, anal) {

    res <- list()

    res$relative <- readRDS(\"./data/mappedOverallRelativeResults.rds\") %>%
    filter(stratOutcome == strat & estOutcome %in% est & analysis == anal & treatment == treat & comparator == comp & database == db)
    res$absolute <- readRDS(\"./data/mappedOverallAbsoluteResults.rds\") %>%
    filter(stratOutcome == strat & estOutcome %in% est & analysis == anal & treatment == treat & comparator == comp & database == db)
    res$cases <- readRDS(\"./data/mappedOverallCasesResults.rds\") %>%
    filter(stratOutcome == strat & estOutcome %in% est & analysis == anal & treatment == treat & comparator == comp & database == db)

    return(res)


    }



    combinedPlot <- function(cases, relative, absolute, treatment, comparator) {

    cases <- reshape::melt(cases, id.vars = c(\"riskStratum\", \"database\", \"estOutcome\"),
    measure.vars = c(\"casesComparator\", \"casesTreatment\")) %>%
    mutate(variable = ifelse(variable == \"casesComparator\", comparator, treatment))


    cases$test <- file.path(cases$database, cases$estOutcome, cases$variable)

    casesPlot <- ggplot2::ggplot(data = cases, ggplot2::aes(x = riskStratum, y = value * 100)) +
    ggplot2::geom_bar(stat = \"identity\", position = ggplot2::position_dodge(), ggplot2::aes(fill = test), width = 0.5) +
    ggplot2::xlab(\"Risk Stratum\") +
    ggplot2::ylab(\"Outcome Rate (%)\") +
    ggplot2::geom_hline(yintercept = 0, size = 0.8) +
    ggplot2::scale_fill_brewer(palette = \"Paired\") + ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(), legend.direction = \"horizontal\",
    legend.position = \"top\") + ggplot2::scale_y_reverse()

    relative$test <- file.path(relative$database, relative$estOutcome)

    rrrPlot <- ggplot2::ggplot(relative, ggplot2::aes(x = riskStratum, y = estimate,
    group = test, color = test)) +
    ggplot2::geom_point(size = 2.5, position = ggplot2::position_dodge(w = 0.3)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper), width = 0,
    position = ggplot2::position_dodge(w = 0.3)) +
    ggplot2::geom_hline(yintercept = 1, linetype = \"dashed\", size = 0.8) +
    ggplot2::xlab(\"Risk Stratum\") +
    ggplot2::ylab(\"Hazard Ratio\") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c(\"#0099FF\", \"#009933\", \"#CC0000\", \"#FF9933\", \"#663399\", \"#CC9966\")) +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = \"none\", axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank())

  absolute$test <- paste(absolute$database, absolute$estOutcome, sep = \"/\")

  arrPlot <- ggplot2::ggplot(absolute, ggplot2::aes(x = riskStratum, y = estimate *
                                                      100, group = test, color = test)) +
    ggplot2::geom_point(size = 2.5, position = ggplot2::position_dodge(w = 0.3)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower * 100, ymax = upper * 100),
                           width = 0, position = ggplot2::position_dodge(w = 0.3)) +
    ggplot2::geom_hline(yintercept = 0, linetype = \"dashed\", size = 0.8) +
    ggplot2::xlab(\"Risk stratum\") +
    ggplot2::ylab(\"Absolute Risk Reduction (%)\") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(values = c(\"#0099FF\", \"#009933\", \"#CC0000\", \"#FF9933\", \"#663399\", \"#CC9966\")) +
    ggplot2::theme(legend.direction = \"horizontal\", legend.position = \"bottom\", legend.title = ggplot2::element_blank())

  ggpubr::ggarrange(casesPlot, rrrPlot, arrPlot, nrow = 3, align = \"v\")
}
",
output = T,
file = file.path(analysisSettings$saveDirectory,
                 analysisSettings$analysisId,
                 "shiny",
                 "global.R"))
}




#' Launches the shiny application
#'
#' Launches the shin application for the assessment of the overall results
#'
#' @param analysisSettings           An R object of type \code{analysisSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createAnalysisSettings}}.
#'
#'@return                            The shiny application

#' @export
runShiny <- function(analysisSettings){

  createUI(analysisSettings)
  createServer(analysisSettings)
  createGlobal(analysisSettings)

  setwd(file.path(analysisSettings$saveDirectory,
                  analysisSettings$analysisId,
                  "shiny"))
  shiny::shinyAppDir(appDir = "./")

}






#' Combines the overall results
#'
#' @param analysisSettings           An R object of type \code{analysisSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createAnalysisSettings}}.
#' @param runSettings                An R object of type \code{runSettings} created using the function
#'                                   \code{\link[RiskStratifiedEstimation]{createRunSettings}}.
#'
#' @return                          Stores the overall results along with the required data to lauch the shiny
#'                                   application in the `shiny` directory

#' @importFrom dplyr %>%
#' @export

createOverallResults <- function(analysisSettings,
                                 runSettings){

  mapTreatments <- analysisSettings$mapTreatments
  mapOutcomes <- analysisSettings$mapOutcomes

  predictOutcomes <-
    analysisSettings$outcomeIds[which(colSums(analysisSettings$analysisMatrix) != 0)]

  pathToResults <- file.path(analysisSettings$saveDirectory,
                             analysisSettings$analysisId,
                             "Estimation")

  absolute <- data.frame(ARR = numeric(),
                         lower = numeric(),
                         upper = numeric(),
                         riskStratum = character(),
                         stratOutcome = numeric(),
                         estOutcome = numeric(),
                         database = character(),
                         analysis = character(),
                         treatment = numeric(),
                         comparator = numeric())
  relative <- data.frame(HR = numeric(),
                         lower = numeric(),
                         upper = numeric(),
                         riskStratum = character(),
                         stratOutcome = numeric(),
                         estOutcome = numeric(),
                         database = character(),
                         analysis = character(),
                         treatment = numeric(),
                         comparator = numeric())
  cases <- data.frame(riskStratum = character(),
                      stratOutcome = numeric(),
                      estOutcome = numeric(),
                      database = character(),
                      analysis = character(),
                      treatment = numeric(),
                      comparator = numeric())


  for(predictOutcome in predictOutcomes){
    absoluteResult <- readRDS(file.path(pathToResults,
                                        predictOutcome,
                                        "absoluteRiskReduction.rds"))
    absoluteResult <- data.frame(absoluteResult,
                                 stratOutcome = predictOutcome,
                                 estOutcome = predictOutcome,
                                 database = analysisSettings$databaseName,
                                 analysis = runSettings$runCmSettings$psMethod,
                                 treatment = analysisSettings$treatmentCohortId,
                                 comparator = analysisSettings$comparatorCohortId)
    absolute <- rbind(absolute, absoluteResult)

    relativeResult <- readRDS(file.path(pathToResults,
                                        predictOutcome,
                                        "relativeRiskReduction.rds"))
    relativeResult <- data.frame(relativeResult,
                                 stratOutcome = predictOutcome,
                                 estOutcome = predictOutcome,
                                 database = analysisSettings$databaseName,
                                 analysis = runSettings$runCmSettings$psMethod,
                                 treatment = analysisSettings$treatmentCohortId,
                                 comparator = analysisSettings$comparatorCohortId)
    relative <- rbind(relative, relativeResult)

    casesResult <- readRDS(file.path(pathToResults,
                                     predictOutcome,
                                     "cases.rds")) %>%
      dplyr::rename("casesComparator" = "comparator") %>%
      dplyr::rename("casesTreatment" = "treatment")
    casesResult <- data.frame(casesResult,
                              stratOutcome = predictOutcome,
                              estOutcome = predictOutcome,
                              database = analysisSettings$databaseName,
                              analysis = runSettings$runCmSettings$psMethod,
                              treatment = analysisSettings$treatmentCohortId,
                              comparator = analysisSettings$comparatorCohortId)
    cases <- rbind(cases, casesResult)

    predLoc <- which(analysisSettings$outcomeIds == predictOutcome)
    compLoc <- analysisSettings$analysisMatrix[, predLoc]
    compareOutcomes <- analysisSettings$outcomeIds[as.logical(compLoc)]
    compareOutcomes <- compareOutcomes[compareOutcomes != predictOutcome]

    if(length(compareOutcomes) != 0){
      for(compareOutcome in compareOutcomes){
        absoluteResult <- readRDS(file.path(pathToResults,
                                            predictOutcome,
                                            compareOutcome,
                                            "absoluteRiskReduction.rds"))
        absoluteResult <- data.frame(absoluteResult,
                                     stratOutcome = predictOutcome,
                                     estOutcome = compareOutcome,
                                     database = analysisSettings$databaseName,
                                     analysis = runSettings$runCmSettings$psMethod,
                                     treatment = analysisSettings$treatmentCohortId,
                                     comparator = analysisSettings$comparatorCohortId)
        absolute <- rbind(absolute, absoluteResult)

        relativeResult <- readRDS(file.path(pathToResults,
                                            predictOutcome,
                                            compareOutcome,
                                            "relativeRiskReduction.rds"))
        relativeResult <- data.frame(relativeResult,
                                     stratOutcome = predictOutcome,
                                     estOutcome = compareOutcome,
                                     database = analysisSettings$databaseName,
                                     analysis = runSettings$runCmSettings$psMethod,
                                     treatment = analysisSettings$treatmentCohortId,
                                     comparator = analysisSettings$comparatorCohortId)
        relative <- rbind(relative, relativeResult)

        casesResult <- readRDS(file.path(pathToResults,
                                         predictOutcome,
                                         compareOutcome,
                                         "cases.rds")) %>%
          dplyr::rename("casesComparator" = "comparator") %>%
          dplyr::rename("casesTreatment" = "treatment")
        casesResult <- data.frame(casesResult,
                                  stratOutcome = predictOutcome,
                                  estOutcome = compareOutcome,
                                  database = analysisSettings$databaseName,
                                  analysis = runSettings$runCmSettings$psMethod,
                                  treatment = analysisSettings$treatmentCohortId,
                                  comparator = analysisSettings$comparatorCohortId)
        cases <- rbind(cases, casesResult)
      }
    }

    saveDir <- file.path(analysisSettings$saveDirectory,
                         analysisSettings$analysisId,
                         "shiny",
                         "data")


    for(directory in c("Comparator", "Matched", "Treatment", "EntirePopulation")){

      outputDir <- file.path(saveDir, "Prediction", predictOutcome, directory)
      if(!dir.exists(outputDir)){
        dir.create(outputDir, recursive = T)
      }
      predictionEvaluationDir <- file.path(analysisSettings$saveDirectory,
                                           analysisSettings$analysisId,
                                           "Prediction",
                                           predictOutcome,
                                           analysisSettings$analysisId,
                                           directory)
      listFiles <- list.files(predictionEvaluationDir)
      file.copy(file.path(predictionEvaluationDir,
                          listFiles),
                outputDir)
    }
  }

  absolute %>%
    dplyr::left_join(mapTreatments, by = c( "treatment" = "idNumber")) %>%
    dplyr::select(-treatment) %>%
    dplyr::rename("treatment" = "label") %>%
    dplyr::left_join(mapTreatments, by = c( "comparator" = "idNumber")) %>%
    dplyr::select(-comparator) %>%
    dplyr::rename("comparator" = "label") %>%
    dplyr::left_join(mapOutcomes, by = c( "stratOutcome" = "idNumber")) %>%
    dplyr::select(-stratOutcome) %>%
    dplyr::rename("stratOutcome" = "label") %>%
    dplyr::left_join(mapOutcomes, by = c( "estOutcome" = "idNumber")) %>%
    dplyr::select(-estOutcome) %>%
    dplyr::rename("estOutcome" = "label") %>%
    dplyr::rename("estimate" = "ARR") %>%
    saveRDS(file.path(saveDir, "mappedOverallAbsoluteResults.rds"))

  relative %>%
    dplyr::left_join(mapTreatments, by = c( "treatment" = "idNumber")) %>%
    dplyr::select(-treatment) %>%
    dplyr::rename("treatment" = "label") %>%
    dplyr::left_join(mapTreatments, by = c( "comparator" = "idNumber")) %>%
    dplyr::select(-comparator) %>%
    dplyr::rename("comparator" = "label") %>%
    dplyr::left_join(mapOutcomes, by = c( "stratOutcome" = "idNumber")) %>%
    dplyr::select(-stratOutcome) %>%
    dplyr::rename("stratOutcome" = "label") %>%
    dplyr::left_join(mapOutcomes, by = c( "estOutcome" = "idNumber")) %>%
    dplyr::select(-estOutcome) %>%
    dplyr::rename("estOutcome" = "label") %>%
    dplyr::rename("estimate" = "HR") %>%
    saveRDS(file.path(saveDir, "mappedOverallRelativeResults.rds"))

  cases %>%
    dplyr::left_join(mapTreatments, by = c( "treatment" = "idNumber")) %>%
    dplyr::select(-treatment) %>%
    dplyr::rename("treatment" = "label") %>%
    dplyr::left_join(mapTreatments, by = c( "comparator" = "idNumber")) %>%
    dplyr::select(-comparator) %>%
    dplyr::rename("comparator" = "label") %>%
    dplyr::left_join(mapOutcomes, by = c( "stratOutcome" = "idNumber")) %>%
    dplyr::select(-stratOutcome) %>%
    dplyr::rename("stratOutcome" = "label") %>%
    dplyr::left_join(mapOutcomes, by = c( "estOutcome" = "idNumber")) %>%
    dplyr::select(-estOutcome) %>%
    dplyr::rename("estOutcome" = "label")%>%
    saveRDS(file.path(saveDir, "mappedOverallCasesResults.rds"))

  saveRDS(analysisSettings$mapOutcomes,
          file.path(saveDir,
                    "mapOutcomes.rds"))
  saveRDS(analysisSettings$mapTreatments,
          file.path(saveDir,
                    "mapTreatments.rds"))

  return(NULL)
}


