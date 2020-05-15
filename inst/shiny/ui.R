addInfo <- function(item, infoId) {
  infoTag <- tags$small(class = "badge pull-right action-button",
                        style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
                        type = "button",
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}

shiny::shinyUI(
  shinydashboard::dashboardPage(
    skin = "black",
    shinydashboard::dashboardHeader(
      title = "RiskStratifiedEstimation"
    ),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          tabName = "analysis",
          "Analysis",
          icon = icon("cogs"),
          shiny::selectInput(
            "treatment",
            "Treatment",
            unique(mapExposures$exposure_name),
            selected = unique(mapExposures$exposure_name)[1]
          ),
          shiny::selectInput(
            "comparator",
            "Comparator",
            unique(mapExposures$exposure_name),
            selected = unique(mapExposures$exposure_name)[2]
          ),
          shiny::selectInput(
            "stratOutcome",
            "Stratification Outcome",
            stratOptions,
            stratOptions[1]
          ),
          shiny::selectInput(
            "estOutcome",
            "Estimation outcome (max: 6)",
            unique(mapOutcomes$outcome_name),
            selected = unique(mapOutcomes$outcome_name)[1],
            multiple = TRUE,
            selectize = TRUE
          ),
          addInfo(
            item = shiny::selectizeInput(
              "database",
              "Database",
              unique(databaseOptions),
              unique(databaseOptions)[1]
            ),
            infoId = "testInfo"
          ),
          shiny::checkboxGroupInput(
            "analysis",
            "Analysis",
            unique(analysisTypeOptions),
            unique(analysisTypeOptions)[1]
          )
        ),
        shinydashboard::menuItem(
          tabName = "estimation",
          "Estimation",
          icon = icon("chart-bar"),
          shiny::selectInput(
            "evaluateEstimation",
            "Evaluate",
            c("Propensity scores",
              "Covariate balance"),
            selected = "Propensity scores"
          ),
          shiny::selectInput(
            "estOutcomeEstimation",
            "Estimation outcome",
            unique(mapOutcomes$outcome_name),
            selected = unique(mapOutcomes$outcome_name)[1]
          )
        ),
        shinydashboard::menuItem(
          tabName = "prediction",
          "Prediction",
          icon = icon("dice-six"),
          shiny::selectInput(
            "predictionPopulation",
            "Cohort",
            c("Comparator",
              "EntirePopulation",
              "Matched",
              "Treatment"),
            selected = "EntirePopulation"
          )
        )

      )
    ),
    shinydashboard::dashboardBody(
      shiny::tabsetPanel(
        id = "relativePanel",
        shiny::tabPanel(
          "Incidence",
          DT::dataTableOutput("mainTableIncidence")
        ),
        shiny::tabPanel(
          "Relative",
          DT::dataTableOutput("mainTableRelative")
        ),
        shiny::tabPanel(
          "Absolute",
          DT::dataTableOutput("mainTableAbsolute")
        ),
        shiny::tabPanel(
          "Risk stratified analysis",
          plotly::plotlyOutput("combinedPlot",
                               height = "600px")
        ),
        shiny::tabPanel(
          "Estimation evaluation",
          shiny::plotOutput(
            "evaluationPlot",
            height = "600px"
          )
        ),
        shiny::tabPanel(
          "Prediction evaluation",
          shiny::fluidRow(
            shinydashboard::box(
              status = "info",
              title = "Calibration",
              shiny::plotOutput(
                "calibrationPlot",
                height = "600px"
              )
            ),
            shinydashboard::box(
              status = "info",
              title = "Discrimination",
              shiny::plotOutput(
                "discriminationPlot",
                height = "600px"
              )
            )
          )
        )
      )
    )
  )
)
