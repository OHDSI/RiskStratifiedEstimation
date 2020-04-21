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
            unique(mapTreatments$label),
            selected = unique(mapTreatments$label)[1]
          ),
          shiny::selectInput(
            "comparator",
            "Comparator",
            unique(mapTreatments$label),
            selected = unique(mapTreatments$label)[2]
          ),
          shiny::selectInput(
            "stratOutcome",
            "Stratification Outcome",
            unique(predictionOutcomes),
            unique(predictionOutcomes)[1]
          ),
          shiny::selectInput(
            "estOutcome",
            "Estimation outcome (max: 6)",
            unique(mapOutcomes$label),
            selected = unique(predictionOutcomes)[1],
            multiple = TRUE,
            selectize = TRUE
          ),
          shiny::checkboxGroupInput(
            "database",
            "Database",
            unique(mappedOverallAbsoluteResults$database),
            unique(mappedOverallAbsoluteResults$database)[1]
          ),
          shiny::checkboxGroupInput(
            "analysis",
            "Analysis",
            unique(mappedOverallAbsoluteResults$analysis),
            unique(mappedOverallAbsoluteResults$analysis)[1]
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
            unique(mapOutcomes$label),
            selected = unique(mapOutcomes$label)[1]
          )
        ),
        shinydashboard::menuItem(
          tabName = "prediction",
          "Prediction",
          icon = icon("dice-six"),
          shiny::selectInput(
            "cohort",
            "Cohort",
            c("Comparator",
              "Entire population",
              "Matched",
              "Treatment"),
            selected = "Entire population"
          )
        )

      )
    ),
    shinydashboard::dashboardBody(
      shiny::tabsetPanel(
        id = "relativePanel",
        shiny::tabPanel(
          "Relative",
          DT::dataTableOutput("mainTableRelative")
        ),
        shiny::tabPanel(
          "Absolute",
          DT::dataTableOutput("mainTableAbsolute")
        ),
        shiny::tabPanel(
          "Plot",
          shiny::plotOutput("combinedPlot",
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
