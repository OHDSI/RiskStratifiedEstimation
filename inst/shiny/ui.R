shiny::shinyUI(
  shinydashboard::dashboardPage(
    skin = "blue",
    shinydashboard::dashboardHeader(
      title = "RiskStratifiedEstimation"
    ),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = "menu1",
        shinydashboard::menuItem(
          tabName = "analysis",
          text = "Analysis",
          icon = icon("cogs"),
          selected = TRUE
        )
      ),
      shinydashboard::sidebarMenu(
        shiny::tags$head(
          shiny::tags$style(
            shiny::HTML(
              '#estOutcomeEstimation+ div>.selectize-dropdown{bottom: 100% !important; top:auto!important;}'
            )
          )
        ),
        shiny::tags$head(
          shiny::tags$style(
            shiny::HTML(
              '#predictionPopulation+ div>.selectize-dropdown{bottom: 100% !important; top:auto!important;}'
            )
          )
        ),
        id = "menu2",
        shinydashboard::menuItem(
          tabName = "estimation",
          text = "Estimation",
          icon = icon("chart-bar")
        ),
        shinydashboard::menuItem(
          tabName = "prediction",
          text = "Prediction",
          icon = icon("dice-six")
        ),
        shiny::selectInput(
          "treatment",
          "Treatment",
          unique(analyses$treatment),
          selected = unique(analyses$treatment)[1]
        ),
        shiny::selectInput(
          "comparator",
          "Comparator",
          unique(analyses$comparator),
          selected = unique(analyses$comparator)[1]
        ),
        shiny::selectInput(
          "stratOutcome",
          "Stratification Outcome",
          stratOptions,
          stratOptions[1]
        ),
        shiny::selectInput(
          "estOutcome",
          "Estimation outcome (max: 5)",
          unique(mapOutcomes$outcome_name),
          selected = "",
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
        shiny::radioButtons(
          "analysis",
          "Analysis",
          unique(analysisTypeOptions),
          unique(analysisTypeOptions)[1]
        ),
        shiny::conditionalPanel(
          condition = "input.menu2 == 'estimation'",
          shiny::selectInput(
            "estOutcomeEstimation",
            "Evaluate outcome",
            choices = unique(mapOutcomes$outcome_name),
            selected = unique(mapOutcomes$outcome_name)[1]
          )
        ),
        shiny::conditionalPanel(
          condition = "input.menu2 == 'prediction'",
          shiny::selectInput(
            "predictionPopulation",
            "Cohort",
            c(
              "Comparator",
              "EntirePopulation",
              "Matched",
              "Treatment"
            ),
            selected = "EntirePopulation",
            selectize = TRUE,
            multiple = TRUE
          )
        )
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "analysis",
          shiny::tabsetPanel(
            id = "relativePanel",
            shiny::tabPanel(
              "Overall",
              shiny::tabsetPanel(
                id = "overallPanel",
                shiny::tabPanel(
                  "Results",
                  DT::dataTableOutput(
                    "overallResults"
                  )
                ),
                shiny::tabPanel(
                  "Incidence",
                  DT::dataTableOutput(
                    "overallIncidence"
                  )
                ),
                shiny::tabPanel(
                  "Propensity scores",
                  shiny::plotOutput(
                    "overallEvaluationPlotPs",
                    height = "600px"
                  )
                ),
                shiny::tabPanel(
                  "Covariate balance",
                  shiny::fluidRow(
                    shinydashboard::box(
                      status = "info",
                      title = "Covariate balance plot",
                      width = NULL,
                      collapsible = TRUE,
                      shiny::plotOutput(
                        "overallEvaluationPlotBalance",
                        height = "600px"
                      )
                    )
                  ),
                  shiny::fluidRow(
                    shinydashboard::box(
                      status = "info",
                      title = "Covariate balance table",
                      width = NULL,
                      collapsible = TRUE,
                      DT::dataTableOutput("overallBalanceTable")
                    )
                  )
                ),
                shiny::tabPanel(
                  "Systematic error",
                  shiny::plotOutput(
                    "overallNegativeControlsPlot",
                    height = "600px"
                  )
                )
              )
            ),
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
              plotly::plotlyOutput(
                "combinedPlot",
                height = "600px"
              )
            ),
            shiny::tabPanel(
              "Systematic Error",
              shiny::plotOutput(
                "negativeControlsPlot",
                height = "600px"
              )
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "estimation",
          shiny::tabsetPanel(
            id = "estimationTabset",
            shiny::tabPanel(
              "Propensity scores",
              shiny::plotOutput(
                "evaluationPlotPs",
                height = "600px"
              )
            ),
            shiny::tabPanel(
              "Covariate balance",
              shiny::fluidRow(
                shinydashboard::box(
                  status = "info",
                  title = "Covariate balance plot",
                  width = NULL,
                  collapsible = TRUE,
                  shiny::plotOutput(
                    "evaluationPlotBalance",
                    height = "600px"
                  )
                )
              ),
              shiny::fluidRow(
                shinydashboard::box(
                  status = "info",
                  title = "Covariate balance table",
                  width = NULL,
                  collapsible = TRUE,
                  DT::dataTableOutput("balanceTable")
                )
              ),
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "prediction",
          shiny::fluidRow(
            shinydashboard::box(
              status = "info",
              title = "Calibration",
              width = NULL,
              collapsible = TRUE,
              shiny::plotOutput(
                "calibrationPlot"
              )
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              status = "info",
              title = "Discrimination",
              width = NULL,
              collapsible = TRUE,
              shiny::plotOutput(
                "discriminationPlot"
              )
            )
          )
        )
      )
    )
  )
)
