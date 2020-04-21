#' @export

runShiny <- function(analysisSettings){

 appDir <- system.file(
   "shiny",
   package = "RiskStratifiedEstimation"
 )
 shinySettings <- list(analysisSettings = analysisSettings)
 .GlobalEnv$shinySettings <- shinySettings
 on.exit(
   rm(
     shinySettings,
     env = .GlobalEnv
   )
 )

 shiny::runApp(appDir)

}
