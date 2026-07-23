#' Run the CPI Shiny Application
#'
#' Launches the CPI data processing Shiny application.
#'
#' @return This function launches a Shiny application.
#' @export

run_cpi_app <- function() {
  
  app_dir <- system.file(
    "shiny",
    package = "cpiShiny"
  )
  
  shiny::runApp(
    appDir = app_dir
  )
}