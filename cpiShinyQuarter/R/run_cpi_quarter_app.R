#' Run the CPI Shiny Application
#'
#' Launches the CPI data processing Shiny application.
#'
#' @return This function launches a Shiny application.
#' @export

run_cpi_quarter_app <- function() {
  
  app_dir <- system.file("shiny", package = "cpiShinyQuarter")
  if (app_dir == ""){
    stop("App directory not found")
  }
  shiny::runApp(app_dir, launch.browser = TRUE)
}
