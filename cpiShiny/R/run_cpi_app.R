#' Run the CPI Shiny Application
#'
#' Launches the CPI data processing Shiny application.
#'
#' @return This function launches a Shiny application.
#' @export

run_cpi_app <- function() {
  
  app_dir <- system.file("app", package = "cpiShiny")
  if (app_dir == ""){
    stop("App directory not found")
  }
  shiny::runApp(appDir, launch.browser = TRUE)
}
