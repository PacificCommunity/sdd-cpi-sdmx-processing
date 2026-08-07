#' Run the CPI Shiny Application
#'
#' Launches the CPI data processing Shiny application.
#'
#' @return This function launches a Shiny application.
#' @export

run_cprice_app <- function() {
  
  app_dir <- system.file("shiny", package = "cpriceShiny")
  if (app_dir == ""){
    stop("App directory not found")
  }
  shiny::runApp(app_dir, launch.browser = TRUE)
}
