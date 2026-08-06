#' Launch IMTS SDMX Shiny App
#'
#' Launches the IMTS Excel to SDMX Shiny application.
#'
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "IMTS2SDMX")
  if (app_dir == "") {
    stop("App directory not found")
  }

  shiny::runApp(app_dir, launch.browser = TRUE)
}
