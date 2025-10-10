#' Launch ThermogramForge Shiny Application
#'
#' @description
#' Launches the ThermogramForge interactive web application for thermal liquid
#' biopsy (TLB) thermogram analysis.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#'   Common options include:
#'   \itemize{
#'     \item \code{port}: Port number (default: auto-selected)
#'     \item \code{host}: Host IP address (default: "127.0.0.1")
#'     \item \code{launch.browser}: Whether to launch browser (default: TRUE)
#'   }
#'
#' @return Runs the Shiny application (no return value)
#'
#' @examples
#' \dontrun{
#' # Launch with default settings
#' run_app()
#'
#' # Launch on specific port
#' run_app(port = 3838)
#'
#' # Launch without opening browser
#' run_app(launch.browser = FALSE)
#' }
#'
#' @export
run_app <- function(...) {
  # Get the path to the Shiny app directory
  app_dir <- system.file("shiny-app", package = "ThermogramForge")
  
  # Check if app directory exists
  if (app_dir == "") {
    stop(
      "Could not find Shiny app directory. ",
      "Try re-installing ThermogramForge.",
      call. = FALSE
    )
  }
  
  # Source the app.R file
  app_path <- file.path(app_dir, "app.R")
  
  if (!file.exists(app_path)) {
    stop(
      "Could not find app.R file. ",
      "Try re-installing ThermogramForge.",
      call. = FALSE
    )
  }
  
  # Print startup message
  message("Starting ThermogramForge v", utils::packageVersion("ThermogramForge"))
  message("Access the app in your browser at the URL shown below")
  
  # Run the application
  shiny::runApp(app_dir, ...)
}