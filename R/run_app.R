#' Run the ThermogramForge Shiny Application
#'
#' Launches the ThermogramForge interactive web application for thermal
#' liquid biopsy thermogram analysis.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#'
#' @return This function does not return a value; it launches a Shiny
#'   application in the user's default web browser.
#'
#' @details
#' ThermogramForge provides an interactive interface for:
#' \itemize{
#'   \item Uploading and validating thermogram data (CSV/Excel)
#'   \item Automated baseline detection using ThermogramBaseline
#'   \item Signal quality assessment
#'   \item Interactive endpoint review and adjustment
#'   \item Comprehensive metric calculation via tlbparam
#'   \item Report generation and export
#' }
#'
#' @examples
#' \dontrun{
#' # Launch the application
#' run_app()
#'
#' # Launch on a specific port
#' run_app(port = 3838)
#' }
#'
#' @seealso
#' \itemize{
#'   \item \href{https://github.com/BuscagliaR/ThermogramBaseline}{ThermogramBaseline} for baseline detection
#'   \item \href{https://github.com/BuscagliaR/tlbparam}{tlbparam} for metric calculations
#' }
#'
#' @export
#' @importFrom shiny runApp
run_app <- function(...) {
  
  # Locate the Shiny app directory within the installed package
  
  app_dir <- system.file("shiny-app", package = "ThermogramForge")
  
  # Validate that the app directory exists
  if (app_dir == "") {
    stop(
      "Could not find Shiny app directory. ",
      "Please ensure ThermogramForge is properly installed.",
      call. = FALSE
    )
  }
  
  # Display startup message
  message(
    "Starting ThermogramForge v",
    utils::packageVersion("ThermogramForge"),
    "..."
  )
  
  # Launch the application
  shiny::runApp(app_dir, ...)
}