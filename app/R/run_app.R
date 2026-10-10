#' Launch ThermogramForge
#'
#' Phase 0 stub. The real UI arrives in Phase 2.
#' @param ... Passed to [shiny::runApp()].
#' @export
run_app <- function(...) {
  shiny::runApp(app(), ...)
}

#' The ThermogramForge Shiny app object
#'
#' This is the single object every distribution target consumes.
#' @export
app <- function() {
  shiny::shinyApp(
    ui = bslib::page_fluid(shiny::h1("ThermogramForge v2 (stub)")),
    server = function(input, output, session) NULL
  )
}
