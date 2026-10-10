# Phase-0 shinylive spike: does thermogramforge.core run in webR, and how fast?
#
# Built by spike/build_site.R, which copies the Small.csv fixture into the
# app and the wasm package repo beside the exported site. The 500-sample
# file is uploaded by the user (too large to bundle).
#
# Nothing heavy happens at top level so the UI appears immediately. The core
# package is added to the shinylive package bundle by spike/build_site.R;
# runtime webr::install() from a side-car repo was tried first and hung
# silently, so it is not used.

library(shiny)
library(bslib)
options(shiny.maxRequestSize = 200 * 1024^2)   # Large500.csv is 26 MB, Large.csv 70 MB

# shinylive::export() scans this file for package names and tries to resolve
# them locally; `webr` and our own package exist only inside the browser, so
# their names are assembled at run time to stay out of the scan. `forecast`
# must stay visible so it is bundled.
if (FALSE) library(forecast)
core_pkg <- paste0("thermogramforge", ".core")
has_pkg <- function(p) requireNamespace(p, quietly = TRUE)
in_webr <- identical(R.version$platform, "wasm32-unknown-emscripten")

ui <- page_sidebar(
  title = "thermogramforge.core webR spike",
  sidebar = sidebar(
    width = 340,
    tags$p(tags$b("Runtime:"), if (in_webr) "webR (wasm)" else "native R"),
    tags$p(tags$b("Core:"), textOutput("core_status", inline = TRUE)),
    tags$pre(textOutput("core_log"), style = "font-size: 11px; white-space: pre-wrap;"),
    selectInput("ep", "Endpoint method",
                c("rolling_sd + kpss (default)" = "default",
                  "rolling_sd_v1 + auto.arima (reference)" = "v1")),
    actionButton("run20", "Run Small.csv (20 samples)", class = "btn-primary"),
    tags$hr(),
    fileInput("upload", "Upload Large500.csv (wide)", accept = ".csv"),
    actionButton("run_upload", "Run uploaded file"),
    tags$hr(),
    tags$p(tags$b("R:"), R.version.string),
    tags$p(tags$b("Platform:"), R.version$platform)
  ),
  card(card_header("Timing"), tableOutput("timing")),
  card(card_header("Per-sample"), tableOutput("per_sample")),
  card(card_header("Log"), verbatimTextOutput("log"))
)

server <- function(input, output, session) {
  log_lines <- reactiveVal(character())
  say <- function(...) log_lines(c(log_lines(), sprintf(...)))
  result <- reactiveVal(NULL)
  core_ready <- reactiveVal(FALSE)
  core_msgs <- reactiveVal(character())
  core_secs <- reactiveVal(NA_real_)

  # The core package is bundled into shinylive/webr/packages by
  # spike/build_site.R, so it is installed before the app starts; all that
  # is left is to attach it and report.
  observe({
    t0 <- proc.time()[["elapsed"]]
    ok <- has_pkg(core_pkg)
    msgs <- if (ok) {
      tryCatch({ library(core_pkg, character.only = TRUE); "library() ok" },
               error = function(e) { ok <<- FALSE; paste("library() failed:", conditionMessage(e)) })
    } else {
      c("package not available in .libPaths():", .libPaths(),
        paste("installed:", paste(head(rownames(installed.packages()), 40), collapse = " ")))
    }
    core_secs(proc.time()[["elapsed"]] - t0)
    core_msgs(msgs)
    core_ready(ok)
  }) |> bindEvent(session$clientData$url_protocol, once = TRUE)

  output$core_status <- renderText({
    if (core_ready()) sprintf("loaded (%.1fs)", core_secs())
    else if (is.na(core_secs())) "loading..."
    else sprintf("FAILED (%.1fs)", core_secs())
  })
  output$core_log <- renderText(paste(core_msgs(), collapse = "\n"))

  # Call through the namespace: library() inside a shinylive session does
  # not make exports visible to these closures.
  core <- function() asNamespace(core_pkg)
  methods_for <- function(which) {
    ns <- core()
    if (identical(which, "v1")) {
      list(ep = ns$ep_rolling_sd_v1(), sig = ns$sig_auto_arima_v1())
    } else {
      list(ep = ns$ep_rolling_sd(), sig = ns$sig_kpss())
    }
  }

  run_file <- function(path, label) tryCatch({
    if (!core_ready()) { say("core not loaded; cannot run"); return() }
    say("%s: exists=%s cwd=%s", label, file.exists(path), getwd())
    t0 <- proc.time()[["elapsed"]]
    ns <- core()
    set <- ns$read_thermogram_csv(path)
    t_read <- proc.time()[["elapsed"]] - t0
    n <- ns$n_samples(set)
    say("%s: read %d samples in %.2fs", label, n, t_read)
    m <- methods_for(input$ep)
    withProgress(message = sprintf("Processing %d samples", n), {
      b <- ns$bench_pipeline(set, endpoint_method = m$ep, signal_method = m$sig)
    })
    say("%s: %s", label, ns$format_bench(b))
    b$totals <- c(read_csv = t_read, b$totals)
    result(b)
  }, error = function(e) say("ERROR in %s: %s", label, conditionMessage(e)))

  observeEvent(input$run20, run_file("data/Small.csv", "Small.csv"))
  observeEvent(input$run_upload, {
    req(input$upload)
    run_file(input$upload$datapath, input$upload$name)
  })

  output$timing <- renderTable({
    b <- result(); req(b)
    data.frame(stage = names(b$totals), seconds = round(unname(b$totals), 3),
               per_sample_ms = round(1000 * unname(b$totals) / b$n, 1))
  })
  output$per_sample <- renderTable({
    b <- result(); req(b)
    head(b$per_sample, 25)
  })
  output$log <- renderText(paste(log_lines(), collapse = "\n"))
}

shinyApp(ui, server)
