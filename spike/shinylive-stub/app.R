# Stub shinylive app for the Tauri serving-path check (plan T5.3).
# Independent of thermogramforge.core: it only reports whether the page is
# crossOriginIsolated, whether shinylive's service worker took control, and
# how long webR took to boot. Rendered large so a window screenshot is legible.

library(shiny)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: -apple-system, Helvetica, sans-serif; padding: 24px; }
    .big { font-size: 28px; margin: 10px 0; }
    .ok { color: #198754; } .bad { color: #dc3545; }
  "))),
  tags$h2("ThermogramForge Tauri + shinylive probe"),
  tags$div(class = "big", "R side: ", tags$b(textOutput("r_ready", inline = TRUE))),
  tags$div(class = "big", id = "coi", "crossOriginIsolated: ..."),
  tags$div(class = "big", id = "sw", "serviceWorker: ..."),
  tags$div(class = "big", id = "proto", "location: ..."),
  tags$div(class = "big", id = "sab", "SharedArrayBuffer: ..."),
  tags$div(class = "big", "R platform: ", tags$b(textOutput("platform", inline = TRUE))),
  tags$div(class = "big", "ndiffs(kpss) works: ", tags$b(textOutput("kpss", inline = TRUE))),
  tags$script(HTML("
    function mark(id, label, ok, extra) {
      var el = document.getElementById(id);
      el.textContent = label + ': ' + (ok ? 'YES' : 'NO') + (extra ? ' (' + extra + ')' : '');
      el.className = 'big ' + (ok ? 'ok' : 'bad');
    }
    mark('coi', 'crossOriginIsolated', window.crossOriginIsolated === true);
    mark('sab', 'SharedArrayBuffer', typeof SharedArrayBuffer !== 'undefined');
    document.getElementById('proto').textContent = 'location: ' + window.location.href;
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker.getRegistration().then(function (reg) {
        mark('sw', 'serviceWorker', !!(reg && reg.active),
             reg ? (reg.active ? reg.active.scriptURL.split('/').pop() : 'registered, not active') : 'none');
      }).catch(function (e) { mark('sw', 'serviceWorker', false, 'error: ' + e); });
    } else {
      mark('sw', 'serviceWorker', false, 'API unavailable');
    }
  "))
)

server <- function(input, output, session) {
  output$r_ready <- renderText("R is running")
  output$platform <- renderText(R.version$platform)
  output$kpss <- renderText({
    ok <- tryCatch({
      requireNamespace("forecast", quietly = TRUE) &&
        is.numeric(forecast::ndiffs(cumsum(rnorm(100)), test = "kpss"))
    }, error = function(e) FALSE)
    if (ok) "yes" else "no"
  })
}

shinyApp(ui, server)
