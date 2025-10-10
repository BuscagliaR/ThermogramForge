# ThermogramForge Shiny Application
# Main application file

# Load required packages
library(shiny)
library(bslib)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)
library(readr)
library(readxl)

# Source utility files
source("utils/theme.R")
source("utils/data_utils.R")
source("utils/processing_utils.R")

# Source module files
source("modules/mod_data_overview.R")
source("modules/mod_review_endpoints.R")
source("modules/mod_report_builder.R")

# Define UI
ui <- page_navbar(
  title = "ThermogramForge",
  id = "main_navbar",
  theme = thermogram_theme(),
  fillable = TRUE,
  
  # Enable shinyjs
  useShinyjs(),
  
  # Add custom CSS
  tags$head(
    tags$style(HTML(custom_css()))
  ),
  
  # Tab 1: Data Overview
  nav_panel(
    title = "Data Overview",
    icon = icon("table"),
    value = "data_overview",
    mod_data_overview_ui("data_overview")
  ),
  
  # Tab 2: Review Endpoints
  nav_panel(
    title = "Review Endpoints",
    icon = icon("chart-line"),
    value = "review_endpoints",
    mod_review_endpoints_ui("review_endpoints")
  ),
  
  # Tab 3: Report Builder
  nav_panel(
    title = "Report Builder",
    icon = icon("file-export"),
    value = "report_builder",
    mod_report_builder_ui("report_builder")
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Increase upload size limit to 150MB
  options(shiny.maxRequestSize = 150 * 1024^2)
  
  # Initialize reactive values for cross-module communication
  app_data <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    baseline_results = NULL,
    signal_detection = NULL,
    current_sample = NULL,
    review_status = list(),
    undo_stack = list(),
    redo_stack = list()
  )
  
  # Call module servers
  mod_data_overview_server("data_overview", app_data)
  mod_review_endpoints_server("review_endpoints", app_data)
  mod_report_builder_server("report_builder", app_data)
  
  # Session cleanup
  session$onSessionEnded(function() {
    message("ThermogramForge session ended")
  })
}

# Create Shiny app object
shinyApp(ui = ui, server = server)