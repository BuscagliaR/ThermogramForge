# ==============================================================================
# ThermogramForge Shiny Application
# ==============================================================================
# Main application file orchestrating all modules and shared state
#
# Architecture:
#   - Single-page application with tabbed navigation
#   - Shared reactive values (app_data) for cross-module communication
#   - Module-based structure for maintainability
#
# Modules:
#   - Data Overview: Upload, process, save/load datasets
#   - Review Endpoints: Interactive baseline review and adjustment
#   - Report Builder: Generate metrics reports using tlbparam
#
# Author: Chris Reger
# Last Updated: October 18, 2025
# ==============================================================================

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
library(openxlsx)  # For Excel file creation with multiple sheets

# === Force inclusion in manifest ===
# These packages are used in processing_utils.R and must be in manifest
library(ThermogramBaseline)
library(tlbparam)
library(forecast)
# === End manifest inclusion ===

# Source utility files using system.file for robustness
app_dir <- if (file.exists("utils/theme.R")) {
  "."  # We're already in inst/shiny-app
} else if (file.exists("inst/shiny-app/utils/theme.R")) {
  "inst/shiny-app"  # We're in package root
} else {
  stop("Cannot find utils directory!")
}

source(file.path(app_dir, "utils/theme.R"))
source(file.path(app_dir, "utils/data_utils.R"))

# Source processing utility files (split for maintainability)
# Order matters due to dependencies between files
source(file.path(app_dir, "utils/format_utils.R"))     # Format detection, validation, conversion
source(file.path(app_dir, "utils/file_io_utils.R"))    # File reading/writing (depends on format_utils)
source(file.path(app_dir, "utils/baseline_utils.R"))   # Baseline detection processing
source(file.path(app_dir, "utils/metrics_utils.R"))    # tlbparam metric calculation

# Source module files
source(file.path(app_dir, "modules/mod_upload_modal.R"))  # Sub-module for upload modal
source(file.path(app_dir, "modules/mod_data_overview.R"))
source(file.path(app_dir, "modules/mod_review_endpoints.R"))
source(file.path(app_dir, "modules/mod_report_builder.R"))
source(file.path(app_dir, "modules/mod_user_guide.R"))

# ==============================================================================
# USER INTERFACE
# ==============================================================================

ui <- page_navbar(
  title = "ThermogramForge",
  id = "main_navbar",
  theme = thermogram_theme(),
  fillable = TRUE,
  
  # Navbar options for sticky positioning
  # Note: Use navbar_options() for modern bslib syntax
  navbar_options = navbar_options(
    position = "fixed-top",  # Fixed to top of viewport
    bg = "white"
  ),
  
  padding = c("64px", 0, 0, 0),  # Bootstrap's default navbar height ~ 56px
  
  # Header content (scripts, CSS, etc.)
  header = tagList(
    
    # Enable shinyjs for dynamic UI manipulation
    useShinyjs(),
    
    # Add custom CSS from theme.R
    tags$head(
      tags$style(HTML(custom_css()))
    ),
    
    # Initialize Bootstrap tooltips for all elements with data-bs-toggle="tooltip"
    # Tooltips are used extensively in Report Builder for metric descriptions
    tags$script(HTML("
      $(document).ready(function() {
        
        // Initialize all tooltips on page load
        $('[data-bs-toggle=\"tooltip\"]').tooltip();
        
        // Re-initialize tooltips after Shiny updates content
        // This ensures dynamically generated tooltips work properly
        $(document).on('shiny:value', function() {
          setTimeout(function() {
            $('[data-bs-toggle=\"tooltip\"]').tooltip();
          }, 100);
        });
        
      });
    "))
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
  ),
  
  # Tab 4: User Guide
  nav_panel(
    title = "User Guide",
    icon = icon("book-open"),
    value = "user_guide",
    mod_user_guide_ui("user_guide")
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------------
  # Configuration
  # ----------------------------------------------------------------------------
  
  # Set upload size limit to 150MB for large datasets - may need to revise
  options(shiny.maxRequestSize = 150 * 1024^2)
  
  # ----------------------------------------------------------------------------
  # Shared Application State (Reactive Values)
  # ----------------------------------------------------------------------------
  # This reactiveValues object is shared across all modules
  # Modules can read and write to these values for cross-module communication
  
  app_data <- reactiveValues(
    
    # Data loading and processing
    raw_data = NULL,                    # Uploaded raw thermogram data
    processed_data = NULL,              # Processed data with baseline detection
    baseline_results = NULL,            # Baseline calculation results
    signal_detection = NULL,            # Signal detection parameters
    
    # Review Endpoints module state
    current_sample = NULL,              # Currently selected sample ID
    review_status = list(),             # Review flags for each sample
    undo_stack = list(),                # Undo history for manual adjustments
    redo_stack = list(),                # Redo history
    
    # Phase 7-8: Dataset tracking and navigation
    current_dataset_id = NULL,          # Unique ID of active dataset
    current_dataset_name = NULL,        # Display name of active dataset
    all_datasets = list(),              # All datasets (synced from Data Overview)
    dataset_load_trigger = 0,           # Incremented to trigger module updates
    navigate_to = NULL,                 # Target tab for programmatic navigation
    
    # Report generation
    generated_reports = list()          # List of generated report metadata
  )
  
  # ----------------------------------------------------------------------------
  # Module Server Calls
  # ----------------------------------------------------------------------------
  # Each module's server function is called here with the shared app_data
  
  mod_data_overview_server("data_overview", app_data)
  mod_review_endpoints_server("review_endpoints", app_data)
  mod_report_builder_server("report_builder", app_data)
  mod_user_guide_server("user_guide", app_data)
  
  # ----------------------------------------------------------------------------
  # Cross-Module Navigation Handler
  # ----------------------------------------------------------------------------
  # Allows modules to trigger navigation to other tabs programmatically
  # Example: After loading data, navigate to Review Endpoints
  
  observeEvent(app_data$navigate_to, {
    req(app_data$navigate_to)
    
    cat(sprintf("[APP] Programmatic navigation to: %s\n", app_data$navigate_to))
    
    # Update navbar to show requested tab
    updateNavbarPage(
      session = session,
      inputId = "main_navbar",
      selected = app_data$navigate_to
    )
    
    # Reset the navigation flag
    app_data$navigate_to <- NULL
  })
  
  # ----------------------------------------------------------------------------
  # Session Lifecycle
  # ----------------------------------------------------------------------------
  
  # Cleanup when user session ends
  session$onSessionEnded(function() {
    message("[APP] ThermogramForge session ended")
    
    # Any additional cleanup can go here
    # (e.g., temporary file deletion, logging)
  })
  
}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================

shinyApp(ui = ui, server = server)