# ==============================================================================
# Report Builder Module
# ==============================================================================
# Purpose: Generate reports with tlbparam metrics for thermal liquid biopsy data
# 
# Features:
#   - Interactive metric selection organized by category
#   - Real-time metric calculation using tlbparam package
#   - Preview table showing calculated metrics
#   - Export to CSV and Excel formats
#   - Direct download and save-to-disk options
#
# Data Flow:
#   1. User selects metrics from organized categories
#   2. Click "Generate Report" to calculate using tlbparam
#   3. Preview table displays results
#   4. Export to CSV/Excel (saves to reports/ directory)
#   5. Direct download option also available
#
# Dependencies:
#   - tlbparam package for metric calculations
#   - app_data$processed_data must be loaded
#   - processing_utils.R for export functions
# ==============================================================================

# ------------------------------------------------------------------------------
# UI Function
# ------------------------------------------------------------------------------
#' Report Builder User Interface
#'
#' Creates the UI for metric selection and report generation
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
mod_report_builder_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container-fluid p-3",
      
      # ========================================================================
      # Dataset Information Card
      # ========================================================================
      # Shows which dataset is currently loaded and ready for analysis
      div(
        class = "card mb-3",
        div(
          class = "card-header bg-primary text-white",
          icon("database"), " Dataset Information"
        ),
        div(
          class = "card-body",
          uiOutput(ns("dataset_info_display"))
        )
      ),
      
      # ========================================================================
      # Metric Selection Card
      # ========================================================================
      # Organized by category with individual All/None buttons per category
      div(
        class = "card mb-3",
        div(
          class = "card-header bg-info text-white",
          icon("list-check"), " Select Metrics to Calculate"
        ),
        div(
          class = "card-body",
          
          # Global control buttons for all metrics
          div(
            class = "mb-3",
            actionButton(
              ns("select_all_metrics"),
              "Select All",
              icon = icon("check-double"),
              class = "btn-success btn-sm me-2"
            ),
            actionButton(
              ns("clear_all_metrics"),
              "Clear All",
              icon = icon("xmark"),
              class = "btn-secondary btn-sm me-2"
            ),
            actionButton(
              ns("reset_to_defaults"),
              "Reset to Defaults",
              icon = icon("rotate-left"),
              class = "btn-outline-primary btn-sm me-2"
            ),
            # Dynamic button - changes based on whether metrics calculated
            uiOutput(ns("calculate_button_ui"))
          ),
          
          hr(),
          
          # Three-column layout for metric categories
          div(
            class = "row",
            
            # ==================================================================
            # Column 1: Peak and Transition Metrics
            # ==================================================================
            div(
              class = "col-md-4",
              
              # Peak Metrics
              # Temperatures where major peaks occur in thermogram
              div(
                class = "card mb-3",
                div(
                  class = "card-header d-flex justify-content-between align-items-center",
                  tags$strong("Peak Metrics"),
                  # Category-specific controls
                  div(
                    class = "btn-group btn-group-sm",
                    actionButton(
                      ns("select_all_peak"),
                      "All",
                      class = "btn-outline-success btn-xs",
                      style = "font-size: 0.75rem; padding: 0.15rem 0.4rem;"
                    ),
                    actionButton(
                      ns("clear_all_peak"),
                      "None",
                      class = "btn-outline-secondary btn-xs",
                      style = "font-size: 0.75rem; padding: 0.15rem 0.4rem;"
                    )
                  )
                ),
                div(
                  class = "card-body",
                  checkboxGroupInput(
                    ns("metrics_peak"),
                    label = NULL,
                    choices = c(
                      "Melting Temperature (Tm)" = "Tm",
                      "Peak 1 Temperature (TPeak 1)" = "Tpeak_1",
                      "Peak 2 Temperature (TPeak 2)" = "Tpeak_2",
                      "Peak 3 Temperature (TPeak 3)" = "Tpeak_3",
                      "Fibrinogen Peak Temp (TPeak F)" = "Tpeak_f"
                    ),
                    selected = c("Tm", "Tpeak_1", "Tpeak_2")
                  )
                )
              ),
              
              # Transition Metrics
              # Temperatures at specific transition points
              div(
                class = "card mb-3",
                div(
                  class = "card-header d-flex justify-content-between align-items-center",
                  tags$strong("Transition Metrics"),
                  div(
                    class = "btn-group btn-group-sm",
                    actionButton(
                      ns("select_all_transition"),
                      "All",
                      class = "btn-outline-success btn-xs",
                      style = "font-size: 0.75rem; padding: 0.15rem 0.4rem;"
                    ),
                    actionButton(
                      ns("clear_all_transition"),
                      "None",
                      class = "btn-outline-secondary btn-xs",
                      style = "font-size: 0.75rem; padding: 0.15rem 0.4rem;"
                    )
                  )
                ),
                div(
                  class = "card-body",
                  checkboxGroupInput(
                    ns("metrics_transition"),
                    label = NULL,
                    choices = c(
                      "Transition 1 Temperature" = "Tm1",
                      "Transition 2 Temperature" = "Tm2",
                      "Transition 3 Temperature" = "Tm3",
                      "Valley Temperature (TV1.2)" = "TV12"
                    ),
                    selected = c("Tm1", "Tm2")
                  )
                )
              )
            ),
            
            # ==================================================================
            # Column 2: Area, Shape, and Height Metrics
            # ==================================================================
            div(
              class = "col-md-4",
              
              # Area Metrics
              # Total area under thermogram curve
              div(
                class = "card mb-3",
                div(
                  class = "card-header d-flex justify-content-between align-items-center",
                  tags$strong("Area Metrics"),
                  div(
                    class = "btn-group btn-group-sm",
                    actionButton(
                      ns("select_all_area"),
                      "All",
                      class = "btn-outline-success btn-xs",
                      style = "font-size: 0.75rem; padding: 0.15rem 0.4rem;"
                    ),
                    actionButton(
                      ns("clear_all_area"),
                      "None",
                      class = "btn-outline-secondary btn-xs",
                      style = "font-size: 0.75rem; padding: 0.15rem 0.4rem;"
                    )
                  )
                ),
                div(
                  class = "card-body",
                  checkboxGroupInput(
                    ns("metrics_area"),
                    label = NULL,
                    choices = c(
                      "Area Under Curve" = "AUC",
                      "Total Area" = "Area"
                    ),
                    selected = c("AUC")
                  )
                )
              ),
              
              # Shape Metrics
              # Characteristics of thermogram shape
              div(
                class = "card mb-3",
                div(
                  class = "card-header d-flex justify-content-between align-items-center",
                  tags$strong("Shape Metrics"),
                  div(
                    class = "btn-group btn-group-sm",
                    actionButton(
                      ns("select_all_shape"),
                      "All",
                      class = "btn-outline-success btn-xs",
                      style = "font-size: 0.75rem; padding: 0.15rem 0.4rem;"
                    ),
                    actionButton(
                      ns("clear_all_shape"),
                      "None",
                      class = "btn-outline-secondary btn-xs",
                      style = "font-size: 0.75rem; padding: 0.15rem 0.4rem;"
                    )
                  )
                ),
                div(
                  class = "card-body",
                  checkboxGroupInput(
                    ns("metrics_shape"),
                    label = NULL,
                    choices = c(
                      "Full Width Half Max (FWHM)" = "FWHM",
                      "Width at 50% Max" = "Width_50"
                    ),
                    selected = c("FWHM")
                  )
                )
              ),
              
              # Height Metrics
              # Min, max, and median heat capacity values
              div(
                class = "card mb-3",
                div(
                  class = "card-header d-flex justify-content-between align-items-center",
                  tags$strong("Height Metrics"),
                  div(
                    class = "btn-group btn-group-sm",
                    actionButton(
                      ns("select_all_height"),
                      "All",
                      class = "btn-outline-success btn-xs",
                      style = "font-size: 0.75rem; padding: 0.15rem 0.4rem;"
                    ),
                    actionButton(
                      ns("clear_all_height"),
                      "None",
                      class = "btn-outline-secondary btn-xs",
                      style = "font-size: 0.75rem; padding: 0.15rem 0.4rem;"
                    )
                  )
                ),
                div(
                  class = "card-body",
                  checkboxGroupInput(
                    ns("metrics_height"),
                    label = NULL,
                    choices = c(
                      "Maximum Heat Capacity" = "Max",
                      "Minimum Heat Capacity" = "Min",
                      "Median Heat Capacity" = "Median"
                    ),
                    selected = c("Max")
                  )
                )
              )
            ),
            
            # ==================================================================
            # Column 3: Temperature Metrics and Info
            # ==================================================================
            div(
              class = "col-md-4",
              
              # Temperature Metrics
              # Specific temperature measurements
              div(
                class = "card mb-3",
                div(
                  class = "card-header d-flex justify-content-between align-items-center",
                  tags$strong("Temperature Metrics"),
                  div(
                    class = "btn-group btn-group-sm",
                    actionButton(
                      ns("select_all_temperature"),
                      "All",
                      class = "btn-outline-success btn-xs",
                      style = "font-size: 0.75rem; padding: 0.15rem 0.4rem;"
                    ),
                    actionButton(
                      ns("clear_all_temperature"),
                      "None",
                      class = "btn-outline-secondary btn-xs",
                      style = "font-size: 0.75rem; padding: 0.15rem 0.4rem;"
                    )
                  )
                ),
                div(
                  class = "card-body",
                  checkboxGroupInput(
                    ns("metrics_temperature"),
                    label = NULL,
                    choices = c(
                      "Temperature at Maximum" = "TMax",
                      "Temperature at Minimum" = "TMin",
                      "First Moment Temperature" = "TFM"
                    ),
                    selected = c("TMax")
                  )
                )
              ),
              
              # Helpful info box
              div(
                class = "alert alert-info",
                icon("info-circle"), " ",
                tags$strong("Tip:"), " Click 'Generate Report' to compute metric values"
              )
            )
          )
        )
      ),
      
      # ========================================================================
      # Report Preview Card
      # ========================================================================
      # Shows calculated metrics in an interactive table
      div(
        class = "card mb-3",
        div(
          class = "card-header bg-success text-white",
          icon("table"), " Report Preview"
        ),
        div(
          class = "card-body",
          uiOutput(ns("report_preview_display"))
        )
      ),
      
      # ========================================================================
      # Export Options Card (Conditional)
      # ========================================================================
      # Only displayed after metrics have been calculated
      # Provides both save-to-disk and direct download options
      uiOutput(ns("export_card_ui"))
    )
  )
}

# ------------------------------------------------------------------------------
# Server Function
# ------------------------------------------------------------------------------
#' Report Builder Server Logic
#'
#' Handles metric calculation, preview display, and export functionality
#'
#' @param id Module namespace ID
#' @param app_data Reactive values containing processed data and app state
#' @return NULL (module manages internal state only)
mod_report_builder_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ==========================================================================
    # Reactive Values
    # ==========================================================================
    # Internal state management for this module
    
    # Stores the data frame of calculated metrics (Sample_ID + metric columns)
    # NULL until first calculation, then persists for export
    calculated_metrics <- reactiveVal(NULL)
    
    # Flag to prevent multiple simultaneous calculations
    # TRUE while calculation in progress, FALSE otherwise
    calculating <- reactiveVal(FALSE)
    
    # ==========================================================================
    # Dataset Reload Observer
    # ==========================================================================
    # Triggered when user loads a different dataset
    # Clears previous calculations to prevent confusion
    
    observeEvent(app_data$dataset_load_trigger, {
      req(app_data$dataset_load_trigger > 0)
      
      cat(sprintf("\n[REPORT_BUILDER] Dataset reload triggered (trigger=%d)\n", 
                  app_data$dataset_load_trigger))
      cat(sprintf("[REPORT_BUILDER] Loading dataset: %s\n", 
                  app_data$current_dataset_name))
      
      # Clear any previous calculations
      calculated_metrics(NULL)
      
      # Log status
      if (!is.null(app_data$processed_data)) {
        n_samples <- length(app_data$processed_data$samples)
        cat(sprintf("[REPORT_BUILDER] Dataset loaded: %d samples available\n", n_samples))
      } else {
        cat("[REPORT_BUILDER] WARNING: No processed data available\n")
      }
      
    }, ignoreInit = TRUE, priority = 100)
    
    # ==========================================================================
    # Dataset Information Display
    # ==========================================================================
    # Shows name, sample count, and status of loaded dataset
    
    output$dataset_info_display <- renderUI({
      
      # Check if data is loaded
      if (is.null(app_data$processed_data)) {
        return(
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"), " ",
            "No processed data available. Please process data in the Data Overview tab."
          )
        )
      }
      
      # Extract information from loaded data
      data <- app_data$processed_data
      n_samples <- length(data$samples)
      
      dataset_name <- if (!is.null(app_data$current_dataset_name)) {
        app_data$current_dataset_name
      } else {
        "Unknown Dataset"
      }
      
      # Display dataset information
      tagList(
        div(
          class = "row",
          div(
            class = "col-md-4",
            tags$strong("Dataset:"), " ", dataset_name
          ),
          div(
            class = "col-md-4",
            tags$strong("Samples:"), " ", n_samples
          ),
          div(
            class = "col-md-4",
            tags$strong("Status:"), " ",
            span(
              class = "badge bg-success",
              "Ready for Analysis"
            )
          )
        )
      )
    })
    
    # ==========================================================================
    # Metric Selection Helper
    # ==========================================================================
    # Aggregates all selected metrics from different categories into one vector
    
    selected_metrics <- reactive({
      metrics <- c(
        input$metrics_peak,
        input$metrics_transition,
        input$metrics_area,
        input$metrics_shape,
        input$metrics_height,
        input$metrics_temperature
      )
      # Remove duplicates (shouldn't occur, but defensive programming)
      unique(metrics)
    })
    
    # ==========================================================================
    # Dynamic Calculate Button
    # ==========================================================================
    # Changes text and appearance based on whether metrics have been calculated
    # - Before: "Generate Report" (blue, calculator icon)
    # - After: "Regenerate Report" (yellow, rotate icon)
    
    output$calculate_button_ui <- renderUI({
      
      has_calculated <- !is.null(calculated_metrics())
      
      if (has_calculated) {
        # After calculation - allow regeneration with different metrics
        actionButton(
          ns("calculate_preview"),
          "Regenerate Report",
          icon = icon("rotate"),
          class = "btn-warning btn-sm"
        )
      } else {
        # Before calculation - initial generation
        actionButton(
          ns("calculate_preview"),
          "Generate Report",
          icon = icon("calculator"),
          class = "btn-primary btn-sm"
        )
      }
    })
    
    # ==========================================================================
    # Global Metric Selection Buttons
    # ==========================================================================
    # Select All, Clear All, Reset to Defaults buttons
    
    # Select All - checks all metrics across all categories
    observeEvent(input$select_all_metrics, {
      updateCheckboxGroupInput(session, "metrics_peak", 
                               selected = c("Tm", "Tpeak_1", "Tpeak_2", "Tpeak_3", "Tpeak_f"))
      updateCheckboxGroupInput(session, "metrics_transition",
                               selected = c("Tm1", "Tm2", "Tm3", "TV12"))
      updateCheckboxGroupInput(session, "metrics_area",
                               selected = c("AUC", "Area"))
      updateCheckboxGroupInput(session, "metrics_shape",
                               selected = c("FWHM", "Width_50"))
      updateCheckboxGroupInput(session, "metrics_height",
                               selected = c("Max", "Min", "Median"))
      updateCheckboxGroupInput(session, "metrics_temperature",
                               selected = c("TMax", "TMin", "TFM"))
      
      showNotification("All metrics selected", type = "message", duration = 2)
    })
    
    # Clear All - unchecks all metrics
    observeEvent(input$clear_all_metrics, {
      updateCheckboxGroupInput(session, "metrics_peak", selected = character(0))
      updateCheckboxGroupInput(session, "metrics_transition", selected = character(0))
      updateCheckboxGroupInput(session, "metrics_area", selected = character(0))
      updateCheckboxGroupInput(session, "metrics_shape", selected = character(0))
      updateCheckboxGroupInput(session, "metrics_height", selected = character(0))
      updateCheckboxGroupInput(session, "metrics_temperature", selected = character(0))
      
      # Clear calculated metrics since selection changed
      calculated_metrics(NULL)
      
      showNotification("All metrics cleared", type = "message", duration = 2)
    })
    
    # Reset to Defaults - selects commonly used metrics
    observeEvent(input$reset_to_defaults, {
      updateCheckboxGroupInput(session, "metrics_peak", 
                               selected = c("Tm", "Tpeak_1", "Tpeak_2"))
      updateCheckboxGroupInput(session, "metrics_transition",
                               selected = c("Tm1", "Tm2"))
      updateCheckboxGroupInput(session, "metrics_area",
                               selected = c("AUC"))
      updateCheckboxGroupInput(session, "metrics_shape",
                               selected = c("FWHM"))
      updateCheckboxGroupInput(session, "metrics_height",
                               selected = c("Max"))
      updateCheckboxGroupInput(session, "metrics_temperature",
                               selected = c("TMax"))
      
      showNotification("Reset to default metrics", type = "message", duration = 2)
    })
    
    # ==========================================================================
    # Category-Specific Select/Clear Buttons
    # ==========================================================================
    # Each metric category has its own All/None buttons in the card header
    
    # Peak Metrics
    observeEvent(input$select_all_peak, {
      updateCheckboxGroupInput(session, "metrics_peak", 
                               selected = c("Tm", "Tpeak_1", "Tpeak_2", "Tpeak_3", "Tpeak_f"))
    })
    observeEvent(input$clear_all_peak, {
      updateCheckboxGroupInput(session, "metrics_peak", selected = character(0))
    })
    
    # Transition Metrics
    observeEvent(input$select_all_transition, {
      updateCheckboxGroupInput(session, "metrics_transition",
                               selected = c("Tm1", "Tm2", "Tm3", "TV12"))
    })
    observeEvent(input$clear_all_transition, {
      updateCheckboxGroupInput(session, "metrics_transition", selected = character(0))
    })
    
    # Area Metrics
    observeEvent(input$select_all_area, {
      updateCheckboxGroupInput(session, "metrics_area",
                               selected = c("AUC", "Area"))
    })
    observeEvent(input$clear_all_area, {
      updateCheckboxGroupInput(session, "metrics_area", selected = character(0))
    })
    
    # Shape Metrics
    observeEvent(input$select_all_shape, {
      updateCheckboxGroupInput(session, "metrics_shape",
                               selected = c("FWHM", "Width_50"))
    })
    observeEvent(input$clear_all_shape, {
      updateCheckboxGroupInput(session, "metrics_shape", selected = character(0))
    })
    
    # Height Metrics
    observeEvent(input$select_all_height, {
      updateCheckboxGroupInput(session, "metrics_height",
                               selected = c("Max", "Min", "Median"))
    })
    observeEvent(input$clear_all_height, {
      updateCheckboxGroupInput(session, "metrics_height", selected = character(0))
    })
    
    # Temperature Metrics
    observeEvent(input$select_all_temperature, {
      updateCheckboxGroupInput(session, "metrics_temperature",
                               selected = c("TMax", "TMin", "TFM"))
    })
    observeEvent(input$clear_all_temperature, {
      updateCheckboxGroupInput(session, "metrics_temperature", selected = character(0))
    })
    
    # ==========================================================================
    # Calculate/Regenerate Report Button Handler
    # ==========================================================================
    # Calculates selected metrics using tlbparam package
    # Stores results for preview and export
    
    observeEvent(input$calculate_preview, {
      
      cat("\n[REPORT_BUILDER] Calculate/Regenerate button clicked\n")
      
      # Validation: Check if data is loaded
      if (is.null(app_data$processed_data)) {
        showNotification(
          "No processed data available. Please load data first.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      # Validation: Check if metrics are selected
      metrics <- selected_metrics()
      if (length(metrics) == 0) {
        showNotification(
          "Please select at least one metric to calculate.",
          type = "warning",
          duration = 3
        )
        return()
      }
      
      # Prevent multiple simultaneous calculations
      calculating(TRUE)
      
      # Notify user that calculation is starting
      showNotification(
        sprintf("Calculating %d metrics for %d samples...", 
                length(metrics),
                length(app_data$processed_data$samples)),
        type = "message",
        duration = 3,
        id = "calc_notification"
      )
      
      # Call calculation function from processing_utils.R
      result <- tryCatch({
        calculate_tlbparam_metrics(
          processed_data = app_data$processed_data,
          selected_metrics = metrics
        )
      }, error = function(e) {
        # Handle calculation errors gracefully
        showNotification(
          sprintf("Error calculating metrics: %s", e$message),
          type = "error",
          duration = 10
        )
        return(NULL)
      })
      
      # Reset calculating flag
      calculating(FALSE)
      
      # Store results if successful
      if (!is.null(result)) {
        calculated_metrics(result)
        showNotification(
          sprintf("Successfully calculated %d metrics for %d samples", 
                  ncol(result) - 1,
                  nrow(result)),
          type = "message",
          duration = 5
        )
      }
    })
    
    # ==========================================================================
    # Report Preview Display
    # ==========================================================================
    # Shows different messages based on state:
    # 1. No data loaded
    # 2. Data loaded but no metrics selected
    # 3. Metrics selected but not calculated
    # 4. Metrics calculated - show preview table
    
    output$report_preview_display <- renderUI({
      
      # State 1: No data loaded
      if (is.null(app_data$processed_data)) {
        return(
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"), " ",
            "No processed data available. Please process data in the Data Overview tab."
          )
        )
      }
      
      # State 2: No metrics selected
      metrics <- selected_metrics()
      if (length(metrics) == 0) {
        return(
          div(
            class = "alert alert-info",
            icon("info-circle"), " ",
            "Select metrics above and click 'Generate Report' to see results."
          )
        )
      }
      
      # State 3: Metrics selected but not calculated
      results <- calculated_metrics()
      if (is.null(results)) {
        return(
          div(
            class = "alert alert-info",
            icon("calculator"), " ",
            tags$strong(length(metrics)), " metrics selected. ",
            "Click 'Generate Report' to compute values."
          )
        )
      }
      
      # State 4: Show preview table
      tagList(
        div(
          class = "alert alert-success mb-3",
          icon("check-circle"), " ",
          sprintf("Showing %d metrics for %d samples", 
                  ncol(results) - 1,
                  nrow(results))
        ),
        DT::dataTableOutput(ns("metrics_preview_table"))
      )
    })
    
    # Render the metrics preview table
    # Interactive table with sorting, search, and pagination
    output$metrics_preview_table <- DT::renderDataTable({
      req(calculated_metrics())
      
      results <- calculated_metrics()
      
      DT::datatable(
        results,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'ftp',  # Filter, table, pagination
          columnDefs = list(
            list(targets = 0, className = "dt-left")
          )
        ),
        rownames = FALSE,
        class = "compact hover"
      ) %>%
        DT::formatRound(
          columns = 2:ncol(results),
          digits = 3
        )
    })
    
    # ==========================================================================
    # Conditional Export Card Display
    # ==========================================================================
    # Only shows export options after metrics have been calculated
    # Provides both save-to-disk and direct download functionality
    
    output$export_card_ui <- renderUI({
      
      # Hide card if no metrics calculated yet
      if (is.null(calculated_metrics())) {
        return(NULL)
      }
      
      # Show export card with download links and save buttons
      div(
        class = "card mb-3",
        div(
          class = "card-header bg-warning d-flex justify-content-between align-items-center",
          div(
            icon("file-export"), " Export Report"
          ),
          div(
            class = "btn-group",
            # Direct download buttons (browser download)
            downloadLink(
              ns("download_csv"),
              label = tagList(icon("download"), " Download CSV"),
              class = "btn btn-primary btn-sm"
            ),
            downloadLink(
              ns("download_excel"),
              label = tagList(icon("download"), " Download Excel"),
              class = "btn btn-success btn-sm"
            )
          )
        ),
        div(
          class = "card-body",
          
          # Report naming section
          div(
            class = "row mb-3",
            div(
              class = "col-md-8",
              textInput(
                ns("report_name"),
                "Report Name:",
                value = "",
                placeholder = "e.g., Blood_Plasma_Analysis_1"
              ),
              div(
                class = "form-text",
                "Leave blank for automatic naming with timestamp"
              )
            )
          ),
          
          # Save to disk buttons (saves to reports/ directory)
          div(
            class = "row mb-3",
            div(
              class = "col-md-12",
              tags$strong("Save to reports/ directory:"),
              div(
                class = "btn-group mt-2",
                actionButton(
                  ns("export_csv"),
                  "Export as CSV",
                  icon = icon("file-csv"),
                  class = "btn-outline-primary btn-sm"
                ),
                actionButton(
                  ns("export_excel"),
                  "Export as Excel",
                  icon = icon("file-excel"),
                  class = "btn-outline-success btn-sm"
                )
              )
            )
          ),
          
          # Export status messages
          div(
            class = "mt-3",
            uiOutput(ns("export_status_display"))
          )
        )
      )
    })
    
    # ==========================================================================
    # Direct Download Handlers
    # ==========================================================================
    # Triggers browser download dialog without saving to reports/ directory
    
    # CSV Download Handler
    output$download_csv <- downloadHandler(
      filename = function() {
        metrics <- calculated_metrics()
        req(metrics)
        
        report_name <- trimws(input$report_name)
        if (nchar(report_name) == 0) {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          sprintf("Report_%s.csv", timestamp)
        } else {
          clean_name <- gsub("[^A-Za-z0-9_-]", "_", report_name)
          sprintf("%s.csv", clean_name)
        }
      },
      content = function(file) {
        metrics <- calculated_metrics()
        req(metrics)
        
        readr::write_csv(metrics, file)
        
        cat(sprintf("[REPORT_BUILDER] Direct CSV download: %s\n", basename(file)))
      }
    )
    
    # Excel Download Handler
    output$download_excel <- downloadHandler(
      filename = function() {
        metrics <- calculated_metrics()
        req(metrics)
        
        report_name <- trimws(input$report_name)
        if (nchar(report_name) == 0) {
          timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
          sprintf("Report_%s.xlsx", timestamp)
        } else {
          clean_name <- gsub("[^A-Za-z0-9_-]", "_", report_name)
          sprintf("%s.xlsx", clean_name)
        }
      },
      content = function(file) {
        metrics <- calculated_metrics()
        req(metrics)
        
        dataset_name <- if (!is.null(app_data$current_dataset_name)) {
          app_data$current_dataset_name
        } else {
          "Unknown_Dataset"
        }
        
        # Create metadata sheet
        metric_names <- setdiff(names(metrics), "Sample_ID")
        
        metadata_sheet <- data.frame(
          Property = c(
            "Report Generated",
            "Source Dataset",
            "Number of Samples",
            "Number of Metrics",
            "Metrics Included"
          ),
          Value = c(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            dataset_name,
            as.character(nrow(metrics)),
            as.character(length(metric_names)),
            paste(metric_names, collapse = ", ")
          ),
          stringsAsFactors = FALSE
        )
        
        # Write multi-sheet Excel file
        writexl::write_xlsx(
          list(
            Metrics = metrics,
            Metadata = metadata_sheet
          ),
          path = file
        )
        
        cat(sprintf("[REPORT_BUILDER] Direct Excel download: %s\n", basename(file)))
      }
    )
    
    # ==========================================================================
    # Save to Disk Handlers (Export Buttons)
    # ==========================================================================
    # Saves files to reports/ directory and tracks in app_data$generated_reports
    
    # CSV Export Handler
    observeEvent(input$export_csv, {
      
      cat("\n[REPORT_BUILDER] CSV export button clicked\n")
      
      # Validation: Check if metrics calculated
      metrics <- calculated_metrics()
      
      if (is.null(metrics)) {
        showNotification(
          "Please calculate metrics first using the 'Generate Report' button.",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      if (nrow(metrics) == 0) {
        showNotification(
          "No metric data available to export.",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      # Get report name and dataset name
      report_name <- trimws(input$report_name)
      
      dataset_name <- if (!is.null(app_data$current_dataset_name)) {
        app_data$current_dataset_name
      } else {
        "Unknown_Dataset"
      }
      
      cat(sprintf("[REPORT_BUILDER] Exporting %d samples, %d metrics\n",
                  nrow(metrics), ncol(metrics) - 1))
      cat(sprintf("[REPORT_BUILDER] Report name: '%s'\n", 
                  ifelse(nchar(report_name) > 0, report_name, "(auto-generated)")))
      
      # Call export function from processing_utils.R
      result <- export_report_csv(
        metrics_data = metrics,
        report_name = report_name,
        dataset_name = dataset_name
      )
      
      if (result$success) {
        
        # Generate unique report ID
        report_id <- paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        
        # Store report metadata for history tracking
        new_report <- list(
          id = report_id,
          name = tools::file_path_sans_ext(result$filename),
          format = "csv",
          filepath = result$filepath,
          dataset_name = dataset_name,
          n_samples = nrow(metrics),
          n_metrics = ncol(metrics) - 1,
          generated_at = Sys.time()
        )
        
        # Add to app_data for display in Data Overview
        app_data$generated_reports[[report_id]] <- new_report
        
        cat(sprintf("[REPORT_BUILDER] Report metadata stored (ID: %s)\n", report_id))
        cat(sprintf("[REPORT_BUILDER] Total reports in history: %d\n", 
                    length(app_data$generated_reports)))
        
        # Show success notification
        showNotification(
          ui = tagList(
            icon("check-circle"), " ",
            tags$strong("CSV Report Generated!"), tags$br(),
            sprintf("File: %s", result$filename), tags$br(),
            sprintf("Location: reports/")
          ),
          type = "message",
          duration = 8
        )
        
        # Clear report name input for next export
        updateTextInput(session, "report_name", value = "")
        
      } else {
        # Show error notification
        showNotification(
          sprintf("Export failed: %s", result$message),
          type = "error",
          duration = 10
        )
      }
      
    })
    
    # Excel Export Handler
    observeEvent(input$export_excel, {
      
      cat("\n[REPORT_BUILDER] Excel export button clicked\n")
      
      # Validation: Check if metrics calculated
      metrics <- calculated_metrics()
      
      if (is.null(metrics)) {
        showNotification(
          "Please calculate metrics first using the 'Generate Report' button.",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      if (nrow(metrics) == 0) {
        showNotification(
          "No metric data available to export.",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      # Get report name and dataset name
      report_name <- trimws(input$report_name)
      
      dataset_name <- if (!is.null(app_data$current_dataset_name)) {
        app_data$current_dataset_name
      } else {
        "Unknown_Dataset"
      }
      
      cat(sprintf("[REPORT_BUILDER] Exporting %d samples, %d metrics to Excel\n",
                  nrow(metrics), ncol(metrics) - 1))
      cat(sprintf("[REPORT_BUILDER] Report name: '%s'\n", 
                  ifelse(nchar(report_name) > 0, report_name, "(auto-generated)")))
      
      # Call export function from processing_utils.R
      result <- export_report_excel(
        metrics_data = metrics,
        report_name = report_name,
        dataset_name = dataset_name
      )
      
      if (result$success) {
        
        # Generate unique report ID
        report_id <- paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        
        # Store report metadata for history tracking
        new_report <- list(
          id = report_id,
          name = tools::file_path_sans_ext(result$filename),
          format = "xlsx",
          filepath = result$filepath,
          dataset_name = dataset_name,
          n_samples = nrow(metrics),
          n_metrics = ncol(metrics) - 1,
          generated_at = Sys.time()
        )
        
        # Add to app_data for display in Data Overview
        app_data$generated_reports[[report_id]] <- new_report
        
        cat(sprintf("[REPORT_BUILDER] Report metadata stored (ID: %s)\n", report_id))
        cat(sprintf("[REPORT_BUILDER] Total reports in history: %d\n", 
                    length(app_data$generated_reports)))
        
        # Show success notification
        showNotification(
          ui = tagList(
            icon("check-circle"), " ",
            tags$strong("Excel Report Generated!"), tags$br(),
            sprintf("File: %s", result$filename), tags$br(),
            sprintf("Location: reports/"), tags$br(),
            tags$small("Contains 2 sheets: Metrics + Metadata")
          ),
          type = "message",
          duration = 10
        )
        
        # Clear report name input for next export
        updateTextInput(session, "report_name", value = "")
        
      } else {
        # Show error notification
        showNotification(
          sprintf("Export failed: %s", result$message),
          type = "error",
          duration = 10
        )
      }
      
    })
    
    # Export status display (currently unused but available for future features)
    output$export_status_display <- renderUI({
      NULL
    })
    
    # ==========================================================================
    # Module Return
    # ==========================================================================
    # This module manages its own state and doesn't return values to parent
    NULL
    
  })
}