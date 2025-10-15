# Report Builder Module
# Phase 8: Metric selection and report generation with tlbparam integration

#' Report Builder UI
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI elements
mod_report_builder_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container-fluid p-3",
      
      # Dataset Info Card
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
      
      # Metric Selection Card
      div(
        class = "card mb-3",
        div(
          class = "card-header bg-info text-white",
          icon("list-check"), " Select Metrics to Calculate"
        ),
        div(
          class = "card-body",
          
          # Control buttons
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
            actionButton(
              ns("calculate_preview"),
              "Calculate Preview",
              icon = icon("calculator"),
              class = "btn-primary btn-sm"
            )
          ),
          
          hr(),
          
          # Metric categories with checkboxes
          div(
            class = "row",
            
            # Column 1
            div(
              class = "col-md-4",
              
              # Peak Metrics
              div(
                class = "card mb-3",
                div(
                  class = "card-header",
                  tags$strong("Peak Metrics")
                ),
                div(
                  class = "card-body",
                  checkboxGroupInput(
                    ns("metrics_peak"),
                    label = NULL,
                    choices = c(
                      "Tm" = "Tm",
                      "TPeak_1" = "Tpeak_1",
                      "TPeak_2" = "Tpeak_2",
                      "TPeak_3" = "Tpeak_3",
                      "TPeak_f" = "Tpeak_f"
                    ),
                    selected = c("Tm", "Tpeak_1", "Tpeak_2")  # Defaults
                  )
                )
              ),
              
              # Transition Metrics
              div(
                class = "card mb-3",
                div(
                  class = "card-header",
                  tags$strong("Transition Metrics")
                ),
                div(
                  class = "card-body",
                  checkboxGroupInput(
                    ns("metrics_transition"),
                    label = NULL,
                    choices = c(
                      "Tm1" = "Tm1",
                      "Tm2" = "Tm2",
                      "Tm3" = "Tm3",
                      "TV12" = "TV12"
                    ),
                    selected = c("Tm1", "Tm2")  # Defaults
                  )
                )
              )
            ),
            
            # Column 2
            div(
              class = "col-md-4",
              
              # Area Metrics
              div(
                class = "card mb-3",
                div(
                  class = "card-header",
                  tags$strong("Area Metrics")
                ),
                div(
                  class = "card-body",
                  checkboxGroupInput(
                    ns("metrics_area"),
                    label = NULL,
                    choices = c(
                      "AUC" = "AUC",
                      "Area" = "Area"
                    ),
                    selected = c("AUC")  # Default
                  )
                )
              ),
              
              # Shape Metrics
              div(
                class = "card mb-3",
                div(
                  class = "card-header",
                  tags$strong("Shape Metrics")
                ),
                div(
                  class = "card-body",
                  checkboxGroupInput(
                    ns("metrics_shape"),
                    label = NULL,
                    choices = c(
                      "FWHM" = "FWHM",
                      "Width_50" = "Width_50"
                    ),
                    selected = c("FWHM")  # Default
                  )
                )
              ),
              
              # Height Metrics
              div(
                class = "card mb-3",
                div(
                  class = "card-header",
                  tags$strong("Height Metrics")
                ),
                div(
                  class = "card-body",
                  checkboxGroupInput(
                    ns("metrics_height"),
                    label = NULL,
                    choices = c(
                      "Max" = "Max",
                      "Min" = "Min",
                      "Median" = "Median"
                    ),
                    selected = c("Max")  # Default
                  )
                )
              )
            ),
            
            # Column 3
            div(
              class = "col-md-4",
              
              # Temperature Metrics
              div(
                class = "card mb-3",
                div(
                  class = "card-header",
                  tags$strong("Temperature Metrics")
                ),
                div(
                  class = "card-body",
                  checkboxGroupInput(
                    ns("metrics_temperature"),
                    label = NULL,
                    choices = c(
                      "TMax" = "TMax",
                      "TMin" = "TMin",
                      "TFM" = "TFM"
                    ),
                    selected = c("TMax")  # Default
                  )
                )
              ),
              
              # Info box
              div(
                class = "alert alert-info",
                icon("info-circle"), " ",
                tags$strong("Tip:"), " Click 'Calculate Preview' to see metric values"
              )
            )
          )
        )
      ),
      
      # Report Preview Card
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
      
      # Export Options Card
      div(
        class = "card mb-3",
        div(
          class = "card-header bg-warning",
          icon("file-export"), " Export Report"
        ),
        div(
          class = "card-body",
          
          # Report naming
          div(
            class = "row mb-3",
            div(
              class = "col-md-8",
              textInput(
                ns("report_name"),
                "Report Name:",
                value = "",
                placeholder = "e.g., Q4_Analysis_2025"
              ),
              div(
                class = "form-text",
                "Leave blank for automatic naming with timestamp"
              )
            )
          ),
          
          # Export buttons
          div(
            class = "row",
            div(
              class = "col-md-12",
              actionButton(
                ns("export_csv"),
                "Generate CSV Report",
                icon = icon("file-csv"),
                class = "btn-primary me-2"
              ),
              actionButton(
                ns("export_excel"),
                "Generate Excel Report",
                icon = icon("file-excel"),
                class = "btn-success"
              )
            )
          ),
          
          # Export status
          div(
            class = "mt-3",
            uiOutput(ns("export_status_display"))
          )
        )
      )
    )
  )
}

#' Report Builder Server
#'
#' @param id Module namespace ID
#' @param app_data Reactive values object containing application data
#'
#' @return Server logic (no return value)
mod_report_builder_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Reactive Values ----
    
    # Store calculated metrics
    calculated_metrics <- reactiveVal(NULL)
    
    # Track if calculation is in progress
    calculating <- reactiveVal(FALSE)
    
    # ---- Dataset Reload Observer (FIX FOR DATASET SWITCHING BUG) ----
    
    observeEvent(app_data$dataset_load_trigger, {
      
      req(app_data$dataset_load_trigger > 0)
      
      cat(sprintf("\n[REPORT_BUILDER] Dataset reload triggered (trigger=%d)\n", 
                  app_data$dataset_load_trigger))
      cat(sprintf("[REPORT_BUILDER] Loading dataset: %s\n", 
                  app_data$current_dataset_name))
      
      # Clear any previous calculations
      calculated_metrics(NULL)
      
      # Log successful reload (no notification - only Review Endpoints shows one)
      if (!is.null(app_data$processed_data)) {
        n_samples <- length(app_data$processed_data$samples)
        cat(sprintf("[REPORT_BUILDER] Dataset loaded successfully: %d samples\n", n_samples))
      } else {
        cat("[REPORT_BUILDER] WARNING: No processed data available\n")
      }
      
    }, ignoreInit = TRUE, priority = 100)
    
    # ---- Dataset Info Display ----
    
    output$dataset_info_display <- renderUI({
      
      if (is.null(app_data$processed_data)) {
        return(
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"), " ",
            "No processed data available. Please process data in the Data Overview tab."
          )
        )
      }
      
      data <- app_data$processed_data
      n_samples <- length(data$samples)
      
      # Get dataset name from app_data (now properly set)
      dataset_name <- if (!is.null(app_data$current_dataset_name)) {
        app_data$current_dataset_name
      } else {
        "Unknown Dataset"
      }
      
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
    
    # ---- Metric Selection Helpers ----
    
    # Get currently selected metrics from all categories
    selected_metrics <- reactive({
      metrics <- c(
        input$metrics_peak,
        input$metrics_transition,
        input$metrics_area,
        input$metrics_shape,
        input$metrics_height,
        input$metrics_temperature
      )
      unique(metrics)
    })
    
    # ---- Button Handlers ----
    
    # Select All
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
      
      showNotification("All metrics selected", type = "message")
    })
    
    # Clear All
    observeEvent(input$clear_all_metrics, {
      updateCheckboxGroupInput(session, "metrics_peak", selected = character(0))
      updateCheckboxGroupInput(session, "metrics_transition", selected = character(0))
      updateCheckboxGroupInput(session, "metrics_area", selected = character(0))
      updateCheckboxGroupInput(session, "metrics_shape", selected = character(0))
      updateCheckboxGroupInput(session, "metrics_height", selected = character(0))
      updateCheckboxGroupInput(session, "metrics_temperature", selected = character(0))
      
      # Clear calculated metrics
      calculated_metrics(NULL)
      
      showNotification("All metrics cleared", type = "message")
    })
    
    # Reset to Defaults
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
      
      showNotification("Reset to default metrics", type = "message")
    })
    
    # Calculate Preview
    observeEvent(input$calculate_preview, {
      
      # Check if data is loaded
      if (is.null(app_data$processed_data)) {
        showNotification(
          "No processed data available. Please load data first.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      # Check if metrics are selected
      metrics <- selected_metrics()
      if (length(metrics) == 0) {
        showNotification(
          "Please select at least one metric to calculate.",
          type = "warning",
          duration = 3
        )
        return()
      }
      
      # Set calculating flag
      calculating(TRUE)
      
      showNotification(
        sprintf("Calculating %d metrics for %d samples...", 
                length(metrics),
                length(app_data$processed_data$samples)),
        type = "message",
        duration = 3
      )
      
      # Calculate metrics
      result <- tryCatch({
        calculate_tlbparam_metrics(
          processed_data = app_data$processed_data,
          selected_metrics = metrics
        )
      }, error = function(e) {
        showNotification(
          sprintf("Error calculating metrics: %s", e$message),
          type = "error",
          duration = 10
        )
        return(NULL)
      })
      
      calculating(FALSE)
      
      if (!is.null(result)) {
        calculated_metrics(result)
        showNotification(
          sprintf("Successfully calculated %d metrics for %d samples", 
                  ncol(result) - 1,
                  nrow(result)),
          type = "message",
          duration = 3
        )
      }
    })
    
    # ---- Report Preview ----
    
    output$report_preview_display <- renderUI({
      
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
      
      # Check if metrics are selected
      metrics <- selected_metrics()
      if (length(metrics) == 0) {
        return(
          div(
            class = "alert alert-info",
            icon("info-circle"), " ",
            "Select metrics above and click 'Calculate Preview' to see results."
          )
        )
      }
      
      # Check if metrics have been calculated
      results <- calculated_metrics()
      if (is.null(results)) {
        return(
          div(
            class = "alert alert-info",
            icon("calculator"), " ",
            tags$strong(length(metrics)), " metrics selected. ",
            "Click 'Calculate Preview' to compute values."
          )
        )
      }
      
      # Show the preview table
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
    output$metrics_preview_table <- DT::renderDataTable({
      req(calculated_metrics())
      
      results <- calculated_metrics()
      
      DT::datatable(
        results,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'ftp',
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
    
    # ---- Export Handlers ----
    
    output$export_status_display <- renderUI({
      # Placeholder for export status messages
      NULL
    })
    
    # CSV Export
    observeEvent(input$export_csv, {
      showNotification(
        "CSV export will be implemented in Session 3",
        type = "message",
        duration = 3
      )
    })
    
    # Excel Export
    observeEvent(input$export_excel, {
      showNotification(
        "Excel export will be implemented in Session 3",
        type = "message",
        duration = 3
      )
    })
    
    # ---- Return nothing ----
    NULL
    
  })
}