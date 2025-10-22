# ==============================================================================
# Report Builder Module - COMPLETE Production Version
# ==============================================================================
#
# PURPOSE:
#   Generate comprehensive thermogram metric reports using tlbparam package
#   for thermal liquid biopsy (TLB) analysis
#
# FEATURES:
#   - Interactive metric selection organized by category (6 categories, 24 metrics)
#   - Real-time metric calculation using tlbparam::clean_thermograms()
#   - Preview table showing calculated metrics with DT DataTables
#   - Export to CSV and Excel formats (both download and save-to-disk)
#   - Automatic exclusion of samples marked with excluded=TRUE
#   - Data hash tracking to detect when regeneration needed
#   - Tooltips explaining each metric on hover
#   - Responsive layout with sticky metric selection
#   - Clear distinction between download (browser) and save (reports/ folder)
#
# DATA FLOW:
#   1. User selects metrics from organized categories
#   2. Click "Generate Report" to calculate using tlbparam
#   3. Excluded samples automatically filtered out before calculation
#   4. Preview table displays results (non-excluded samples only)
#   5. Export to CSV/Excel (saves to reports/ directory for tracking)
#   6. Direct download option also available (custom location)
#
# DEPENDENCIES:
#   - tlbparam package: Metric calculations
#     Install: remotes::install_github('BuscagliaR/tlbparam')
#   - digest package: MD5 hashing for change detection
#   - app_data$processed_data: Must contain baseline-subtracted thermogram data
#   - processing_utils.R: calculate_tlbparam_metrics(), export functions
#
# KEY FUNCTIONS:
#   - mod_report_builder_ui(): Creates user interface with metric selection
#   - mod_report_builder_server(): Handles calculations and exports
#   - calculate_data_hash(): Detects data changes requiring regeneration
#   - get_selected_metrics(): Collects user metric selections
#
# AUTHOR: Chris Reger
# LAST UPDATED: October 18, 2025
# ==============================================================================

library(digest)  # For data hashing

# ------------------------------------------------------------------------------
# UI Function
# ------------------------------------------------------------------------------
mod_report_builder_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container-fluid p-3",
      
      # ========================================================================
      # Dataset Information Card (Compact)
      # ========================================================================
      div(
        class = "card mb-3",
        div(
          class = "card-header bg-primary text-white",
          icon("database"), " Dataset Information"
        ),
        div(
          class = "card-body py-2",
          uiOutput(ns("dataset_info_display"))
        )
      ),
      
      # ========================================================================
      # Main Layout: Side-by-Side Columns
      # ========================================================================
      div(
        class = "row",
        
        # LEFT COLUMN: Metric Selection (40% width)
        # ======================================================================
        div(
          class = "col-md-5",
          
          div(
            class = "card mb-3 sticky-top",
            style = "top: 70px;",  # Account for navbar height
            
            div(
              class = "card-header bg-info text-white d-flex justify-content-between align-items-center",
              div(icon("list-check"), " Select Metrics"),
              div(
                class = "btn-group btn-group-sm",
                actionButton(
                  ns("select_all_metrics"),
                  "All",
                  class = "btn-light btn-sm"
                ),
                actionButton(
                  ns("clear_all_metrics"),
                  "None",
                  class = "btn-light btn-sm"
                ),
                actionButton(
                  ns("reset_to_defaults"),
                  "Defaults",
                  class = "btn-outline-light btn-sm"
                )
              )
            ),
            
            div(
              class = "card-body",
              style = "max-height: calc(100vh - 200px); overflow-y: auto;",
              
              # Info message
              div(
                class = "alert alert-sm alert-info py-2 px-3 mb-3",
                style = "font-size: 0.85rem;",
                icon("info-circle"), " ",
                "Select metrics to calculate for ", strong("non-excluded"), " samples."
              ),
              
              # Dynamic metric categories
              uiOutput(ns("metric_selection_ui"))
            )
          )
        ),
        
        # RIGHT COLUMN: Generation & Preview (60% width)
        # ======================================================================
        div(
          class = "col-md-7",
          
          # Generation Card
          div(
            class = "card mb-3",
            div(
              class = "card-header bg-success text-white",
              icon("calculator"), " Generate Report"
            ),
            div(
              class = "card-body",
              
              # Status message (regeneration needed?)
              uiOutput(ns("regeneration_status")),
              
              # Generate button
              actionButton(
                ns("calculate_metrics"),
                textOutput(ns("generate_button_text"), inline = TRUE),
                icon = icon("play"),
                class = "btn-success btn-lg w-100"
              )
            )
          ),
          
          # Preview Card (conditional)
          uiOutput(ns("report_preview_card"))
        )
      )
    )
  )
}


# ------------------------------------------------------------------------------
# Server Function
# ------------------------------------------------------------------------------
mod_report_builder_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ==========================================================================
    # Reactive Values
    # ==========================================================================
    
    # Calculated metrics data frame
    calculated_metrics <- reactiveVal(NULL)
    
    # Hash of data when metrics were calculated
    data_hash_at_calculation <- reactiveVal(NULL)
    
    # Calculation in progress flag
    calculating <- reactiveVal(FALSE)
    
    # ==========================================================================
    # Helper Functions
    # ==========================================================================
    
    # Calculate hash of current processed data state
    calculate_data_hash <- function() {
      req(app_data$processed_data)
      
      hash_data <- lapply(app_data$processed_data$samples, function(s) {
        list(
          lower = s$lower_endpoint,
          upper = s$upper_endpoint,
          excluded = s$excluded,
          manual = s$manual_adjustment
        )
      })
      
      digest::digest(hash_data, algo = "md5")
    }
    
    # Check if data has changed since last calculation
    data_has_changed <- reactive({
      if (is.null(calculated_metrics())) return(FALSE)
      if (is.null(data_hash_at_calculation())) return(TRUE)
      
      current_hash <- calculate_data_hash()
      !identical(current_hash, data_hash_at_calculation())
    })
    
    # Get list of selected metric keys from checkboxes
    get_selected_metrics <- function() {
      all_keys <- c(
        # Peak Heights
        "peak_1", "peak_2", "peak_3", "peak_f",
        # Peak Temperatures
        "tpeak_1", "tpeak_2", "tpeak_3", "tpeak_f",
        # Peak Ratios
        "peak1_peak2_ratio", "peak1_peak3_ratio", "peak2_peak3_ratio",
        # Valleys
        "v12", "tv12",
        # Valley Ratios
        "v12_peak1_ratio", "v12_peak2_ratio", "v12_peak3_ratio",
        # Global Metrics
        "max_dcp", "tmax", "tfm", "width", "area",
        # Additional
        "min_dcp", "tmin", "median_dcp"
      )
      
      Filter(function(key) {
        isTRUE(input[[paste0("metric_", key)]])
      }, all_keys)
    }
    
    # ==========================================================================
    # Dataset Reload Observer
    # ==========================================================================
    
    observeEvent(app_data$dataset_load_trigger, {
      req(app_data$dataset_load_trigger > 0)
      
      cat(sprintf("\n[REPORT_BUILDER] Dataset reload triggered\n"))
      
      calculated_metrics(NULL)
      data_hash_at_calculation(NULL)
      
      if (!is.null(app_data$processed_data)) {
        n_total <- length(app_data$processed_data$samples)
        n_excluded <- sum(sapply(app_data$processed_data$samples, function(s) isTRUE(s$excluded)))
        cat(sprintf("[REPORT_BUILDER] Dataset loaded: %d total, %d excluded\n", n_total, n_excluded))
      }
      
    }, ignoreInit = TRUE, priority = 100)
    
    # ==========================================================================
    # Dataset Information Display
    # ==========================================================================
    
    output$dataset_info_display <- renderUI({
      
      if (is.null(app_data$processed_data)) {
        return(
          p(
            class = "text-muted mb-0",
            icon("exclamation-triangle"), " No data loaded"
          )
        )
      }
      
      dataset_name <- app_data$current_dataset_name %||% "Unknown"
      samples <- app_data$processed_data$samples
      n_total <- length(samples)
      n_excluded <- sum(sapply(samples, function(s) isTRUE(s$excluded)))
      n_included <- n_total - n_excluded
      
      tagList(
        div(
          class = "row small",
          div(
            class = "col-6",
            strong("Dataset:"), " ", dataset_name
          ),
          div(
            class = "col-6 text-end",
            strong("Total:"), " ",
            tags$span(class = "badge bg-secondary", n_total)
          )
        ),
        div(
          class = "row small mt-1",
          div(
            class = "col-6",
            strong("Included:"), " ",
            tags$span(class = "badge bg-success", n_included)
          ),
          div(
            class = "col-6 text-end",
            strong("Excluded:"), " ",
            tags$span(
              class = sprintf("badge bg-%s", if(n_excluded > 0) "warning" else "secondary"),
              n_excluded
            )
          )
        )
      )
    })
    
    # ==========================================================================
    # Regeneration Status Message
    # ==========================================================================
    
    output$regeneration_status <- renderUI({
      
      if (is.null(calculated_metrics())) {
        return(NULL)
      }
      
      if (data_has_changed()) {
        div(
          class = "alert alert-warning py-2 px-3 mb-3",
          style = "font-size: 0.9rem;",
          icon("exclamation-triangle"), " ",
          strong("Data has changed!"), " ",
          "Endpoints or exclusions have been modified. ",
          "Please regenerate the report to reflect current data."
        )
      } else {
        div(
          class = "alert alert-success py-2 px-3 mb-3",
          style = "font-size: 0.9rem;",
          icon("check-circle"), " ",
          "Report is up-to-date with current data."
        )
      }
    })
    
    # ==========================================================================
    # Dynamic Button Text
    # ==========================================================================
    
    output$generate_button_text <- renderText({
      if (is.null(calculated_metrics())) {
        "Generate Report"
      } else {
        "Regenerate Report"
      }
    })
    
    # ==========================================================================
    # Metric Selection UI with Complete tlbparam Metrics
    # ==========================================================================
    
    output$metric_selection_ui <- renderUI({
      
      # Complete metric definitions matching tlbparam output
      metric_categories <- list(
        
        "Peak Heights" = list(
          list(
            key = "peak_1",
            label = "Peak 1",
            tooltip = "Height of peak in region 60-66°C",
            default = TRUE
          ),
          list(
            key = "peak_2",
            label = "Peak 2",
            tooltip = "Height of peak in region 67-73°C",
            default = TRUE
          ),
          list(
            key = "peak_3",
            label = "Peak 3",
            tooltip = "Height of peak in region 73-81°C",
            default = TRUE
          ),
          list(
            key = "peak_f",
            label = "Peak F (Fibrinogen)",
            tooltip = "Height of peak in Fibrinogen region (47-60°C)",
            default = FALSE
          )
        ),
        
        "Peak Temperatures" = list(
          list(
            key = "tpeak_1",
            label = "T Peak 1",
            tooltip = "Temperature of Peak 1 (°C)",
            default = TRUE
          ),
          list(
            key = "tpeak_2",
            label = "T Peak 2",
            tooltip = "Temperature of Peak 2 (°C)",
            default = TRUE
          ),
          list(
            key = "tpeak_3",
            label = "T Peak 3",
            tooltip = "Temperature of Peak 3 (°C)",
            default = TRUE
          ),
          list(
            key = "tpeak_f",
            label = "T Peak F (Fibrinogen)",
            tooltip = "Temperature of Fibrinogen peak (°C)",
            default = FALSE
          )
        ),
        
        "Peak Ratios" = list(
          list(
            key = "peak1_peak2_ratio",
            label = "Peak 1 / Peak 2",
            tooltip = "Ratio of Peak 1 to Peak 2",
            default = FALSE
          ),
          list(
            key = "peak1_peak3_ratio",
            label = "Peak 1 / Peak 3",
            tooltip = "Ratio of Peak 1 to Peak 3",
            default = FALSE
          ),
          list(
            key = "peak2_peak3_ratio",
            label = "Peak 2 / Peak 3",
            tooltip = "Ratio of Peak 2 to Peak 3",
            default = FALSE
          )
        ),
        
        "Valley Metrics" = list(
          list(
            key = "v12",
            label = "V1.2",
            tooltip = "Valley (minimum) between Peak 1 and Peak 2",
            default = FALSE
          ),
          list(
            key = "tv12",
            label = "TV1.2",
            tooltip = "Temperature of valley between Peak 1 and Peak 2 (°C)",
            default = FALSE
          ),
          list(
            key = "v12_peak1_ratio",
            label = "V1.2 / Peak 1",
            tooltip = "Ratio of V1.2 to Peak 1 amplitude",
            default = FALSE
          ),
          list(
            key = "v12_peak2_ratio",
            label = "V1.2 / Peak 2",
            tooltip = "Ratio of V1.2 to Peak 2 amplitude",
            default = FALSE
          ),
          list(
            key = "v12_peak3_ratio",
            label = "V1.2 / Peak 3",
            tooltip = "Ratio of V1.2 to Peak 3 amplitude",
            default = FALSE
          )
        ),
        
        "Global Metrics (Primary)" = list(
          list(
            key = "max_dcp",
            label = "Max",
            tooltip = "Maximum observed excess heat capacity",
            default = TRUE
          ),
          list(
            key = "tmax",
            label = "TMax",
            tooltip = "Temperature at maximum height (°C)",
            default = TRUE
          ),
          list(
            key = "tfm",
            label = "TFM",
            tooltip = "Temperature of first moment (°C)",
            default = FALSE
          ),
          list(
            key = "width",
            label = "Width",
            tooltip = "Full width at half maximum",
            default = TRUE
          ),
          list(
            key = "area",
            label = "Area",
            tooltip = "Total area under thermogram signature",
            default = TRUE
          )
        ),
        
        "Additional Metrics" = list(
          list(
            key = "min_dcp",
            label = "Min",
            tooltip = "Minimum observed excess heat capacity",
            default = FALSE
          ),
          list(
            key = "tmin",
            label = "TMin",
            tooltip = "Temperature at minimum (°C)",
            default = FALSE
          ),
          list(
            key = "median_dcp",
            label = "Median",
            tooltip = "Median observed excess heat capacity",
            default = FALSE
          )
        )
      )
      
      # Create compact cards for each category
      category_uis <- lapply(names(metric_categories), function(category_name) {
        
        metrics <- metric_categories[[category_name]]
        category_id <- gsub("[^A-Za-z0-9]", "_", tolower(category_name))
        
        div(
          class = "card mb-2",
          div(
            class = "card-header py-1 px-2 bg-light d-flex justify-content-between align-items-center",
            style = "font-size: 0.9rem;",
            strong(category_name),
            div(
              class = "btn-group btn-group-sm",
              actionButton(
                ns(paste0("select_all_", category_id)),
                "All",
                class = "btn-outline-success btn-sm py-0 px-2",
                style = "font-size: 0.75rem;"
              ),
              actionButton(
                ns(paste0("clear_all_", category_id)),
                "None",
                class = "btn-outline-secondary btn-sm py-0 px-2",
                style = "font-size: 0.75rem;"
              )
            )
          ),
          div(
            class = "card-body py-2 px-3",
            
            lapply(metrics, function(m) {
              div(
                class = "form-check",
                style = "font-size: 0.85rem;",
                tags$input(
                  type = "checkbox",
                  class = "form-check-input",
                  id = ns(paste0("metric_", m$key)),
                  value = m$key,
                  checked = if(m$default) "checked" else NULL
                ),
                tags$label(
                  class = "form-check-label",
                  `for` = ns(paste0("metric_", m$key)),
                  `data-bs-toggle` = "tooltip",
                  `data-bs-placement` = "right",
                  title = m$tooltip,
                  m$label
                )
              )
            })
          )
        )
      })
      
      tagList(
        category_uis,
        tags$script(HTML("
          $(function () {
            $('[data-bs-toggle=\"tooltip\"]').tooltip();
          });
        "))
      )
    })
    
    # ==========================================================================
    # Category Control Button Observers
    # ==========================================================================
    
    # Peak Heights
    observeEvent(input$select_all_peak_heights, {
      lapply(c("peak_1", "peak_2", "peak_3", "peak_f"), function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', true);", ns(paste0("metric_", key))))
      })
    })
    
    observeEvent(input$clear_all_peak_heights, {
      lapply(c("peak_1", "peak_2", "peak_3", "peak_f"), function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', false);", ns(paste0("metric_", key))))
      })
    })
    
    # Peak Temperatures
    observeEvent(input$select_all_peak_temperatures, {
      lapply(c("tpeak_1", "tpeak_2", "tpeak_3", "tpeak_f"), function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', true);", ns(paste0("metric_", key))))
      })
    })
    
    observeEvent(input$clear_all_peak_temperatures, {
      lapply(c("tpeak_1", "tpeak_2", "tpeak_3", "tpeak_f"), function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', false);", ns(paste0("metric_", key))))
      })
    })
    
    # Peak Ratios
    observeEvent(input$select_all_peak_ratios, {
      lapply(c("peak1_peak2_ratio", "peak1_peak3_ratio", "peak2_peak3_ratio"), function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', true);", ns(paste0("metric_", key))))
      })
    })
    
    observeEvent(input$clear_all_peak_ratios, {
      lapply(c("peak1_peak2_ratio", "peak1_peak3_ratio", "peak2_peak3_ratio"), function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', false);", ns(paste0("metric_", key))))
      })
    })
    
    # Valley Metrics
    observeEvent(input$select_all_valley_metrics, {
      lapply(c("v12", "tv12", "v12_peak1_ratio", "v12_peak2_ratio", "v12_peak3_ratio"), function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', true);", ns(paste0("metric_", key))))
      })
    })
    
    observeEvent(input$clear_all_valley_metrics, {
      lapply(c("v12", "tv12", "v12_peak1_ratio", "v12_peak2_ratio", "v12_peak3_ratio"), function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', false);", ns(paste0("metric_", key))))
      })
    })
    
    # Global Metrics (Primary)
    observeEvent(input$select_all_global_metrics_primary, {
      lapply(c("max_dcp", "tmax", "tfm", "width", "area"), function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', true);", ns(paste0("metric_", key))))
      })
    })
    
    observeEvent(input$clear_all_global_metrics_primary, {
      lapply(c("max_dcp", "tmax", "tfm", "width", "area"), function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', false);", ns(paste0("metric_", key))))
      })
    })
    
    # Additional Metrics
    observeEvent(input$select_all_additional_metrics, {
      lapply(c("min_dcp", "tmin", "median_dcp"), function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', true);", ns(paste0("metric_", key))))
      })
    })
    
    observeEvent(input$clear_all_additional_metrics, {
      lapply(c("min_dcp", "tmin", "median_dcp"), function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', false);", ns(paste0("metric_", key))))
      })
    })
    
    # ==========================================================================
    # Global Control Buttons
    # ==========================================================================
    
    observeEvent(input$select_all_metrics, {
      all_keys <- c(
        "peak_1", "peak_2", "peak_3", "peak_f",
        "tpeak_1", "tpeak_2", "tpeak_3", "tpeak_f",
        "peak1_peak2_ratio", "peak1_peak3_ratio", "peak2_peak3_ratio",
        "v12", "tv12", "v12_peak1_ratio", "v12_peak2_ratio", "v12_peak3_ratio",
        "max_dcp", "tmax", "tfm", "width", "area",
        "min_dcp", "tmin", "median_dcp"
      )
      
      lapply(all_keys, function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', true);", ns(paste0("metric_", key))))
      })
    })
    
    observeEvent(input$clear_all_metrics, {
      all_keys <- c(
        "peak_1", "peak_2", "peak_3", "peak_f",
        "tpeak_1", "tpeak_2", "tpeak_3", "tpeak_f",
        "peak1_peak2_ratio", "peak1_peak3_ratio", "peak2_peak3_ratio",
        "v12", "tv12", "v12_peak1_ratio", "v12_peak2_ratio", "v12_peak3_ratio",
        "max_dcp", "tmax", "tfm", "width", "area",
        "min_dcp", "tmin", "median_dcp"
      )
      
      lapply(all_keys, function(key) {
        shinyjs::runjs(sprintf("$('#%s').prop('checked', false);", ns(paste0("metric_", key))))
      })
    })
    
    observeEvent(input$reset_to_defaults, {
      # Defaults: Peak 1-3, TPeak 1-3, Max, TMax, Width, Area
      default_keys <- c("peak_1", "peak_2", "peak_3", "tpeak_1", "tpeak_2", "tpeak_3", 
                        "max_dcp", "tmax", "width", "area")
      all_keys <- c(
        "peak_1", "peak_2", "peak_3", "peak_f",
        "tpeak_1", "tpeak_2", "tpeak_3", "tpeak_f",
        "peak1_peak2_ratio", "peak1_peak3_ratio", "peak2_peak3_ratio",
        "v12", "tv12", "v12_peak1_ratio", "v12_peak2_ratio", "v12_peak3_ratio",
        "max_dcp", "tmax", "tfm", "width", "area",
        "min_dcp", "tmin", "median_dcp"
      )
      
      lapply(all_keys, function(key) {
        checked <- if(key %in% default_keys) "true" else "false"
        shinyjs::runjs(sprintf("$('#%s').prop('checked', %s);", ns(paste0("metric_", key)), checked))
      })
    })
    
    # ==========================================================================
    # Calculate Metrics Handler - Using tlbparam
    # ==========================================================================
    
    observeEvent(input$calculate_metrics, {
      
      cat("\n[REPORT_BUILDER] Calculate metrics triggered\n")
      
      if (calculating()) return()
      if (is.null(app_data$processed_data)) {
        showNotification("No data loaded", type = "error", duration = 5)
        return()
      }
      
      # Get selected metrics
      selected_metrics <- get_selected_metrics()
      
      if (length(selected_metrics) == 0) {
        showNotification("Please select at least one metric", type = "warning", duration = 5)
        return()
      }
      
      cat(sprintf("[REPORT_BUILDER] Selected %d metrics: %s\n", 
                  length(selected_metrics), 
                  paste(selected_metrics, collapse = ", ")))
      
      calculating(TRUE)
      showNotification(id = "calc_progress", "Calculating metrics using tlbparam...", duration = NULL, type = "message")
      
      # Filter excluded samples BEFORE calling tlbparam
      all_samples <- app_data$processed_data$samples
      included_samples <- Filter(function(s) !isTRUE(s$excluded), all_samples)
      
      n_total <- length(all_samples)
      n_included <- length(included_samples)
      n_excluded <- n_total - n_included
      
      cat(sprintf("[REPORT_BUILDER] Filtering: %d total, %d included, %d excluded\n",
                  n_total, n_included, n_excluded))
      
      if (n_included == 0) {
        calculating(FALSE)
        removeNotification("calc_progress")
        showNotification("All samples excluded", type = "error", duration = 8)
        return()
      }
      
      # Create temporary processed_data with only included samples
      filtered_data <- list(
        samples = included_samples,
        summary = app_data$processed_data$summary
      )
      
      # Call tlbparam calculation function from processing_utils.R
      tryCatch({
        
        cat("[REPORT_BUILDER] Calling calculate_tlbparam_metrics()...\n")
        
        result <- calculate_tlbparam_metrics(
          processed_data = filtered_data,
          selected_metrics = selected_metrics
        )
        
        if (is.null(result)) {
          stop("tlbparam calculation returned NULL")
        }
        
        # Store results and hash
        calculated_metrics(result)
        data_hash_at_calculation(calculate_data_hash())
        
        cat(sprintf("[REPORT_BUILDER] ✅ Success: %d samples, %d metrics\n",
                    nrow(result), ncol(result) - 1))
        
        removeNotification("calc_progress")
        showNotification(
          sprintf("Metrics calculated! %d samples, %d metrics", n_included, ncol(result) - 1),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        cat(sprintf("[REPORT_BUILDER] ❌ Error: %s\n", e$message))
        removeNotification("calc_progress")
        showNotification(
          HTML(sprintf(
            "<strong>Calculation Error</strong><br/>%s<br/><br/>Please check console for details.",
            e$message
          )),
          type = "error",
          duration = 10
        )
        calculated_metrics(NULL)
      }, finally = {
        calculating(FALSE)
      })
    })
    
    # ==========================================================================
    # Preview Card
    # ==========================================================================
    
    output$report_preview_card <- renderUI({
      
      metrics <- calculated_metrics()
      if (is.null(metrics)) return(NULL)
      
      div(
        class = "card mb-3",
        div(
          class = "card-header bg-warning text-dark",
          icon("table"), " Report Preview"
        ),
        div(
          class = "card-body",
          
          p(
            class = "text-muted small",
            icon("info-circle"),
            sprintf(" Showing %d samples, %d metrics", nrow(metrics), ncol(metrics) - 1)
          ),
          
          DT::dataTableOutput(ns("metrics_preview_table")),
          
          hr(),
          
          # Export Section
          h5(class = "mb-3", icon("download"), " Export Options"),
          
          textInput(
            ns("report_name"),
            "Report Name (optional)",
            placeholder = "Auto-generated if left blank",
            width = "100%"
          ),
          
          div(
            class = "row",
            
            # Direct Download Column
            div(
              class = "col-md-6",
              h6(class = "text-muted", icon("arrow-down"), " Direct Download"),
              p(class = "small text-muted", "Download to your custom location"),
              downloadButton(
                ns("download_csv"),
                "Download CSV",
                class = "btn-primary w-100 mb-2",
                icon = icon("file-csv")
              ),
              downloadButton(
                ns("download_excel"),
                "Download Excel",
                class = "btn-success w-100",
                icon = icon("file-excel")
              )
            ),
            
            # Save to Disk Column
            div(
              class = "col-md-6",
              h6(class = "text-muted", icon("save"), " Save to Reports Folder"),
              p(class = "small text-muted", "Save to reports/ for tracking in Data Overview"),
              actionButton(
                ns("export_csv"),
                "Save CSV",
                class = "btn-outline-primary w-100 mb-2",
                icon = icon("floppy-disk")
              ),
              actionButton(
                ns("export_excel"),
                "Save Excel",
                class = "btn-outline-success w-100",
                icon = icon("floppy-disk")
              )
            )
          )
        )
      )
    })
    
    output$metrics_preview_table <- DT::renderDataTable({
      req(calculated_metrics())
      
      DT::datatable(
        calculated_metrics(),
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'frtip',
          order = list(list(0, 'asc'))
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })
    
    # ==========================================================================
    # Download Handlers (Direct Browser Download)
    # ==========================================================================
    
    output$download_csv <- downloadHandler(
      filename = function() {
        dataset_name <- tools::file_path_sans_ext(app_data$current_dataset_name %||% "Report")
        sprintf("%s_%s.csv", dataset_name, format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        req(calculated_metrics())
        readr::write_csv(calculated_metrics(), file)
        cat(sprintf("[REPORT_BUILDER] Direct CSV download: %s\n", basename(file)))
      }
    )
    
    output$download_excel <- downloadHandler(
      filename = function() {
        dataset_name <- tools::file_path_sans_ext(app_data$current_dataset_name %||% "Report")
        sprintf("%s_%s.xlsx", dataset_name, format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        req(calculated_metrics())
        
        metrics <- calculated_metrics()
        dataset_name <- app_data$current_dataset_name %||% "Unknown"
        metric_names <- setdiff(names(metrics), "Sample_ID")
        
        metadata_sheet <- data.frame(
          Property = c("Report Generated", "Source Dataset", "Samples", "Metrics", "Metrics Included"),
          Value = c(
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            dataset_name,
            as.character(nrow(metrics)),
            as.character(length(metric_names)),
            paste(metric_names, collapse = ", ")
          ),
          stringsAsFactors = FALSE
        )
        
        writexl::write_xlsx(
          list(Metrics = metrics, Metadata = metadata_sheet),
          path = file
        )
        
        cat(sprintf("[REPORT_BUILDER] Direct Excel download: %s\n", basename(file)))
      }
    )
    
    # ==========================================================================
    # Export Handlers (Save to reports/ folder)
    # ==========================================================================
    
    observeEvent(input$export_csv, {
      
      cat("\n[REPORT_BUILDER] CSV export (save to disk) triggered\n")
      
      metrics <- calculated_metrics()
      if (is.null(metrics) || nrow(metrics) == 0) {
        showNotification(
          "Please calculate metrics first",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      result <- export_report_csv(
        metrics_data = metrics,
        report_name = trimws(input$report_name),
        dataset_name = app_data$current_dataset_name %||% "Unknown"
      )
      
      if (result$success) {
        
        report_id <- paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        
        app_data$generated_reports[[report_id]] <- list(
          id = report_id,
          name = tools::file_path_sans_ext(result$filename),
          format = "csv",
          filepath = result$filepath,
          dataset_name = app_data$current_dataset_name %||% "Unknown",
          n_samples = nrow(metrics),
          n_metrics = ncol(metrics) - 1,
          generated_at = Sys.time()
        )
        
        cat(sprintf("[REPORT_BUILDER] Report tracked: %s\n", report_id))
        
        showNotification(
          HTML(sprintf(
            "<strong>CSV Saved!</strong><br/>File: %s<br/>Location: reports/<br/>View in Data Overview",
            result$filename
          )),
          type = "message",
          duration = 8
        )
        
        updateTextInput(session, "report_name", value = "")
        
      } else {
        showNotification(
          sprintf("Export failed: %s", result$message),
          type = "error",
          duration = 10
        )
      }
    })
    
    observeEvent(input$export_excel, {
      
      cat("\n[REPORT_BUILDER] Excel export (save to disk) triggered\n")
      
      metrics <- calculated_metrics()
      if (is.null(metrics) || nrow(metrics) == 0) {
        showNotification(
          "Please calculate metrics first",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      result <- export_report_excel(
        metrics_data = metrics,
        report_name = trimws(input$report_name),
        dataset_name = app_data$current_dataset_name %||% "Unknown"
      )
      
      if (result$success) {
        
        report_id <- paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        
        app_data$generated_reports[[report_id]] <- list(
          id = report_id,
          name = tools::file_path_sans_ext(result$filename),
          format = "xlsx",
          filepath = result$filepath,
          dataset_name = app_data$current_dataset_name %||% "Unknown",
          n_samples = nrow(metrics),
          n_metrics = ncol(metrics) - 1,
          generated_at = Sys.time()
        )
        
        cat(sprintf("[REPORT_BUILDER] Report tracked: %s\n", report_id))
        
        showNotification(
          HTML(sprintf(
            "<strong>Excel Saved!</strong><br/>File: %s<br/>Location: reports/<br/>View in Data Overview",
            result$filename
          )),
          type = "message",
          duration = 8
        )
        
        updateTextInput(session, "report_name", value = "")
        
      } else {
        showNotification(
          sprintf("Export failed: %s", result$message),
          type = "error",
          duration = 10
        )
      }
    })
    
  })
}