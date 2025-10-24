# ==============================================================================
# Data Overview Module - Professional Refactored Version
# ==============================================================================
# 
# PURPOSE:
#   Central hub for dataset management in ThermogramForge. Handles the complete
#   workflow from raw data upload through processing, saving, loading, and 
#   navigation to downstream analysis modules.
#
# FEATURES:
#   - Upload and validate raw thermogram data (CSV/Excel formats)
#   - Process data with automatic baseline detection
#   - Save processed datasets in multiple formats (RDS/CSV/Excel)
#   - Load previously saved datasets for continued analysis
#   - Multi-dataset session support (work with multiple files simultaneously)
#   - Navigate to Review Endpoints or Report Builder modules
#   - Track generated reports with search/filter capabilities
#   - Platform-aware folder opening for reports directory
#
# DATA FLOW:
#   1. User uploads raw data → validated and stored
#   2. User triggers processing → baseline detection applied
#   3. Processed data → available for review or report generation
#   4. User saves to disk → persisted in data/processed/ directory
#   5. User loads from disk → restored to session for continued work
#   6. Reports generated → tracked with metadata in session
#
# KEY REACTIVE VALUES:
#   - uploaded_datasets: List of all datasets in current session
#   - saved_files_df: Data frame of files in data/processed/ directory
#   - app_data$processed_data: Currently active dataset for analysis
#   - app_data$generated_reports: List of reports generated in session
#
# MODULE STRUCTURE:
#   - mod_data_overview_ui: User interface definition
#   - mod_data_overview_server: Server logic and reactivity
#
# DEPENDENCIES:
#   - utils/data_utils.R: File reading and validation
#   - utils/processing_utils.R: Baseline detection and file operations
#   - ThermogramBaseline package: Core analysis functions
#
# AUTHOR: Chris Reger
# LAST UPDATED: October 17, 2025 (Phase 8 - Polish Items 4 & 5)
# ==============================================================================


# ==============================================================================
# USER INTERFACE
# ==============================================================================

#' Data Overview Module UI
#'
#' Creates the user interface for the Data Overview tab, including upload
#' controls, dataset management, and report tracking.
#'
#' @param id Character string. Namespace ID for the module. Used to create
#'   unique IDs for all UI elements within this module.
#'
#' @return A tagList containing the complete UI structure for the Data Overview
#'   tab, including summary cards, dataset lists, and action buttons.
#'
#' @details
#' The UI is organized into several main sections:
#' \itemize{
#'   \item Summary Cards: Display counts of datasets and reports
#'   \item Upload Button: Primary action to add new data
#'   \item Unprocessed Datasets: Files awaiting baseline detection
#'   \item Processed Datasets: Files ready for review or reporting
#'   \item Saved Datasets: Files persisted to disk
#'   \item Generated Reports: Track reports created in this session
#' }
#'
#' @examples
#' \dontrun{
#' # In app.R or main UI definition:
#' mod_data_overview_ui("data_overview")
#' }
#'
#' @export
mod_data_overview_ui <- function(id) {
  
  # Create namespace function for this module
  ns <- NS(id)
  
  # Return complete UI structure
  tagList(
    
    # -------------------------------------------------------------------------
    # SECTION 1: Summary Statistics Cards
    # -------------------------------------------------------------------------
    # Three cards showing counts of raw, processed, and saved datasets
    div(
      class = "row mb-3",
      
      # Card 1: Raw (Unprocessed) Datasets
      div(
        class = "col-md-4",
        div(
          class = "card text-center",
          div(
            class = "card-body",
            h5(class = "card-title", icon("file-csv"), " Raw Datasets"),
            h2(class = "text-primary", textOutput(ns("raw_count")))
          )
        )
      ),
      
      # Card 2: Processed Datasets
      div(
        class = "col-md-4",
        div(
          class = "card text-center",
          div(
            class = "card-body",
            h5(class = "card-title", icon("check-circle"), " Processed Datasets"),
            h2(class = "text-success", textOutput(ns("processed_count")))
          )
        )
      ),
      
      # Card 3: Saved to Disk
      div(
        class = "col-md-4",
        div(
          class = "card text-center",
          div(
            class = "card-body",
            h5(class = "card-title", icon("save"), " Saved to Disk"),
            h2(class = "text-info", textOutput(ns("saved_count")))
          )
        )
      )
    ),
    
    # -------------------------------------------------------------------------
    # SECTION 2: Main Content Area
    # -------------------------------------------------------------------------
    div(
      class = "row",
      div(
        class = "col-12",
        
        # Upload Button (Primary Action)
        div(
          class = "mb-3",
          actionButton(
            ns("upload_btn"),
            "Upload New Data",
            icon = icon("upload"),
            class = "btn-primary btn-lg"
          )
        ),
        
        # ---------------------------------------------------------------------
        # SUBSECTION 2.1: Unprocessed Datasets Card
        # ---------------------------------------------------------------------
        # Shows recently uploaded files that haven't been processed yet
        div(
          class = "card mb-3",
          div(
            class = "card-header",
            icon("file-csv"), " Unprocessed Datasets"
          ),
          div(
            class = "card-body",
            p(
              class = "text-muted",
              icon("info-circle"),
              " Uploaded files awaiting processing. Click 'Process Data' to detect endpoints."
            ),
            uiOutput(ns("raw_files_ui"))
          )
        ),
        
        # ---------------------------------------------------------------------
        # SUBSECTION 2.2: Processed Datasets Card
        # ---------------------------------------------------------------------
        # Shows datasets with completed baseline detection
        div(
          class = "card mb-3",
          div(
            class = "card-header",
            icon("check-circle"), " Processed Datasets"
          ),
          div(
            class = "card-body",
            p(
              class = "text-muted",
              icon("info-circle"),
              " Datasets with detected endpoints. Review them or generate reports."
            ),
            uiOutput(ns("processed_files_ui"))
          )
        ),
        
        # ---------------------------------------------------------------------
        # SUBSECTION 2.3: Saved Datasets Card
        # ---------------------------------------------------------------------
        # Shows datasets persisted to disk (data/processed/ directory)
        div(
          class = "card mb-3",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            div(icon("folder-open"), " Saved Datasets"),
            actionButton(
              ns("refresh_saved"),
              "Refresh",
              icon = icon("sync"),
              class = "btn-sm btn-outline-secondary"
            )
          ),
          div(
            class = "card-body",
            p(
              class = "text-muted",
              icon("info-circle"),
              " Datasets saved to disk (data/processed/ directory). ",
              "RDS files contain full data for Review Endpoints. ",
              "CSV/Excel files are for report generation only."
            ),
            uiOutput(ns("saved_files_ui"))
          )
        ),
        
        # ---------------------------------------------------------------------
        # SUBSECTION 2.4: Generated Reports Card (with Search/Filter)
        # ---------------------------------------------------------------------
        # Shows reports created during this session with search functionality
        div(
          class = "card mt-3",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            div(icon("chart-line"), " Generated Reports"),
            actionButton(
              ns("open_reports_folder"),
              "Open Reports Folder",
              icon = icon("folder-open"),
              class = "btn-sm btn-outline-primary"
            )
          ),
          div(
            class = "card-body",
            
            # Information text
            p(
              class = "text-muted",
              icon("info-circle"),
              " Reports generated during this session. ",
              "Use the search box to filter reports. ",
              "Click 'Open Reports Folder' to view all report files."
            ),
            
            # Search/Filter Input (NEW - Item #5)
            div(
              class = "mb-3",
              textInput(
                ns("reports_search"),
                label = NULL,
                placeholder = "Search reports by name, dataset, or format...",
                width = "100%"
              )
            ),
            
            # Reports List (filtered based on search)
            uiOutput(ns("generated_reports_list"))
          )
        )
      )
    )
  )
}


# ==============================================================================
# SERVER LOGIC
# ==============================================================================

#' Data Overview Module Server
#'
#' Handles all server-side logic for the Data Overview module, including file
#' upload, data processing, saving/loading, and navigation.
#'
#' @param id Character string. Namespace ID matching the UI function.
#' @param app_data Reactive values object. Shared application state used for
#'   communication between modules. Must contain:
#'   \itemize{
#'     \item processed_data: Currently active dataset
#'     \item current_dataset_name: Name of active dataset
#'     \item generated_reports: List of report metadata
#'   }
#'
#' @return No return value (module server functions modify reactive values and
#'   produce side effects like UI updates and file operations).
#'
#' @details
#' This server function manages the complete dataset lifecycle:
#' \enumerate{
#'   \item File Upload: Validates and stores uploaded thermogram data
#'   \item Processing: Applies baseline detection using ThermogramBaseline
#'   \item Session Management: Tracks multiple datasets simultaneously
#'   \item Persistence: Saves/loads datasets to/from disk
#'   \item Navigation: Routes users to Review or Report Builder modules
#'   \item Reporting: Tracks generated reports with metadata
#' }
#'
#' The function creates dynamic observers for dataset-specific actions, ensuring
#' each dataset can be independently processed, saved, reviewed, or reported.
#'
#' @examples
#' \dontrun{
#' # In app.R or main server function:
#' mod_data_overview_server("data_overview", app_data)
#' }
#'
#' @export
mod_data_overview_server <- function(id, app_data) {
  
  moduleServer(id, function(input, output, session) {
    
    # Get namespace function
    ns <- session$ns
    
    # =========================================================================
    # REACTIVE DATA STRUCTURES
    # =========================================================================
    
    # Primary dataset storage
    # Each dataset is a list containing:
    #   - id: Unique identifier (e.g., "dataset_1")
    #   - file_name: Original filename
    #   - data: Raw data frame
    #   - format_info: Metadata about file format
    #   - upload_time: POSIXct timestamp
    #   - status: "unprocessed", "processed", or "loaded"
    #   - processed_data: Results from baseline detection (if processed)
    #   - temp_params: Temperature and detection parameters
    uploaded_datasets <- reactiveVal(list())
    
    # Counter for generating unique dataset IDs
    dataset_counter <- reactiveVal(0)
    
    # Tracker for which observers have been created (prevents duplicates)
    created_observers_unprocessed <- reactiveVal(character(0))
    created_observers_processed <- reactiveVal(character(0))
    
    # Trigger for refreshing saved files list
    saved_files_trigger <- reactiveVal(0)
    
    # Reactive data frame of files in data/processed/ directory
    saved_files_df <- reactive({
      saved_files_trigger()  # Depend on trigger
      list_processed_datasets()  # Call utility function
    })
    
    ui_refresh_trigger <- reactiveVal(0)
    
    # =========================================================================
    # SUMMARY COUNTS (Output: Summary Cards)
    # =========================================================================
    
    # Count of unprocessed datasets
    output$raw_count <- renderText({
      datasets <- uploaded_datasets()
      
      if (length(datasets) == 0) return("0")
      
      count <- length(Filter(function(d) d$status == "unprocessed", datasets))
      as.character(count)
    })
    
    # Count of processed datasets (including loaded from disk)
    output$processed_count <- renderText({
      datasets <- uploaded_datasets()
      
      if (length(datasets) == 0) return("0")
      
      count <- length(Filter(function(d) d$status %in% c("processed", "loaded"), datasets))
      as.character(count)
    })
    
    # Count of datasets saved to disk
    output$saved_count <- renderText({
      df <- saved_files_df()
      as.character(nrow(df))
    })
    
    # =========================================================================
    # FILE UPLOAD: Modal Dialog and Processing
    # =========================================================================
    
    # Show upload modal when upload button is clicked
    observeEvent(input$upload_btn, {
      showModal(
        modalDialog(
          title = tagList(icon("upload"), " Upload Raw Thermogram Data"),
          size = "l",
          
          # File input and temperature filtering in SAME ROW
          fluidRow(
            column(
              6,
              fileInput(
                ns("file_upload"),
                "Choose CSV or Excel file",
                accept = c(".csv", ".xlsx", ".xls"),
                multiple = FALSE
              )
            ),
            column(
              3,
              numericInput(
                ns("temp_min"),
                "Min Temp (°C)",
                value = 20,
                min = 0,
                max = 100
              )
            ),
            column(
              3,
              numericInput(
                ns("temp_max"),
                "Max Temp (°C)",
                value = 110,
                min = 0,
                max = 150
              )
            )
          ),
          
          # DATA PREVIEW SECTION 
          hr(),
          h5(icon("table"), " Data Preview"),
          div(
            id = ns("upload_preview_container"),
            style = "min-height: 150px;",
            em("Upload a file to see preview of first 5 rows...")
          ),
          
          hr(),
          
          # Advanced options (collapsible)
          h5(
            icon("cog"), " Advanced Options",
            actionButton(
              ns("toggle_advanced"),
              "Show/Hide",
              class = "btn-sm btn-outline-secondary ms-2"
            )
          ),
          
          shinyjs::hidden(
            div(
              id = ns("advanced_options"),
              
              h6(class = "text-muted mb-3", "Baseline Detection Parameters"),
              
              fluidRow(
                column(
                  6,
                  numericInput(
                    ns("window_size"),
                    "Window Size",
                    value = 90,
                    min = 10,
                    max = 200,
                    step = 10
                  ),
                  tags$small(class = "form-text text-muted",
                             "Points for rolling variance calculation")
                ),
                column(
                  6,
                  selectInput(
                    ns("point_selection"),
                    "Endpoint Selection",
                    choices = c(
                      "Innermost" = "innermost",
                      "Outmost" = "outmost",
                      "Middle" = "middle"
                    ),
                    selected = "innermost"
                  ),
                  tags$small(class = "form-text text-muted",
                             "Strategy for selecting endpoints")
                )
              ),
              
              fluidRow(
                column(
                  6,
                  numericInput(
                    ns("exclusion_lower"),
                    "Exclusion Lower (°C)",
                    value = 60,
                    min = 40,
                    max = 80
                  ),
                  tags$small(class = "form-text text-muted",
                             "Lower bound of transition region")
                ),
                column(
                  6,
                  numericInput(
                    ns("exclusion_upper"),
                    "Exclusion Upper (°C)",
                    value = 80,
                    min = 60,
                    max = 95
                  ),
                  tags$small(class = "form-text text-muted",
                             "Upper bound of transition region")
                )
              ),
              
              fluidRow(
                column(
                  6,
                  numericInput(
                    ns("grid_resolution"),
                    "Grid Resolution (°C)",
                    value = 0.1,
                    min = 0.01,
                    max = 1.0,
                    step = 0.05
                  ),
                  tags$small(class = "form-text text-muted",
                             "Temperature step for output grid")
                )
              )
            )
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_upload"),
              "Upload Data",
              class = "btn-primary",
              icon = icon("check")
            )
          ),
          easyClose = FALSE
        )
      )
    })
    
    # Show data preview when file is selected
    observeEvent(input$file_upload, {
      req(input$file_upload)
      
      # Try to read and preview the file
      preview_result <- tryCatch({
        file_info <- input$file_upload
        
        # Determine file type and read
        if (grepl("\\.csv$", file_info$name, ignore.case = TRUE)) {
          data <- readr::read_csv(file_info$datapath, show_col_types = FALSE)
        } else if (grepl("\\.(xlsx|xls)$", file_info$name, ignore.case = TRUE)) {
          data <- readxl::read_excel(file_info$datapath)
        } else {
          return(list(success = FALSE, message = "Unsupported file type"))
        }
        
        # Get first 5 rows for preview
        preview_data <- head(data, 5)
        
        list(
          success = TRUE, 
          data = preview_data, 
          nrow = nrow(data), 
          ncol = ncol(data),
          filename = file_info$name
        )
        
      }, error = function(e) {
        list(success = FALSE, message = paste("Error reading file:", e$message))
      })
      
      # Render the preview table
      if (preview_result$success) {
        output$upload_preview <- DT::renderDT({
          DT::datatable(
            preview_result$data,
            options = list(
              dom = 't',  # Just table, no controls
              scrollX = TRUE,
              pageLength = 5,
              ordering = FALSE
            ),
            rownames = FALSE,
            class = 'table table-sm table-striped'
          )
        })
        
        # Update container with info banner and show the table
        removeUI(selector = paste0("#", ns("upload_preview_container"), " > *"))
        
        insertUI(
          selector = paste0("#", ns("upload_preview_container")),
          where = "beforeEnd",
          ui = tagList(
            div(
              class = "alert alert-info mb-2",
              style = "padding: 0.5rem 1rem;",
              icon("info-circle"), " ",
              sprintf("File: %s | Rows: %d | Columns: %d", 
                      preview_result$filename, 
                      preview_result$nrow, 
                      preview_result$ncol)
            ),
            DT::DTOutput(ns("upload_preview"))
          )
        )
        
      } else {
        # Show error message
        removeUI(selector = paste0("#", ns("upload_preview_container"), " > *"))
        
        insertUI(
          selector = paste0("#", ns("upload_preview_container")),
          where = "beforeEnd",
          ui = div(
            class = "alert alert-danger",
            icon("exclamation-triangle"), " ",
            preview_result$message
          )
        )
      }
    })  
    
    # Toggle advanced options visibility
    observeEvent(input$toggle_advanced, {
      shinyjs::toggle("advanced_options")
    })
    
    # Handle file upload confirmation
    observeEvent(input$confirm_upload, {
      req(input$file_upload)
      
      file_info <- input$file_upload
      cat(sprintf("\n[UPLOAD] Processing file: %s\n", file_info$name))
      
      # Read and validate file using utility function
      result <- tryCatch({
        read_thermogram_file(
          file_info$datapath,
          temp_min = input$temp_min,
          temp_max = input$temp_max
        )
      }, error = function(e) {
        showNotification(
          sprintf("Error reading file: %s", e$message),
          type = "error",
          duration = 3
        )
        return(NULL)
      })
      
      # Exit if reading failed
      if (is.null(result)) {
        removeModal()
        return()
      }
      
      # Generate unique ID for this dataset
      new_counter <- dataset_counter() + 1
      dataset_counter(new_counter)
      dataset_id <- sprintf("dataset_%d", new_counter)
      
      # Store dataset in session
      current_datasets <- uploaded_datasets()
      
      new_dataset <- list(
        id = dataset_id,
        file_name = file_info$name,
        data = result$data,
        format_info = result$format_info,
        upload_time = Sys.time(),
        status = "unprocessed",
        processed_data = NULL,
        temp_params = list(
          temp_min = input$temp_min,
          temp_max = input$temp_max,
          window_size = input$window_size,
          exclusion_lower = input$exclusion_lower,
          exclusion_upper = input$exclusion_upper,
          grid_resolution = input$grid_resolution,
          point_selection = input$point_selection
        )
      )
      
      current_datasets[[dataset_id]] <- new_dataset
      uploaded_datasets(current_datasets)
      
      cat(sprintf("[UPLOAD] Added dataset: %s (ID: %s)\n", file_info$name, dataset_id))
      
      showNotification(
        sprintf("Uploaded %s (%d samples)", file_info$name, result$format_info$n_samples),
        type = "message",
        duration = 2
      )
      
      removeModal()
    })
    
    # =========================================================================
    # UNPROCESSED DATASETS: Display and Actions
    # =========================================================================
    
    output$raw_files_ui <- renderUI({
      datasets <- uploaded_datasets()
      
      # Filter to unprocessed only
      unprocessed <- Filter(function(d) d$status == "unprocessed", datasets)
      
      # Show message if no unprocessed files
      if (length(unprocessed) == 0) {
        return(
          p(
            class = "text-muted",
            "No unprocessed datasets. Click 'Upload New Data' to begin."
          )
        )
      }
      
      # Create UI row for each unprocessed dataset
      rows <- lapply(unprocessed, function(dataset) {
        
        # Action buttons for this dataset
        process_btn <- actionButton(
          ns(paste0("process_", dataset$id)),
          "Process Data",
          icon = icon("cogs"),
          class = "btn-sm btn-success"
        )
        
        remove_btn <- actionButton(
          ns(paste0("remove_", dataset$id)),
          "Remove",
          icon = icon("times"),
          class = "btn-sm btn-outline-danger"
        )
        
        # Return row HTML
        div(
          class = "d-flex justify-content-between align-items-center mb-2 p-3 border rounded",
          div(
            class = "flex-grow-1",
            div(
              icon("file-csv"), " ", strong(dataset$file_name),
              tags$span(class = "badge bg-warning ms-2", "Unprocessed")
            ),
            div(
              class = "small text-muted mt-1",
              sprintf(
                "Samples: %d | Uploaded: %s",
                dataset$format_info$n_samples,
                format(dataset$upload_time, "%Y-%m-%d %H:%M:%S")
              )
            )
          ),
          div(
            class = "flex-shrink-0",
            process_btn,
            remove_btn
          )
        )
      })
      
      tagList(rows)
    })
    
    # Create dynamic observers for unprocessed dataset buttons
    # This observer watches for new datasets and creates button handlers
    observe({
      # Depend on uploaded_datasets (do NOT use isolate here!)
      datasets <- uploaded_datasets()
      existing_obs <- isolate(created_observers_unprocessed())  # ← CHANGED
      
      # Get IDs of unprocessed datasets
      dataset_ids <- names(Filter(function(d) d$status == "unprocessed", datasets))
      
      cat(sprintf("\n[OBSERVERS_UNPROC] Check triggered\n"))
      cat(sprintf("[OBSERVERS_UNPROC] Unprocessed IDs: %s\n", 
                  ifelse(length(dataset_ids) > 0, paste(dataset_ids, collapse=", "), "none")))
      cat(sprintf("[OBSERVERS_UNPROC] Existing trackers: %s\n", 
                  ifelse(length(existing_obs) > 0, paste(existing_obs, collapse=", "), "none")))
      
      # Only create observers for datasets we haven't seen before
      new_ids <- setdiff(dataset_ids, existing_obs)
      
      if (length(new_ids) > 0) {
        cat(sprintf("[OBSERVERS_UNPROC] Creating observers for: %s\n", 
                    paste(new_ids, collapse=", ")))
        
        lapply(new_ids, function(dataset_id) {
          
          # Process button observer
          local({
            did <- dataset_id
            observeEvent(input[[paste0("process_", did)]], {
              cat(sprintf("\n[PROCESS_BTN] Process clicked: %s\n", did))
              process_dataset(did)
            }, ignoreInit = TRUE)
          })
          
          # Remove button observer
          local({
            did <- dataset_id
            observeEvent(input[[paste0("remove_", did)]], {
              cat(sprintf("[REMOVE_BTN] Remove clicked: %s\n", did))
              current <- uploaded_datasets()
              current[[did]] <- NULL
              uploaded_datasets(current)
              showNotification("Dataset removed", type = "message", duration = 2)
            }, ignoreInit = TRUE)
          })
        })
        
        # Update tracker to include new observers
        created_observers_unprocessed(c(existing_obs, new_ids))  # ← CHANGED
        cat(sprintf("[OBSERVERS_UNPROC] ✓ Updated tracker: %s\n\n", 
                    paste(c(existing_obs, new_ids), collapse=", ")))
      }
    })
    
    # =========================================================================
    # DATA PROCESSING: Baseline Detection
    # =========================================================================
    
    # Process a single dataset (called by button observers)
    process_dataset <- function(dataset_id) {
      
      datasets <- uploaded_datasets()
      dataset <- datasets[[dataset_id]]
      
      # Validate dataset exists
      if (is.null(dataset)) {
        showNotification("Dataset not found", type = "error", duration = 3)
        return()
      }
      
      # Show progress notification
      showNotification(
        sprintf("Processing %s...", dataset$file_name),
        id = "processing",
        duration = NULL,
        type = "message"
      )
      
      cat(sprintf("\n[PROCESS] Starting processing for: %s\n", dataset_id))
      
      # Call processing utility function
      result <- tryCatch({
        process_thermogram_data(
          data = dataset$data,
          format_info = dataset$format_info,
          temp_params = dataset$temp_params
        )
      }, error = function(e) {
        removeNotification("processing")
        showNotification(
          sprintf("Processing error: %s", e$message),
          type = "error",
          duration = 3
        )
        cat(sprintf("[PROCESS] ERROR: %s\n", e$message))
        return(NULL)
      })
      
      removeNotification("processing")
      
      # Exit if processing failed
      if (is.null(result)) {
        return()
      }
      
      # After processing succeeds:
      dataset$status <- "processed"
      dataset$processed_data <- result  # result from process_thermogram_data()
      
      # Update in reactive
      datasets[[dataset_id]] <- dataset
      uploaded_datasets(datasets)
      
      cat(sprintf("[PROCESS] ✓ Dataset status set to: %s\n", dataset$status))
      cat(sprintf("[PROCESS] ✓ This should trigger observer creation\n\n"))
      
      
      # Refresh UI
      ui_refresh_trigger(isolate(ui_refresh_trigger()) + 1)
      cat(sprintf("[PROCESS] ✓ UI refresh triggered (count: %d)\n", 
                  isolate(ui_refresh_trigger())))
      
      cat(sprintf("[PROCESS] Successfully processed: %s (%d samples, %d with signal)\n", 
                  dataset_id, result$n_samples, result$n_signal))
      
      showNotification(
        sprintf(
          "Processed %s: %d samples (%d with signal, %d no signal)",
          dataset$file_name,
          result$n_samples,
          result$n_signal,
          result$n_no_signal
        ),
        type = "message",
        duration = 2
      )
    }
    
    # =========================================================================
    # PROCESSED DATASETS: Display and Actions
    # =========================================================================
    
    output$processed_files_ui <- renderUI({
      
      # Force dependency on ui_refresh_trigger
      trigger_count <- ui_refresh_trigger()
      
      # Depend on uploaded_datasets WITHOUT isolate
      datasets <- uploaded_datasets()
      
      # DEBUG: Log that renderUI was triggered
      cat(sprintf("\n╔════════════════════════════════════╗\n"))
      cat(sprintf("║   PROCESSED UI RENDER TRIGGERED    ║\n"))
      cat(sprintf("╚════════════════════════════════════╝\n"))
      cat(sprintf("[PROCESSED_UI] Trigger count: %d\n", trigger_count))
      cat(sprintf("[PROCESSED_UI] Total datasets: %d\n", length(datasets)))
      
      if (length(datasets) == 0) {
        cat("[PROCESSED_UI] No datasets in session\n\n")
        return(
          p(
            class = "text-muted",
            "No datasets in session. Click 'Upload New Data' to begin."
          )
        )
      }
      
      # Log all datasets and their statuses
      cat("[PROCESSED_UI] All datasets:\n")
      for (did in names(datasets)) {
        cat(sprintf("[PROCESSED_UI]   - %s: status='%s', file='%s'\n", 
                    did, datasets[[did]]$status, datasets[[did]]$file_name))
      }
      
      # Filter to processed only (including loaded from disk)
      processed <- Filter(function(d) {
        is_processed <- d$status %in% c("processed", "loaded")
        if (is_processed) {
          cat(sprintf("[PROCESSED_UI] ✓ Including: %s (status: %s)\n", 
                      d$id, d$status))
        }
        is_processed
      }, datasets)
      
      cat(sprintf("[PROCESSED_UI] Filtered result: %d processed dataset(s)\n", 
                  length(processed)))
      
      # Show message if no processed files
      if (length(processed) == 0) {
        cat("[PROCESSED_UI] Returning placeholder message\n\n")
        return(
          p(
            class = "text-muted",
            "No processed datasets yet. Upload and process data to begin."
          )
        )
      }
      
      # Create UI rows for each processed dataset
      cat(sprintf("[PROCESSED_UI] Building UI for %d dataset(s)...\n", length(processed)))
      
      rows <- lapply(processed, function(dataset) {
        
        cat(sprintf("[PROCESSED_UI]   Building row for: %s\n", dataset$id))
        
        # Action buttons for this dataset - CONDITIONAL BASED ON STATUS
        if (dataset$status == "processed") {
          # PROCESSED datasets: All 3 buttons available
          cat(sprintf("[PROCESSED_UI]     Status is PROCESSED - showing all 3 buttons\n"))
          
          button_set <- div(
            class = "btn-group-sm",
            role = "group",
            actionButton(
              ns(paste0("review_", dataset$id)),
              "Review Endpoints",
              icon = icon("chart-line"),
              class = "btn-primary btn-sm me-1",
              title = "Manually review and adjust baseline endpoints"
            ),
            actionButton(
              ns(paste0("save_", dataset$id)),
              "Save to Disk",
              icon = icon("save"),
              class = "btn-success btn-sm me-1",
              title = "Save processed data to local disk"
            ),
            actionButton(
              ns(paste0("report_", dataset$id)),
              "Create Report",
              icon = icon("file-alt"),
              class = "btn-info btn-sm",
              title = "Generate metrics report"
            )
          )
          
        } else if (dataset$status == "loaded") {
          # LOADED datasets: Only "Create Report" button
          # (CSV/Excel files - read-only, cannot be reviewed or re-saved)
          cat(sprintf("[PROCESSED_UI]     Status is LOADED - showing only Create Report button\n"))
          
          button_set <- div(
            class = "btn-group-sm",
            role = "group",
            actionButton(
              ns(paste0("report_", dataset$id)),
              "Create Report",
              icon = icon("file-alt"),
              class = "btn-info btn-sm",
              title = "Generate metrics report"
            )
          )
          
        } else {
          # Fallback for unknown status (defensive coding)
          cat(sprintf("[PROCESSED_UI]     ⚠ Unknown status: %s - showing no buttons\n", 
                      dataset$status))
          button_set <- div(
            class = "btn-group-sm",
            tags$small(class = "text-muted", "Unknown status")
          )
        }
        
        # Determine badge based on status
        status_badge <- if (dataset$status == "loaded") {
          tags$span(class = "badge bg-info ms-2", "Loaded")
        } else {
          tags$span(class = "badge bg-success ms-2", "Processed")
        }
        
        # Sample count
        n_samples <- if (!is.null(dataset$processed_data) && 
                         !is.null(dataset$processed_data$samples)) {
          length(dataset$processed_data$samples)
        } else {
          "?"
        }
        
        cat(sprintf("[PROCESSED_UI]     - Samples: %s, Status: %s\n", 
                    n_samples, dataset$status))
        
        # Return row HTML with clear styling
        div(
          class = "d-flex justify-content-between align-items-center mb-2 p-3 border rounded bg-light",
          style = "border-left: 4px solid #198754 !important;",  # Green left border
          div(
            class = "flex-grow-1",
            div(
              icon("check-circle", class = "text-success"), " ", 
              strong(dataset$file_name),
              status_badge
            ),
            div(
              class = "small text-muted mt-1",
              sprintf("Samples: %s", n_samples)
            )
          ),
          div(
            class = "flex-shrink-0",
            button_set
          )
        )
      })
      
      cat(sprintf("[PROCESSED_UI] ✓ Created %d UI row(s)\n", length(rows)))
      cat(sprintf("╚════════════════════════════════════╝\n\n"))
      
      # Return the rows wrapped in tagList
      tagList(rows)
    })
    
    # =============================================================================
    # DYNAMIC OBSERVERS: Processed Dataset Action Buttons
    # =============================================================================
    # Creates click handlers for Review, Save, and Report buttons on each dataset
    # Uses local() to create proper closures and ignoreInit = TRUE for clean setup
    observe({
      # Depend on uploaded_datasets (do NOT use isolate here!)
      datasets <- uploaded_datasets()
      existing_obs <- isolate(created_observers_processed())  # ← CHANGED
      
      # Get IDs of ALL processed datasets
      dataset_ids <- names(Filter(function(d) {
        d$status %in% c("processed", "loaded")
      }, datasets))
      
      # Debug logging
      cat(sprintf("\n[OBSERVERS_PROC] Check triggered\n"))
      cat(sprintf("[OBSERVERS_PROC] Total datasets: %d\n", length(datasets)))
      cat(sprintf("[OBSERVERS_PROC] Processed IDs: %s\n", 
                  ifelse(length(dataset_ids) > 0, paste(dataset_ids, collapse=", "), "none")))
      cat(sprintf("[OBSERVERS_PROC] Existing trackers: %s\n", 
                  ifelse(length(existing_obs) > 0, paste(existing_obs, collapse=", "), "none")))
      
      # Find datasets that need observers
      new_ids <- setdiff(dataset_ids, existing_obs)
      
      if (length(new_ids) > 0) {
        cat(sprintf("[OBSERVERS_PROC] Creating %d new observer(s): %s\n", 
                    length(new_ids), paste(new_ids, collapse=", ")))
        
        # Create observers for each new dataset
        lapply(new_ids, function(dataset_id) {
          
          # Review Endpoints Button Observer
          local({
            did <- dataset_id
            button_id <- paste0("review_", did)
            cat(sprintf("[OBSERVERS_PROC]   - Creating: %s\n", button_id))
            
            observeEvent(input[[button_id]], {
              cat(sprintf("\n╔══════════════════════════════════════╗\n"))
              cat(sprintf("║  REVIEW BUTTON CLICKED: %-12s ║\n", button_id))
              cat(sprintf("╚══════════════════════════════════════╝\n"))
              cat(sprintf("[CLICK] Dataset ID: %s\n", did))
              navigate_to_review(did)
            }, ignoreInit = TRUE)
          })
          
          # Save to Disk Button Observer
          local({
            did <- dataset_id
            button_id <- paste0("save_", did)
            cat(sprintf("[OBSERVERS_PROC]   - Creating: %s\n", button_id))
            
            observeEvent(input[[button_id]], {
              cat(sprintf("\n╔══════════════════════════════════════╗\n"))
              cat(sprintf("║  SAVE BUTTON CLICKED: %-14s ║\n", button_id))
              cat(sprintf("╚══════════════════════════════════════╝\n"))
              cat(sprintf("[CLICK] Dataset ID: %s\n", did))
              show_save_modal(did)
            }, ignoreInit = TRUE)
          })
          
          # Create Report Button Observer
          local({
            did <- dataset_id
            button_id <- paste0("report_", did)
            cat(sprintf("[OBSERVERS_PROC]   - Creating: %s\n", button_id))
            
            observeEvent(input[[button_id]], {
              cat(sprintf("\n╔══════════════════════════════════════╗\n"))
              cat(sprintf("║  REPORT BUTTON CLICKED: %-12s ║\n", button_id))
              cat(sprintf("╚══════════════════════════════════════╝\n"))
              cat(sprintf("[CLICK] Dataset ID: %s\n", did))
              navigate_to_reports(did)
            }, ignoreInit = TRUE)
          })
        })
        
        # Update the observer tracker
        created_observers_processed(c(existing_obs, new_ids))  # ← CHANGED
        cat(sprintf("[OBSERVERS_PROC] ✓ Tracker updated: %s\n\n", 
                    paste(c(existing_obs, new_ids), collapse=", ")))
      } else {
        cat("[OBSERVERS_PROC] No new observers needed\n\n")
      }
    })
    
    # =========================================================================
    # NAVIGATION: To Review Endpoints Module
    # =========================================================================
    
    navigate_to_review <- function(dataset_id) {
      cat(sprintf("\n╔══════════════════════════════════════════════════╗\n"))
      cat(sprintf("║       NAVIGATE TO REVIEW ENDPOINTS              ║\n"))
      cat(sprintf("╚══════════════════════════════════════════════════╝\n"))
      cat(sprintf("[NAVIGATE] Target dataset ID: %s\n", dataset_id))
      
      # Get all datasets
      datasets <- uploaded_datasets()
      
      # Validate: Dataset exists
      if (is.null(datasets[[dataset_id]])) {
        cat(sprintf("[NAVIGATE] ✗ ERROR: Dataset '%s' not found!\n", dataset_id))
        cat(sprintf("[NAVIGATE] Available datasets: %s\n", 
                    paste(names(datasets), collapse=", ")))
        showNotification(
          "Dataset not found in session",
          type = "error",
          duration = 5
        )
        return()
      }
      
      dataset <- datasets[[dataset_id]]
      cat(sprintf("[NAVIGATE] ✓ Dataset found: %s\n", dataset$file_name))
      cat(sprintf("[NAVIGATE]   Status: %s\n", dataset$status))
      
      # Validate: Dataset is processed
      if (!dataset$status %in% c("processed", "loaded")) {
        cat(sprintf("[NAVIGATE] ✗ ERROR: Dataset not processed\n"))
        cat(sprintf("[NAVIGATE]   Current status: %s\n", dataset$status))
        showNotification(
          sprintf("Dataset '%s' has not been processed yet", dataset$file_name),
          type = "warning",
          duration = 5
        )
        return()
      }
      
      # Validate: Processed data exists
      if (is.null(dataset$processed_data)) {
        cat(sprintf("[NAVIGATE] ✗ ERROR: No processed_data in dataset\n"))
        showNotification(
          "Dataset has no processed data",
          type = "error",
          duration = 5
        )
        return()
      }
      
      # Validate: Samples exist
      if (is.null(dataset$processed_data$samples)) {
        cat(sprintf("[NAVIGATE] ✗ ERROR: No samples in processed_data\n"))
        cat(sprintf("[NAVIGATE]   Available fields: %s\n", 
                    paste(names(dataset$processed_data), collapse=", ")))
        showNotification(
          "Invalid processed data structure - no samples found",
          type = "error",
          duration = 5
        )
        return()
      }
      
      n_samples <- length(dataset$processed_data$samples)
      cat(sprintf("[NAVIGATE] ✓ Processed data validated\n"))
      cat(sprintf("[NAVIGATE]   Samples available: %d\n", n_samples))
      
      # Set application data for Review Endpoints module
      app_data$processed_data <- dataset$processed_data
      app_data$current_dataset_name <- dataset$file_name
      
      cat(sprintf("[NAVIGATE] ✓ app_data$processed_data updated\n"))
      cat(sprintf("[NAVIGATE]   Dataset name: %s\n", app_data$current_dataset_name))
      cat(sprintf("[NAVIGATE]   Samples: %d\n", 
                  length(app_data$processed_data$samples)))
      
      # Switch to Review Endpoints tab
      updateNavbarPage(
        session = session,
        inputId = "main_navbar",
        selected = "review_endpoints"
      )
      
      cat(sprintf("[NAVIGATE] ✓ Switched to Review Endpoints tab\n"))
      cat(sprintf("╚══════════════════════════════════════════════════╝\n\n"))
      
      # User notification
      showNotification(
        sprintf("Loaded '%s' for review (%d samples)", 
                dataset$file_name, 
                n_samples),
        type = "message",
        duration = 3
      )
    }
    
    # =========================================================================
    # NAVIGATION: To Report Builder Module
    # =========================================================================
    
    navigate_to_reports <- function(dataset_id) {
      cat(sprintf("\n╔══════════════════════════════════════════════════╗\n"))
      cat(sprintf("║       NAVIGATE TO REPORT BUILDER                ║\n"))
      cat(sprintf("╚══════════════════════════════════════════════════╝\n"))
      cat(sprintf("[NAVIGATE] Target dataset ID: %s\n", dataset_id))
      
      # Get all datasets
      datasets <- uploaded_datasets()
      
      # Validate: Dataset exists
      if (is.null(datasets[[dataset_id]])) {
        cat(sprintf("[NAVIGATE] ✗ ERROR: Dataset not found\n"))
        showNotification(
          "Dataset not found in session",
          type = "error",
          duration = 3
        )
        return()
      }
      
      dataset <- datasets[[dataset_id]]
      cat(sprintf("[NAVIGATE] ✓ Dataset found: %s\n", dataset$file_name))
      
      # Validate: Dataset is processed
      if (!dataset$status %in% c("processed", "loaded")) {
        cat(sprintf("[NAVIGATE] ✗ ERROR: Dataset not processed\n"))
        showNotification(
          sprintf("Dataset '%s' has not been processed yet", dataset$file_name),
          type = "warning",
          duration = 3
        )
        return()
      }
      
      # Validate: Processed data exists
      if (is.null(dataset$processed_data)) {
        cat(sprintf("[NAVIGATE] ✗ ERROR: No processed_data\n"))
        showNotification(
          "Dataset has no processed data",
          type = "error",
          duration = 3
        )
        return()
      }
      
      cat(sprintf("[NAVIGATE] ✓ Processed data validated\n"))
      
      # Set application data for Report Builder module
      app_data$processed_data <- dataset$processed_data
      app_data$current_dataset_name <- dataset$file_name
      
      cat(sprintf("[NAVIGATE] ✓ app_data$processed_data updated\n"))
      
      # Switch to Report Builder tab
      updateNavbarPage(
        session = session,
        inputId = "main_navbar",
        selected = "report_builder"
      )
      
      cat(sprintf("[NAVIGATE] ✓ Switched to Report Builder tab\n"))
      cat(sprintf("╚══════════════════════════════════════════════════╝\n\n"))
      
      # User notification
      showNotification(
        sprintf("Loaded '%s' for report generation", dataset$file_name),
        type = "message",
        duration = 2
      )
    }
    
    # =========================================================================
    # SAVE TO DISK: Modal and Handler
    # =========================================================================
    
    # Show save modal with format options
    show_save_modal <- function(dataset_id) {
      datasets <- uploaded_datasets()
      dataset <- datasets[[dataset_id]]
      
      # Validate dataset
      if (is.null(dataset)) {
        showNotification("Dataset not found", type = "error", duration = 3)
        return()
      }
      
      # Generate default filename: sanitized name + timestamp
      base_name <- tools::file_path_sans_ext(dataset$file_name)
      default_name <- sprintf(
        "%s_%s",
        gsub("[^A-Za-z0-9_-]", "_", base_name),
        format(Sys.time(), "%Y%m%d_%H%M%S")
      )
      
      showModal(
        modalDialog(
          title = tagList(icon("save"), " Save Processed Dataset"),
          size = "m",
          
          # Filename input
          textInput(
            ns("save_filename"),
            "Dataset Name",
            value = default_name,
            placeholder = "Enter a name for this dataset"
          ),
          
          # Format selection
          radioButtons(
            ns("save_format"),
            "Save Format",
            choices = list(
              "RDS - Full data (recommended for review)" = "rds",
              "CSV - Metrics only (for external analysis)" = "csv",
              "Excel - Metrics + metadata" = "xlsx"
            ),
            selected = "rds"
          ),
          
          # Format explanation
          div(
            class = "alert alert-info",
            icon("info-circle"),
            tags$ul(
              class = "mb-0",
              tags$li(
                strong("RDS:"),
                " Contains full thermogram data, curves, and endpoints. ",
                "Use this for loading back into Review Endpoints."
              ),
              tags$li(
                strong("CSV:"),
                " Contains calculated metrics in wide format. ",
                "Cannot be loaded for review."
              ),
              tags$li(
                strong("Excel:"),
                " Same as CSV but with additional metadata sheet."
              )
            )
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_save"),
              "Save",
              class = "btn-primary",
              icon = icon("check")
            )
          ),
          easyClose = FALSE
        )
      )
      
      # Store dataset ID for confirmation handler
      session$userData$save_dataset_id <- dataset_id
    }
    
    # Handle save confirmation
    observeEvent(input$confirm_save, {
      req(session$userData$save_dataset_id)
      req(input$save_filename)
      
      dataset_id <- session$userData$save_dataset_id
      datasets <- uploaded_datasets()
      dataset <- datasets[[dataset_id]]
      
      # Validate dataset still exists
      if (is.null(dataset)) {
        showNotification("Dataset not found", type = "error", duration = 3)
        removeModal()
        return()
      }
      
      cat(sprintf(
        "\n[SAVE] Saving dataset: %s (Format: %s)\n",
        input$save_filename,
        input$save_format
      ))
      
      # Call save utility function
      result <- save_processed_data(
        data = dataset$processed_data,     
        filename = input$save_filename, 
        format = input$save_format
      )
      
      # Handle result
      if (result$success) {
        showNotification(
          sprintf("Saved: %s", result$filename),
          type = "message",
          duration = 2
        )
        
        # Refresh saved files list
        saved_files_trigger(saved_files_trigger() + 1)
        
      } else {
        showNotification(
          sprintf("Save failed: %s", result$message),
          type = "error",
          duration = 3
        )
      }
      
      removeModal()
    })
    
    # =========================================================================
    # SAVED FILES: Display, Load, and Delete
    # =========================================================================
    
    # Refresh saved files list manually
    observeEvent(input$refresh_saved, {
      cat("[REFRESH] Refreshing saved files list\n")
      saved_files_trigger(saved_files_trigger() + 1)
      showNotification("Refreshed saved files list", type = "message", duration = 2)
    })
    
    # Render saved files list
    output$saved_files_ui <- renderUI({
      df <- saved_files_df()
      
      # Show message if no saved files
      if (nrow(df) == 0) {
        return(
          p(
            class = "text-muted",
            "No saved datasets yet. Process and save data to see files here."
          )
        )
      }
      
      # Create row for each saved file
      rows <- lapply(seq_len(nrow(df)), function(i) {
        file_info <- df[i, ]
        
        # Action buttons using JavaScript for index passing
        load_btn <- tags$button(
          class = "btn btn-sm btn-primary",
          icon("folder-open"), " Load",
          onclick = sprintf(
            "Shiny.setInputValue('%s', %d, {priority: 'event'})",
            ns("load_file_index"),
            i
          )
        )
        
        delete_btn <- tags$button(
          class = "btn btn-sm btn-outline-danger",
          icon("trash"), " Delete",
          onclick = sprintf(
            "Shiny.setInputValue('%s', %d, {priority: 'event'})",
            ns("delete_file_index"),
            i
          )
        )
        
        # Return row HTML
        div(
          class = "d-flex justify-content-between align-items-center mb-2 p-3 border rounded",
          div(
            class = "flex-grow-1",
            div(
              icon("file"), " ", strong(file_info$filename),
              tags$span(
                class = sprintf(
                  "badge bg-%s ms-2",
                  if(file_info$format == "RDS") "success" else "info"
                ),
                file_info$format
              ),
              tags$span(class = "badge bg-secondary ms-2", file_info$load_type)
            ),
            div(
              class = "small text-muted mt-1",
              sprintf(
                "Modified: %s | Size: %.2f MB",
                file_info$modified,
                file_info$size_mb
              )
            )
          ),
          div(
            class = "flex-shrink-0",
            load_btn,
            delete_btn
          )
        )
      })
      
      tagList(rows)
    })
    
    # Handle load file button clicks
    observeEvent(input$load_file_index, {
      req(input$load_file_index)
      
      df <- saved_files_df()
      file_index <- input$load_file_index
      
      # Validate index
      if (file_index < 1 || file_index > nrow(df)) {
        showNotification("Invalid file selection", type = "error", duration = 3)
        return()
      }
      
      file_info <- df[file_index, ]
      
      # Show confirmation modal
      showModal(
        modalDialog(
          title = tagList(icon("folder-open"), " Load Processed Dataset"),
          size = "m",
          
          p("Load this dataset into the application?"),
          
          # File details
          div(
            class = "alert alert-info",
            icon("info-circle"),
            tags$ul(
              class = "mb-0",
              tags$li(strong("File: "), file_info$filename),
              tags$li(strong("Format: "), file_info$format),
              tags$li(strong("Load Type: "), file_info$load_type),
              tags$li(strong("Size: "), sprintf("%.2f MB", file_info$size_mb))
            )
          ),
          
          # Navigation guidance
          if (file_info$format == "RDS") {
            div(
              class = "alert alert-success",
              icon("check-circle"),
              " After loading, you can navigate to Review Endpoints or Report Builder."
            )
          } else {
            div(
              class = "alert alert-warning",
              icon("exclamation-triangle"),
              " This file contains metrics only. ",
              "You can create reports but cannot review endpoints."
            )
          },
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_load"),
              "Load Dataset",
              class = "btn-primary",
              icon = icon("check")
            )
          ),
          easyClose = FALSE
        )
      )
      
      # Store file info for confirmation handler
      session$userData$load_filepath <- file_info$filepath
      session$userData$load_filename <- file_info$filename
      session$userData$load_format <- file_info$format
    })
    
    # Handle load confirmation
    observeEvent(input$confirm_load, {
      req(session$userData$load_filepath)
      
      filepath <- session$userData$load_filepath
      filename <- session$userData$load_filename
      
      cat(sprintf("\n[LOAD] Loading from disk: %s\n", filepath))
      cat(sprintf("[LOAD] Loading file: %s\n", filename))
      
      tryCatch({
        # Load the file
        loaded <- load_processed_data(filepath)
        
        # Validate loaded data
        if (is.null(loaded) || !is.list(loaded)) {
          showNotification(
            "Failed to load file: Invalid format",
            type = "error",
            duration = 3
          )
          removeModal()
          return()
        }
        
        # Extract data and type
        data <- loaded$data
        data_type <- loaded$data_type  # "full" or "report_only"
        
        # =========================================================================
        # FIXED: Generate unique dataset ID for the loaded file
        # =========================================================================
        
        # Use same ID generation as uploaded datasets for consistency
        # Format: LOADED_filename_timestamp
        base_name <- tools::file_path_sans_ext(filename)
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        dataset_id <- sprintf("LOADED_%s_%s", base_name, timestamp)
        
        # Check for collision (extremely unlikely but defensive)
        counter <- 1
        original_id <- dataset_id
        datasets <- uploaded_datasets()
        while (dataset_id %in% names(datasets)) {
          counter <- counter + 1
          dataset_id <- sprintf("%s_%d", original_id, counter)
        }
        
        cat(sprintf("[LOAD] Generated dataset ID: %s\n", dataset_id))
        
        # =========================================================================
        # FIXED: Create dataset entry with all required fields
        # =========================================================================
        
        if (data_type == "full") {
          # Full RDS data - can populate Review Endpoints
          cat(sprintf("[LOAD] Loaded full dataset: %d samples\n", length(data$samples)))
          
          # Create dataset entry structure
          loaded_dataset <- list(
            id = dataset_id,
            file_name = filename,
            data = NULL,  # Not used for loaded full data
            format_info = list(
              data_format = "rds",
              n_samples = length(data$samples),
              n_rows = sum(sapply(data$samples, function(s) length(s$temperature)))
            ),
            upload_time = Sys.time(),
            status = "loaded",
            processed_data = data,  # Full processed data from RDS
            temp_params = list()
          )
          
          # Add to uploaded_datasets reactive
          datasets[[dataset_id]] <- loaded_dataset
          uploaded_datasets(datasets)
          
          cat(sprintf("[LOAD] ✓ Added to uploaded_datasets with ID: %s\n", dataset_id))
          
          # Also set as current processed data for Review Endpoints
          app_data$processed_data <- data
          
          showNotification(
            HTML(sprintf(
              "<strong>Loaded %s</strong><br/>%d samples ready for review",
              filename,
              length(data$samples)
            )),
            type = "message",
            duration = 2
          )
          
          # Switch to Review Endpoints tab
          updateNavbarPage(session, "main_navbar", selected = "review_endpoints")
          
        } else if (data_type == "report_only") {
          # CSV/Excel data - for reports only
          cat(sprintf("[LOAD] Loaded report-only dataset: %d samples\n", nrow(data)))
          
          # Create dataset entry structure for report data
          loaded_dataset <- list(
            id = dataset_id,
            file_name = filename,
            data = data,  # Store the wide-format metrics data
            format_info = list(
              data_format = tolower(tools::file_ext(filename)),
              n_samples = nrow(data),
              n_columns = ncol(data),
              load_type = "report_only"
            ),
            upload_time = Sys.time(),
            status = "loaded",
            processed_data = NULL,  # Not applicable for report-only data
            temp_params = list()
          )
          
          # Add to uploaded_datasets reactive
          datasets[[dataset_id]] <- loaded_dataset
          uploaded_datasets(datasets)
          
          cat(sprintf("[LOAD] ✓ Added to uploaded_datasets with ID: %s\n", dataset_id))
          
          # Also store in app_data for potential report generation
          app_data$report_data <- data
          
          showNotification(
            HTML(sprintf(
              "<strong>Loaded %s</strong><br/>This file contains metrics only. Navigate to Report Builder to create reports.",
              filename
            )),
            type = "warning",
            duration = 3
          )
          
          # Stay on Data Overview tab (don't switch tabs)
        }
        
        # Trigger UI refresh to show newly added dataset
        ui_refresh_trigger(ui_refresh_trigger() + 1)
        
        # Also refresh saved files list
        saved_files_trigger(saved_files_trigger() + 1)
        
      }, error = function(e) {
        cat(sprintf("[LOAD] ERROR: %s\n", e$message))
        showNotification(
          sprintf("Load failed: %s", e$message),
          type = "error",
          duration = 3
        )
      })
      
      removeModal()
    })
    
    # Handle delete file button clicks
    observeEvent(input$delete_file_index, {
      req(input$delete_file_index)
      
      df <- saved_files_df()
      file_index <- input$delete_file_index
      
      # Validate index
      if (file_index < 1 || file_index > nrow(df)) {
        showNotification("Invalid file selection", type = "error", duration = 3)
        return()
      }
      
      file_info <- df[file_index, ]
      
      # Show confirmation modal
      showModal(
        modalDialog(
          title = tagList(icon("trash"), " Delete Saved Dataset"),
          size = "m",
          
          p("Are you sure you want to delete this file? This action cannot be undone."),
          
          # Warning with file details
          div(
            class = "alert alert-danger",
            icon("exclamation-triangle"),
            tags$ul(
              class = "mb-0",
              tags$li(strong("File: "), file_info$filename),
              tags$li(strong("Format: "), file_info$format),
              tags$li(strong("Size: "), sprintf("%.2f MB", file_info$size_mb))
            )
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_delete"),
              "Delete",
              class = "btn-danger",
              icon = icon("trash")
            )
          ),
          easyClose = FALSE
        )
      )
      
      # Store file info for confirmation handler
      session$userData$delete_filepath <- file_info$filepath
      session$userData$delete_filename <- file_info$filename
    })
    
    # Handle delete confirmation
    observeEvent(input$confirm_delete, {
      req(session$userData$delete_filepath)
      
      filepath <- session$userData$delete_filepath
      filename <- session$userData$delete_filename
      
      cat(sprintf("[DELETE] Deleting file: %s\n", filepath))
      
      # Call delete utility function
      result <- delete_processed_data(filepath)
      
      # Safe type checking before accessing $success
      if (is.list(result) && isTRUE(result$success)) {
        showNotification(
          sprintf("Deleted: %s", filename),
          type = "message",
          duration = 2
        )
        
        # Refresh saved files list
        saved_files_trigger(saved_files_trigger() + 1)
        
      } else if (is.list(result)) {
        showNotification(
          sprintf("Delete failed: %s", result$message),
          type = "error",
          duration = 3
        )
      } else {
        showNotification(
          "Delete failed: Unexpected error",
          type = "error",
          duration = 3
        )
      }
      
      removeModal()
    })
    
    # =========================================================================
    # GENERATED REPORTS: Display with Search/Filter
    # =========================================================================
    
    output$generated_reports_list <- renderUI({
      reports <- app_data$generated_reports
      search_term <- input$reports_search
      
      # Show message if no reports
      if (is.null(reports) || length(reports) == 0) {
        return(
          p(
            class = "text-muted",
            "No reports generated yet. Go to Report Builder to create reports."
          )
        )
      }
      
      # Apply search filter if search term exists
      if (!is.null(search_term) && nchar(trimws(search_term)) > 0) {
        # Convert search term to lowercase for case-insensitive matching
        search_lower <- tolower(trimws(search_term))
        
        # Filter reports based on search term
        # Search in: report name, dataset name, and format
        filtered_reports <- Filter(function(report) {
          name_match <- grepl(search_lower, tolower(report$name), fixed = TRUE)
          dataset_match <- grepl(search_lower, tolower(report$dataset_name), fixed = TRUE)
          format_match <- grepl(search_lower, tolower(report$format), fixed = TRUE)
          
          # Return TRUE if any field matches
          name_match || dataset_match || format_match
        }, reports)
        
        # Show message if no results
        if (length(filtered_reports) == 0) {
          return(
            div(
              class = "alert alert-info",
              icon("search"),
              sprintf(
                " No reports match '%s'. Try a different search term.",
                search_term
              )
            )
          )
        }
        
        # Use filtered reports
        reports_to_show <- filtered_reports
      } else {
        # Show all reports if no search term
        reports_to_show <- reports
      }
      
      # Create row for each report
      rows <- lapply(seq_along(reports_to_show), function(i) {
        report <- reports_to_show[[i]]
        
        # Find original index in full reports list (for delete functionality)
        original_index <- which(sapply(reports, function(r) {
          identical(r$id, report$id)
        }))[1]
        
        # Format badge color based on format
        format_badge_color <- if (report$format == "xlsx") "primary" else "info"
        
        # Action buttons
        download_btn <- tags$button(
          class = "btn btn-sm btn-success",
          icon("download"), " Download",
          onclick = sprintf(
            "Shiny.setInputValue('%s', %d, {priority: 'event'})",
            ns("download_report_index"),
            original_index
          )
        )
        
        delete_btn <- tags$button(
          class = "btn btn-sm btn-outline-danger",
          icon("trash"), " Delete",
          onclick = sprintf(
            "Shiny.setInputValue('%s', %d, {priority: 'event'})",
            ns("delete_report_index"),
            original_index
          )
        )
        
        # Return row HTML
        div(
          class = "d-flex justify-content-between align-items-center mb-2 p-3 border rounded",
          div(
            class = "flex-grow-1",
            div(
              icon("chart-line"), " ", strong(report$name),
              tags$span(
                class = sprintf("badge bg-%s ms-2", format_badge_color),
                toupper(report$format)
              )
            ),
            div(
              class = "small text-muted mt-1",
              sprintf(
                "Dataset: %s | %d samples, %d metrics | %s",
                report$dataset_name,
                report$n_samples,
                report$n_metrics,
                format(report$generated_at, "%Y-%m-%d %H:%M:%S")
              )
            )
          ),
          div(
            class = "flex-shrink-0",
            download_btn,
            delete_btn
          )
        )
      })
      
      tagList(rows)
    })
    
    # =========================================================================
    # REPORT ACTIONS: Download and Delete
    # =========================================================================
    
    # Handle report download button clicks
    observeEvent(input$download_report_index, {
      req(input$download_report_index)
      
      reports <- app_data$generated_reports
      report_index <- input$download_report_index
      
      # Validate index
      if (report_index < 1 || report_index > length(reports)) {
        showNotification("Invalid report selection", type = "error", duration = 3)
        return()
      }
      
      report <- reports[[report_index]]
      
      # Check file exists
      if (!file.exists(report$filepath)) {
        showNotification(
          "Report file not found. It may have been moved or deleted.",
          type = "error",
          duration = 3
        )
        return()
      }
      
      # Trigger browser download using JavaScript
      # Note: This requires custom JavaScript handler in app.R or theme.R
      session$sendCustomMessage(
        type = "downloadFile",
        message = list(
          filepath = report$filepath,
          filename = basename(report$filepath)
        )
      )
      
      showNotification(
        sprintf("Downloading: %s", report$name),
        type = "message",
        duration = 2
      )
    })
    
    # Handle report delete button clicks
    observeEvent(input$delete_report_index, {
      req(input$delete_report_index)
      
      reports <- app_data$generated_reports
      report_index <- input$delete_report_index
      
      # Validate index
      if (report_index < 1 || report_index > length(reports)) {
        showNotification("Invalid report selection", type = "error", duration = 3)
        return()
      }
      
      report <- reports[[report_index]]
      
      # Show confirmation modal
      showModal(
        modalDialog(
          title = tagList(icon("trash"), " Delete Report"),
          size = "m",
          
          p("Are you sure you want to delete this report from the list?"),
          p(
            strong("Note:"),
            " The report file on disk will NOT be deleted, ",
            "only removed from this session's tracking."
          ),
          
          # Report details
          div(
            class = "alert alert-warning",
            icon("info-circle"),
            tags$ul(
              class = "mb-0",
              tags$li(strong("Report: "), report$name),
              tags$li(strong("Format: "), toupper(report$format)),
              tags$li(strong("File: "), report$filepath)
            )
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_delete_report"),
              "Remove from List",
              class = "btn-warning",
              icon = icon("minus-circle")
            )
          ),
          easyClose = FALSE
        )
      )
      
      # Store report index for confirmation handler
      session$userData$delete_report_index <- report_index
    })
    
    # Handle report delete confirmation
    observeEvent(input$confirm_delete_report, {
      req(session$userData$delete_report_index)
      
      report_index <- session$userData$delete_report_index
      reports <- app_data$generated_reports
      
      # Validate index still valid
      if (report_index < 1 || report_index > length(reports)) {
        showNotification("Invalid report selection", type = "error", duration = 3)
        removeModal()
        return()
      }
      
      report_name <- reports[[report_index]]$name
      
      # Remove from list (sets element to NULL, which R automatically removes)
      app_data$generated_reports[[report_index]] <- NULL
      
      cat(sprintf("[DELETE_REPORT] Removed from tracking: %s\n", report_name))
      
      showNotification(
        sprintf("Removed report from list: %s", report_name),
        type = "message",
        duration = 2
      )
      
      removeModal()
    })
    
    # =========================================================================
    # OPEN REPORTS FOLDER: Platform-Aware (NEW - Item #4)
    # =========================================================================
    
    observeEvent(input$open_reports_folder, {
      cat("\n[OPEN_FOLDER] Opening reports directory\n")
      
      # Get absolute path to reports directory
      reports_dir <- file.path(getwd(), "reports")
      
      # Create directory if it doesn't exist
      if (!dir.exists(reports_dir)) {
        dir.create(reports_dir, recursive = TRUE)
        cat(sprintf("[OPEN_FOLDER] Created directory: %s\n", reports_dir))
      }
      
      # Detect platform and open folder accordingly
      result <- tryCatch({
        
        if (.Platform$OS.type == "windows") {
          # Windows: Use shell.exec
          shell.exec(reports_dir)
          
        } else if (Sys.info()["sysname"] == "Darwin") {
          # macOS: Use 'open' command
          system2("open", args = shQuote(reports_dir))
          
        } else {
          # Linux: Try common file managers in order
          if (Sys.which("xdg-open") != "") {
            system2("xdg-open", args = shQuote(reports_dir))
          } else if (Sys.which("nautilus") != "") {
            system2("nautilus", args = shQuote(reports_dir))
          } else if (Sys.which("dolphin") != "") {
            system2("dolphin", args = shQuote(reports_dir))
          } else {
            stop("No suitable file manager found")
          }
        }
        
        TRUE  # Success
        
      }, error = function(e) {
        cat(sprintf("[OPEN_FOLDER] Error: %s\n", e$message))
        return(FALSE)
      })
      
      # Show appropriate notification
      if (result) {
        showNotification(
          "Opening reports folder in file browser...",
          type = "message",
          duration = 2
        )
      } else {
        showNotification(
          HTML(sprintf(
            "Could not open folder automatically.<br/>Please navigate to:<br/><code>%s</code>",
            reports_dir
          )),
          type = "warning",
          duration = 3
        )
      }
    })
    
  })
}


# ==============================================================================
# END OF MODULE
# ==============================================================================
