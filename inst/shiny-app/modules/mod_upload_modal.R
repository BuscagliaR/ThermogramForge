# ==============================================================================
# Upload Modal Sub-Module
# ==============================================================================
#
# FILE: mod_upload_modal.R
# MODULE: Upload Modal
# VERSION: 1.1.0
# AUTHOR: Chris Reger
# LAST UPDATED: December 9, 2024
#

# Diagnostic: Confirm this file is being sourced
cat("\n[STARTUP] mod_upload_modal.R loaded successfully\n")

# ==============================================================================
# PURPOSE
# ==============================================================================
#
# Self-contained sub-module handling the file upload workflow for ThermogramForge.
# This module manages the upload modal dialog, including:
#
#   - File selection (CSV and Excel formats)
#   - Excel multi-sheet detection and selection
#   - Live data preview
#   - Temperature range parameters
#   - Advanced baseline detection options
#
# Designed to be called from mod_data_overview.R as a sub-module.
#
# ==============================================================================
# ARCHITECTURE
# ==============================================================================
#
# This module follows a "trigger + return reactive" pattern:
#   1. Parent module calls mod_upload_modal_server() with a trigger reactive
#   2. When trigger fires, sub-module shows modal from within its own context
#   3. On confirm, sub-module returns uploaded data via reactive
#   4. Parent observes the reactive and processes new uploads
#
# KEY INSIGHT: The modal MUST be shown from within moduleServer() context
# so that all input/output IDs are properly namespaced. Showing the modal
# from the parent with manually constructed IDs causes namespace mismatches.
#
# REACTIVE FLOW:
#   trigger (from parent)
#       -> show modal (within sub-module server)
#       -> file_upload (input)
#       -> sheet detection (observer)
#       -> excel_sheets (reactiveVal)
#       -> sheet_selector_ui (renderUI)
#       -> selected_sheet (reactiveVal)
#       -> preview_data (reactive)
#       -> preview_section_ui (renderUI)
#
# ON CONFIRM:
#   confirm_upload (input)
#       -> read file with parameters
#       -> update uploaded_data reactiveVal
#       -> close modal
#
# ==============================================================================
# USAGE
# ==============================================================================
#
# In parent module (mod_data_overview.R):
#
#   # Create trigger reactive
#   upload_trigger <- reactiveVal(0)
#
#   # Initialize server (pass trigger)
#   uploaded_result <- mod_upload_modal_server("upload_modal", upload_trigger)
#   
#   # Trigger modal from parent
#   observeEvent(input$upload_btn, {
#     upload_trigger(upload_trigger() + 1)  # Increment to trigger
#   })
#
#   # Observe uploads
#   observeEvent(uploaded_result(), {
#     result <- uploaded_result()
#     if (!is.null(result)) {
#       # Process the uploaded data
#     }
#   }, ignoreNULL = TRUE)
#
# ==============================================================================
# DEPENDENCIES
# ==============================================================================
#
# R Packages:
#   - shiny: Core framework
#   - shinyjs: JavaScript utilities (for toggle)
#   - readxl: Excel file reading and sheet detection
#   - readr: CSV file reading
#
# Project Files:
#   - data_utils.R: read_thermogram_file()
#
# ==============================================================================


# ==============================================================================
# MODULE UI FUNCTION
# ==============================================================================
#
#' Upload Modal Sub-Module UI
#'
#' Creates placeholder UI elements for the upload modal sub-module.
#' The actual modal is shown dynamically via showModal() from the server.
#'
#' @param id Character string. Namespace ID for the module.
#'
#' @return An empty tagList. All UI is rendered dynamically in the modal.
#'
#' @details
#' This function returns minimal/empty UI because the modal is shown 
#' dynamically from within the server function. This ensures proper
#' namespace handling for all inputs and outputs.
#'
#' @family upload_modal
#' @seealso \code{\link{mod_upload_modal_server}}
#'
#' @export
mod_upload_modal_ui <- function(id) {
  # No static UI needed - modal is shown dynamically from server
  # This ensures all inputs/outputs are properly namespaced
  tagList()
}


# ==============================================================================
# MODULE SERVER FUNCTION
# ==============================================================================
#
#' Upload Modal Sub-Module Server
#'
#' Server logic for the upload modal, handling file selection, sheet detection,
#' preview generation, and data upload.
#'
#' @param id Character string. Namespace ID matching the UI function.
#' @param trigger A reactive expression that triggers the modal to open.
#'   Typically a reactiveVal that gets incremented when upload button is clicked.
#'
#' @return A reactive expression that returns the uploaded data when available,
#'   or NULL when no upload has occurred. The returned list contains:
#'   \describe{
#'     \item{data}{The raw thermogram data frame}
#'     \item{format_info}{List with format details (n_samples, format_type, etc.)}
#'     \item{file_name}{Original file name}
#'     \item{sheet}{Excel sheet name (NULL for CSV)}
#'     \item{params}{List of all parameters (temp_min, temp_max, etc.)}
#'   }
#'
#' @details
#' The server manages several reactive components:
#'
#' \strong{Key Design Decision:}
#' The modal is shown from WITHIN the moduleServer context using showModal().
#' This ensures that all input IDs (file_upload, sheet_selector, etc.) are
#' automatically namespaced correctly. Attempting to show the modal from the
#' parent module causes namespace mismatches.
#'
#' \strong{Reactive Values:}
#' \itemize{
#'   \item \code{excel_sheets}: Available sheet names (NULL for CSV)
#'   \item \code{selected_sheet}: Currently selected sheet
#'   \item \code{uploaded_data}: Result of successful upload (returned to parent)
#' }
#'
#' @family upload_modal
#' @seealso \code{\link{mod_upload_modal_ui}}
#'
#' @export
mod_upload_modal_server <- function(id, trigger) {
  moduleServer(id, function(input, output, session) {
    
    # Namespace function for this module
    ns <- session$ns
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    
    # Excel sheet names detected from uploaded file (NULL for CSV)
    excel_sheets <- reactiveVal(NULL)
    
    # Currently selected sheet name
    selected_sheet <- reactiveVal(NULL)
    
    # Result of successful upload - parent module observes this
    uploaded_data <- reactiveVal(NULL)
    
    
    # =========================================================================
    # SECTION 1: SHOW MODAL (Triggered by Parent)
    # =========================================================================
    # When parent increments the trigger, show the upload modal.
    # CRITICAL: Modal is shown from HERE (inside moduleServer) so that
    # all input/output IDs are properly namespaced.
    # =========================================================================
    
    observeEvent(trigger(), {
      
      cat("\n")
      cat("========================================\n")
      cat("[UPLOAD_MODAL] Trigger received - showing modal\n")
      cat("========================================\n")
      
      # Reset state for new upload
      excel_sheets(NULL)
      selected_sheet(NULL)
      
      # Show the modal dialog
      showModal(
        modalDialog(
          title = tagList(icon("upload"), " Upload Raw Thermogram Data"),
          size = "l",
          
          # ---------------------------------------------------------------------
          # ROW 1: File Input + Temperature Range
          # ---------------------------------------------------------------------
          fluidRow(
            column(
              6,
              fileInput(
                ns("file_upload"),
                "Choose CSV or Excel file",
                accept = c(".csv", ".xlsx", ".xls", ".xlsm"),
                multiple = FALSE
              )
            ),
            column(
              3,
              numericInput(ns("temp_min"), "Min Temp (deg C)", 
                           value = 20, min = 0, max = 100)
            ),
            column(
              3,
              numericInput(ns("temp_max"), "Max Temp (deg C)", 
                           value = 110, min = 0, max = 150)
            )
          ),
          
          # ---------------------------------------------------------------------
          # ROW 2: Sheet Selector (conditional - rendered via renderUI)
          # ---------------------------------------------------------------------
          uiOutput(ns("sheet_selector_ui")),
          
          # ---------------------------------------------------------------------
          # ROW 3: Data Preview
          # ---------------------------------------------------------------------
          hr(),
          h5(tagList(icon("table"), " Data Preview"), class = "mt-3"),
          uiOutput(ns("preview_section_ui")),
          
          # ---------------------------------------------------------------------
          # ROW 4: Advanced Options (collapsible)
          # ---------------------------------------------------------------------
          hr(),
          actionLink(ns("toggle_advanced"), 
                     tagList(icon("cog"), " Advanced Options"), 
                     class = "text-secondary"),
          
          shinyjs::hidden(
            div(
              id = ns("advanced_options"),
              class = "mt-3 p-3 border rounded bg-light",
              h6("Baseline Detection Parameters"),
              
              fluidRow(
                column(6, 
                       numericInput(ns("window_size"), "Window Size (points)", 
                                    value = 90, min = 30, max = 200),
                       tags$small(class = "form-text text-muted", "Rolling variance window")
                ),
                column(6, 
                       selectInput(ns("point_selection"), "Endpoint Selection",
                                   choices = c("Innermost" = "innermost", 
                                               "Outermost" = "outermost", 
                                               "Middle" = "middle"), 
                                   selected = "innermost"),
                       tags$small(class = "form-text text-muted", "Selection strategy")
                )
              ),
              
              fluidRow(
                column(6, 
                       numericInput(ns("exclusion_lower"), "Exclusion Lower (deg C)", 
                                    value = 60, min = 40, max = 80),
                       tags$small(class = "form-text text-muted", "Lower bound of transition")
                ),
                column(6, 
                       numericInput(ns("exclusion_upper"), "Exclusion Upper (deg C)", 
                                    value = 80, min = 60, max = 95),
                       tags$small(class = "form-text text-muted", "Upper bound of transition")
                )
              ),
              
              fluidRow(
                column(6, 
                       numericInput(ns("grid_resolution"), "Grid Resolution (deg C)", 
                                    value = 0.1, min = 0.01, max = 1.0, step = 0.05),
                       tags$small(class = "form-text text-muted", "Temperature step")
                )
              )
            )
          ),
          
          # ---------------------------------------------------------------------
          # Footer: Cancel and Upload buttons
          # ---------------------------------------------------------------------
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_upload"), "Upload Data", 
                         class = "btn-primary", icon = icon("check"))
          ),
          easyClose = FALSE
        )
      )
      
    }, ignoreInit = TRUE)
    
    
    # =========================================================================
    # SECTION 2: FILE UPLOAD AND SHEET DETECTION
    # =========================================================================
    # When a file is uploaded, detect if it's Excel and enumerate sheets.
    # =========================================================================
    
    observeEvent(input$file_upload, {
      
      cat("\n")
      cat("----------------------------------------\n")
      cat("[UPLOAD_MODAL] File upload detected\n")
      cat("----------------------------------------\n")
      
      # Reset state when no file
      if (is.null(input$file_upload)) {
        cat("[UPLOAD_MODAL] No file - resetting state\n")
        excel_sheets(NULL)
        selected_sheet(NULL)
        return()
      }
      
      file_info <- input$file_upload
      cat(sprintf("[UPLOAD_MODAL] File name: %s\n", file_info$name))
      cat(sprintf("[UPLOAD_MODAL] File size: %d bytes\n", file_info$size))
      
      # Check if Excel file
      is_excel <- grepl("\\.(xlsx|xls|xlsm)$", file_info$name, ignore.case = TRUE)
      cat(sprintf("[UPLOAD_MODAL] Is Excel: %s\n", is_excel))
      
      if (is_excel) {
        # Detect sheets in Excel file
        tryCatch({
          sheets <- readxl::excel_sheets(file_info$datapath)
          cat(sprintf("[UPLOAD_MODAL] Found %d sheet(s): %s\n", 
                      length(sheets), paste(sheets, collapse = ", ")))
          
          excel_sheets(sheets)
          
          # Auto-select first sheet
          if (length(sheets) > 0) {
            selected_sheet(sheets[1])
            cat(sprintf("[UPLOAD_MODAL] Auto-selected: '%s'\n", sheets[1]))
          }
          
        }, error = function(e) {
          cat(sprintf("[UPLOAD_MODAL] ERROR reading sheets: %s\n", e$message))
          excel_sheets(NULL)
          selected_sheet(NULL)
        })
        
      } else {
        # CSV file - no sheets
        cat("[UPLOAD_MODAL] CSV file - no sheets\n")
        excel_sheets(NULL)
        selected_sheet(NULL)
      }
      
      cat("----------------------------------------\n\n")
      
    }, ignoreNULL = FALSE)
    
    
    # =========================================================================
    # SECTION 3: SHEET SELECTOR UI
    # =========================================================================
    # Renders a dropdown for sheet selection when multiple sheets detected.
    # =========================================================================
    
    output$sheet_selector_ui <- renderUI({
      
      sheets <- excel_sheets()
      
      cat(sprintf("[UPLOAD_MODAL:SHEET_UI] sheets = %s\n",
                  if (is.null(sheets)) "NULL" else paste(sheets, collapse = ", ")))
      
      # Only show dropdown for multi-sheet Excel files
      if (!is.null(sheets) && length(sheets) > 1) {
        cat(sprintf("[UPLOAD_MODAL:SHEET_UI] Rendering dropdown (%d sheets)\n", 
                    length(sheets)))
        
        div(
          class = "alert alert-info mt-2 mb-2",
          style = "padding: 0.75rem 1rem;",
          div(
            icon("layer-group"), " ",
            strong("Multiple sheets detected."),
            " Select which sheet contains your data:"
          ),
          selectInput(
            ns("sheet_selector"),
            label = NULL,
            choices = sheets,
            selected = sheets[1],
            width = "100%"
          )
        )
        
      } else if (!is.null(sheets) && length(sheets) == 1) {
        # Single sheet - show confirmation message
        cat(sprintf("[UPLOAD_MODAL:SHEET_UI] Single sheet: '%s'\n", sheets[1]))
        
        div(
          class = "alert alert-success mt-2 mb-2",
          style = "padding: 0.5rem 1rem;",
          icon("check-circle"), " ",
          sprintf("Using sheet: '%s'", sheets[1])
        )
        
      } else {
        # CSV or no file - no selector needed
        cat("[UPLOAD_MODAL:SHEET_UI] No selector needed\n")
        NULL
      }
    })
    
    
    # =========================================================================
    # SECTION 4: SHEET SELECTION HANDLER
    # =========================================================================
    
    observeEvent(input$sheet_selector, {
      req(input$sheet_selector)
      cat(sprintf("[UPLOAD_MODAL] Sheet selected: '%s'\n", input$sheet_selector))
      selected_sheet(input$sheet_selector)
    }, ignoreInit = TRUE)
    
    
    # =========================================================================
    # SECTION 5: PREVIEW DATA (Reactive)
    # =========================================================================
    
    preview_data <- reactive({
      
      req(input$file_upload)
      
      file_info <- input$file_upload
      sheet <- selected_sheet()
      
      cat(sprintf("[UPLOAD_MODAL:PREVIEW] File: %s, Sheet: %s\n",
                  file_info$name, if (is.null(sheet)) "NULL" else sheet))
      
      tryCatch({
        # Read based on file type
        if (grepl("\\.csv$", file_info$name, ignore.case = TRUE)) {
          data <- readr::read_csv(file_info$datapath, 
                                  show_col_types = FALSE,
                                  n_max = 100)
          
        } else if (grepl("\\.(xlsx|xls|xlsm)$", file_info$name, ignore.case = TRUE)) {
          if (!is.null(sheet)) {
            data <- readxl::read_excel(file_info$datapath, sheet = sheet, n_max = 100)
          } else {
            data <- readxl::read_excel(file_info$datapath, n_max = 100)
          }
          
        } else {
          return(list(success = FALSE, message = "Unsupported file type"))
        }
        
        cat(sprintf("[UPLOAD_MODAL:PREVIEW] Read %d rows x %d cols\n", 
                    nrow(data), ncol(data)))
        
        list(
          success = TRUE,
          data = head(data, 5),
          nrow = nrow(data),
          ncol = ncol(data),
          filename = file_info$name,
          sheet = sheet
        )
        
      }, error = function(e) {
        cat(sprintf("[UPLOAD_MODAL:PREVIEW] ERROR: %s\n", e$message))
        list(success = FALSE, message = paste("Error reading file:", e$message))
      })
    })
    
    
    # =========================================================================
    # SECTION 6: PREVIEW SECTION UI
    # =========================================================================
    
    output$preview_section_ui <- renderUI({
      
      # Check if file is uploaded
      if (is.null(input$file_upload)) {
        cat("[UPLOAD_MODAL:PREVIEW_UI] No file - showing placeholder\n")
        return(
          div(
            class = "text-muted py-3 text-center",
            icon("cloud-upload-alt", class = "fa-2x mb-2"), 
            br(),
            "Select a file to see a preview"
          )
        )
      }
      
      result <- preview_data()
      
      # Handle error
      if (!result$success) {
        cat(sprintf("[UPLOAD_MODAL:PREVIEW_UI] Error: %s\n", result$message))
        return(
          div(
            class = "alert alert-danger",
            icon("exclamation-triangle"), " ", result$message
          )
        )
      }
      
      # Build preview
      cat(sprintf("[UPLOAD_MODAL:PREVIEW_UI] Building table (%d rows)\n", 
                  nrow(result$data)))
      
      # Info message
      info_parts <- c(
        sprintf("File: %s", result$filename),
        sprintf("Rows: %d", result$nrow),
        sprintf("Columns: %d", result$ncol)
      )
      if (!is.null(result$sheet)) {
        info_parts <- c(info_parts, sprintf("Sheet: '%s'", result$sheet))
      }
      info_msg <- paste(info_parts, collapse = " | ")
      
      # Build table
      preview_df <- result$data
      col_names <- names(preview_df)
      
      # Header cells
      header_cells <- lapply(col_names, function(col) {
        tags$th(col, 
                style = "padding: 6px 10px; border: 1px solid #dee2e6; background-color: #f8f9fa; font-size: 0.85em; white-space: nowrap;"
        )
      })
      
      # Data rows
      data_rows <- lapply(seq_len(nrow(preview_df)), function(i) {
        row_cells <- lapply(seq_len(ncol(preview_df)), function(j) {
          val <- preview_df[[j]][i]
          cell_text <- if (is.na(val)) "" else as.character(val)
          tags$td(cell_text, 
                  style = "padding: 6px 10px; border: 1px solid #dee2e6; font-size: 0.85em;"
          )
        })
        bg_style <- if (i %% 2 == 0) "background-color: #f8f9fa;" else ""
        tags$tr(style = bg_style, row_cells)
      })
      
      # Return complete preview
      tagList(
        div(
          class = "alert alert-info mb-2",
          style = "padding: 0.5rem 1rem;",
          icon("info-circle"), " ", info_msg
        ),
        div(
          style = "overflow-x: auto; max-height: 200px; border: 1px solid #dee2e6; border-radius: 4px;",
          tags$table(
            class = "table table-sm mb-0",
            style = "width: 100%;",
            tags$thead(
              style = "position: sticky; top: 0; z-index: 1;",
              tags$tr(header_cells)
            ),
            tags$tbody(data_rows)
          )
        ),
        tags$small(
          class = "text-muted",
          sprintf("Showing first %d of %d rows", nrow(preview_df), result$nrow)
        )
      )
    })
    
    
    # =========================================================================
    # SECTION 7: ADVANCED OPTIONS TOGGLE
    # =========================================================================
    
    observeEvent(input$toggle_advanced, {
      shinyjs::toggle("advanced_options")
    })
    
    
    # =========================================================================
    # SECTION 8: CONFIRM UPLOAD
    # =========================================================================
    
    observeEvent(input$confirm_upload, {
      
      req(input$file_upload)
      
      file_info <- input$file_upload
      sheet <- selected_sheet()
      
      cat("\n")
      cat("========================================\n")
      cat("[UPLOAD_MODAL] CONFIRM UPLOAD\n")
      cat(sprintf("[UPLOAD_MODAL] File: %s\n", file_info$name))
      cat(sprintf("[UPLOAD_MODAL] Sheet: %s\n", if (is.null(sheet)) "NULL" else sheet))
      cat(sprintf("[UPLOAD_MODAL] Temp: %d - %d\n", input$temp_min, input$temp_max))
      cat("========================================\n")
      
      # Read the full file
      result <- tryCatch({
        read_thermogram_file(
          filepath = file_info$datapath,
          temp_min = input$temp_min,
          temp_max = input$temp_max,
          sheet = sheet
        )
      }, error = function(e) {
        cat(sprintf("[UPLOAD_MODAL] ERROR: %s\n", e$message))
        showNotification(
          sprintf("Error reading file: %s", e$message), 
          type = "error", 
          duration = 5
        )
        return(NULL)
      })
      
      if (is.null(result)) {
        cat("[UPLOAD_MODAL] Read failed - aborting\n")
        return()
      }
      
      cat(sprintf("[UPLOAD_MODAL] Read %d samples successfully\n", 
                  result$format_info$n_samples))
      
      # Package result for parent
      upload_result <- list(
        data = result$data,
        format_info = result$format_info,
        file_name = file_info$name,
        sheet = sheet,
        params = list(
          temp_min = input$temp_min,
          temp_max = input$temp_max,
          window_size = input$window_size,
          exclusion_lower = input$exclusion_lower,
          exclusion_upper = input$exclusion_upper,
          grid_resolution = input$grid_resolution,
          point_selection = input$point_selection
        ),
        timestamp = Sys.time()
      )
      
      # Update reactive (parent observes this)
      uploaded_data(upload_result)
      
      # Show success
      success_msg <- sprintf("Uploaded: %s (%d samples)", 
                             file_info$name, 
                             result$format_info$n_samples)
      if (!is.null(sheet)) {
        success_msg <- paste0(success_msg, sprintf(" [Sheet: '%s']", sheet))
      }
      showNotification(success_msg, type = "message", duration = 3)
      
      cat("[UPLOAD_MODAL] Complete - closing modal\n")
      cat("========================================\n\n")
      
      removeModal()
    })
    
    
    # =========================================================================
    # RETURN VALUE
    # =========================================================================
    
    return(uploaded_data)
  })
}