# Data Overview Module
# Displays summary cards, file upload, processing, and saved data management

#' Data Overview UI
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI elements
mod_data_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Page header
    div(
      class = "container-fluid p-3",
      
      # Welcome section
      div(
        class = "card mb-3",
        div(
          class = "card-body",
          h2("Welcome to ThermogramForge"),
          p(
            "Upload thermogram data to begin analysis. ThermogramForge provides ",
            "automated baseline detection, signal quality assessment, and comprehensive ",
            "metric calculation for thermal liquid biopsy (TLB) data."
          )
        )
      ),
      
      # Summary cards row
      div(
        class = "row mb-3",
        
        # Raw data card
        div(
          class = "col-md-4",
          div(
            class = "card",
            div(
              class = "card-header",
              icon("database"), " Raw Data Files"
            ),
            div(
              class = "card-body text-center",
              h1(
                class = "display-4 text-primary",
                textOutput(ns("raw_count"))
              ),
              p(class = "text-muted", "files uploaded")
            )
          )
        ),
        
        # Processed data card
        div(
          class = "col-md-4",
          div(
            class = "card",
            div(
              class = "card-header",
              icon("cogs"), " Processed Samples"
            ),
            div(
              class = "card-body text-center",
              h1(
                class = "display-4 text-success",
                textOutput(ns("processed_count"))
              ),
              p(class = "text-muted", "samples processed")
            )
          )
        ),
        
        # Reports card
        div(
          class = "col-md-4",
          div(
            class = "card",
            div(
              class = "card-header",
              icon("file-export"), " Reports Generated"
            ),
            div(
              class = "card-body text-center",
              h1(
                class = "display-4 text-info",
                textOutput(ns("reports_count"))
              ),
              p(class = "text-muted", "reports created")
            )
          )
        )
      ),
      
      # Upload section
      div(
        class = "card",
        div(
          class = "card-header",
          icon("upload"), " Upload Data"
        ),
        div(
          class = "card-body",
          p(
            "Upload CSV or Excel files containing thermogram data. ",
            "Files should contain Temperature and dCp columns."
          ),
          div(
            class = "d-flex gap-2",
            actionButton(
              ns("upload_btn"),
              "Upload New Raw Thermogram Data",
              icon = icon("upload"),
              class = "btn-primary btn-lg"
            ),
            uiOutput(ns("process_button_ui"))
          )
        )
      ),
      
      # Uploaded files list
      div(
        class = "card mt-3",
        div(
          class = "card-header",
          icon("list"), " Uploaded Files"
        ),
        div(
          class = "card-body",
          uiOutput(ns("uploaded_files_list"))
        )
      ),
      
      # ===================================================================
      # Processed Thermograms Section
      # ===================================================================
      div(
        class = "card mt-3",
        div(
          class = "card-header d-flex justify-content-between align-items-center",
          div(
            icon("folder-open"), " Saved Processed Datasets"
          ),
          actionButton(
            ns("refresh_saved_files"),
            "Refresh",
            icon = icon("sync"),
            class = "btn-sm btn-secondary"
          )
        ),
        div(
          class = "card-body",
          p(
            class = "text-muted",
            icon("info-circle"),
            " RDS files contain full data for Review Endpoints and Reports. ",
            "CSV/Excel files can be loaded for report generation only."
          ),
          uiOutput(ns("saved_files_ui"))
        )
      )
    )
  )
}

#' Data Overview Server
#'
#' @param id Module namespace ID
#' @param app_data Reactive values object containing application data
#'
#' @return Server logic (no return value)
mod_data_overview_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Initialize uploaded files tracker
    uploaded_files <- reactiveVal(list())
    
    # Reactive for saved files list (refreshable)
    saved_files_trigger <- reactiveVal(0)
    
    saved_files_df <- reactive({
      # Depend on trigger for refresh
      saved_files_trigger()
      
      # Get list of saved processed datasets
      list_processed_datasets()
    })
    
    # =========================================================================
    # SUMMARY COUNTS
    # =========================================================================
    
    output$raw_count <- renderText({
      length(uploaded_files())
    })
    
    output$processed_count <- renderText({
      if (is.null(app_data$processed_data)) {
        "0"
      } else {
        # Handle both full and report-only data
        if (!is.null(app_data$processed_data$summary)) {
          as.character(app_data$processed_data$summary$n_success)
        } else if (!is.null(app_data$processed_data$n_samples)) {
          as.character(app_data$processed_data$n_samples)
        } else {
          "0"
        }
      }
    })
    
    output$reports_count <- renderText({
      "0"
    })
    
    # =========================================================================
    # RAW DATA UPLOAD (existing functionality - PRESERVED)
    # =========================================================================
    
    # Upload button handler
    observeEvent(input$upload_btn, {
      showModal(
        modalDialog(
          title = tagList(icon("upload"), " Upload Thermogram Data"),
          size = "l",
          easyClose = FALSE,
          
          # File input
          div(
            class = "mb-3",
            fileInput(
              ns("file_upload"),
              label = NULL,
              multiple = FALSE,
              accept = c(".csv", ".xlsx", ".xls"),
              buttonLabel = "Choose File",
              placeholder = "Drop file here or click to browse"
            )
          ),
          
          uiOutput(ns("upload_status")),
          uiOutput(ns("data_preview")),
          
          footer = tagList(
            actionButton(ns("cancel_upload"), "Cancel", class = "btn-secondary"),
            actionButton(ns("confirm_upload"), "Add to Dataset", class = "btn-primary", icon = icon("check"))
          )
        )
      )
    })
    
    upload_result <- reactive({
      req(input$file_upload)
      tryCatch({
        process_upload(input$file_upload$datapath, input$file_upload$name)
      }, error = function(e) {
        list(success = FALSE, validation = list(valid = FALSE, errors = c(as.character(e)), warnings = character()))
      })
    })
    
    output$upload_status <- renderUI({
      req(upload_result())
      result <- upload_result()
      
      if (result$success) {
        format_desc <- switch(
          result$format_info$format_type,
          "single_sample" = "Single sample",
          "multi_sample_long" = "Multiple samples (long format)",
          "multi_sample_wide" = "Multiple samples (wide format)",
          "Unknown format"
        )
        
        if (length(result$format_info$data_points) > 1) {
          data_point_range <- sprintf("%d - %d", min(result$format_info$data_points), max(result$format_info$data_points))
        } else {
          data_point_range <- as.character(result$format_info$data_points[1])
        }
        
        sample_ids_display <- NULL
        if (!is.null(result$format_info$sample_ids)) {
          n_ids <- length(result$format_info$sample_ids)
          if (n_ids <= 10) {
            sample_ids_display <- paste(result$format_info$sample_ids, collapse = ", ")
          } else {
            sample_ids_display <- paste(paste(result$format_info$sample_ids[1:10], collapse = ", "), "...")
          }
        }
        
        div(
          class = "alert alert-success",
          icon("check-circle"), " File uploaded successfully!",
          tags$hr(),
          tags$ul(
            tags$li(strong("Format:"), " ", format_desc),
            tags$li(strong("Samples:"), " ", result$format_info$n_samples),
            tags$li(strong("Data points per sample:"), " ", data_point_range),
            if (!is.null(sample_ids_display)) tags$li(strong("Sample IDs:"), " ", sample_ids_display)
          )
        )
      } else {
        div(
          class = "alert alert-danger",
          icon("exclamation-triangle"), " File validation failed",
          tags$hr(),
          tags$ul(lapply(result$validation$errors, function(e) tags$li(e)))
        )
      }
    })
    
    output$data_preview <- renderUI({
      req(upload_result())
      result <- upload_result()
      
      if (result$success && !is.null(result$data)) {
        div(
          class = "mt-3",
          h5("Data Preview (first 10 rows)"),
          div(style = "max-height: 300px; overflow-y: auto;", DT::dataTableOutput(ns("preview_table")))
        )
      }
    })
    
    output$preview_table <- DT::renderDataTable({
      req(upload_result())
      result <- upload_result()
      
      if (result$success && !is.null(result$data)) {
        DT::datatable(head(result$data, 10), options = list(dom = 't', pageLength = 10, scrollX = TRUE), rownames = FALSE, class = "compact")
      }
    })
    
    observeEvent(input$confirm_upload, {
      req(upload_result())
      result <- upload_result()
      
      if (result$success) {
        current_files <- uploaded_files()
        current_files[[length(current_files) + 1]] <- result
        uploaded_files(current_files)
        app_data$raw_data <- result$data
        showNotification(paste("Successfully added", result$file_name), type = "message", duration = 3)
        removeModal()
      } else {
        showNotification("Cannot add invalid file", type = "error", duration = 3)
      }
    })
    
    observeEvent(input$cancel_upload, { removeModal() })
    
    # =========================================================================
    # PROCESS DATA
    # =========================================================================
    
    output$process_button_ui <- renderUI({
      req(app_data$raw_data)
      actionButton(ns("process_btn"), "Process Data", icon = icon("cogs"), class = "btn-success btn-lg")
    })
    
    observeEvent(input$process_btn, {
      req(app_data$raw_data)
      files <- uploaded_files()
      if (length(files) == 0) return()
      recent_file <- files[[length(files)]]
      
      showModal(
        modalDialog(
          title = tagList(icon("cogs"), " Processing Thermogram Data"),
          size = "m",
          easyClose = FALSE,
          footer = NULL,
          div(
            class = "text-center",
            h5("Running baseline detection and signal quality assessment..."),
            br(),
            div(class = "spinner-border text-primary", role = "status", style = "width: 3rem; height: 3rem;", tags$span(class = "visually-hidden", "Processing...")),
            br(), br(),
            p(class = "text-muted", "This may take a moment for large datasets.")
          )
        )
      )
      
      Sys.sleep(0.1)
      
      result <- process_thermogram_data(recent_file$data, recent_file$format_info, progress_callback = NULL)
      app_data$processed_data <- result
      app_data$baseline_results <- result$samples
      
      signal_summary <- list()
      for (sample_id in names(result$samples)) {
        sample <- result$samples[[sample_id]]
        if (sample$success) signal_summary[[sample_id]] <- sample$has_signal
      }
      app_data$signal_detection <- signal_summary
      
      removeModal()
      
      showModal(
        modalDialog(
          title = tagList(icon("check-circle", class = "text-success"), " Processing Complete"),
          size = "m",
          easyClose = TRUE,
          div(
            class = "alert alert-success",
            h5("Successfully processed thermogram data!"),
            tags$hr(),
            tags$ul(
              tags$li(sprintf("Total samples: %d", result$summary$n_total)),
              tags$li(sprintf("Successfully processed: %d", result$summary$n_success)),
              tags$li(sprintf("Failed: %d", result$summary$n_failed)),
              tags$li(sprintf("With signal: %d", result$summary$n_signal), tags$small(class = "text-muted ms-2", "(good quality)")),
              tags$li(sprintf("No signal detected: %d", result$summary$n_no_signal), tags$small(class = "text-warning ms-2", "(noise only)"))
            )
          ),
          footer = tagList(
            actionButton(ns("goto_review"), "Go to Review Endpoints", icon = icon("arrow-right"), class = "btn-primary"),
            modalButton("Close")
          )
        )
      )
    })
    
    observeEvent(input$goto_review, {
      removeModal()
      app_data$navigate_to <- "review_endpoints"
    })
    
    output$uploaded_files_list <- renderUI({
      files <- uploaded_files()
      
      if (length(files) == 0) {
        return(p(class = "text-muted text-center", "No files uploaded yet. Click the button above to get started."))
      }
      
      div(
        lapply(seq_along(files), function(i) {
          file_info <- files[[i]]
          div(
            class = "card mb-2",
            div(
              class = "card-body p-2",
              div(
                class = "d-flex justify-content-between align-items-center",
                div(
                  icon("file"), " ", tags$strong(file_info$file_name),
                  tags$small(class = "text-muted ms-2", sprintf("(%d samples, %d rows)", file_info$format_info$n_samples, file_info$format_info$n_rows))
                ),
                tags$small(class = "text-muted", format(file_info$upload_time, "%Y-%m-%d %H:%M"))
              )
            )
          )
        })
      )
    })
    
    # =========================================================================
    # SAVED PROCESSED DATASETS
    # =========================================================================
    
    observe({ isolate(saved_files_trigger(1)) })
    
    observeEvent(input$refresh_saved_files, {
      saved_files_trigger(saved_files_trigger() + 1)
      showNotification("Refreshed saved datasets list", type = "message", duration = 2)
    })
    
    output$saved_files_ui <- renderUI({
      df <- saved_files_df()
      
      if (nrow(df) == 0) {
        return(
          div(
            class = "text-center text-muted py-4",
            icon("folder-open", style = "font-size: 3em; opacity: 0.3;"),
            p("No saved processed datasets found"),
            p(class = "small", "Process data and save from the Review Endpoints tab to see datasets here")
          )
        )
      }
      
      rows <- lapply(seq_len(nrow(df)), function(i) {
        file_info <- df[i, ]
        
        # All files can be loaded now
        action_buttons <- tagList(
          actionButton(
            ns(paste0("load_", i)),
            "Load",
            icon = icon("folder-open"),
            class = "btn-primary btn-sm me-2",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("load_file_index"), i)
          ),
          actionButton(
            ns(paste0("delete_", i)),
            "Delete",
            icon = icon("trash"),
            class = "btn-outline-danger btn-sm",
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("delete_file_index"), i)
          )
        )
        
        div(
          class = "d-flex justify-content-between align-items-center mb-2 p-3 border rounded",
          div(
            class = "flex-grow-1",
            div(
              icon("file"), " ", strong(file_info$filename),
              tags$span(class = sprintf("badge bg-%s ms-2", if(file_info$format == "RDS") "success" else "info"), file_info$format),
              tags$span(class = "badge bg-secondary ms-2", file_info$load_type)
            ),
            div(class = "small text-muted mt-1", sprintf("Modified: %s | Size: %.2f MB", file_info$modified, file_info$size_mb))
          ),
          div(class = "flex-shrink-0", action_buttons)
        )
      })
      
      tagList(rows)
    })
    
    # =========================================================================
    # LOAD FILE HANDLER
    # =========================================================================
    
    observeEvent(input$load_file_index, {
      req(input$load_file_index)
      
      df <- saved_files_df()
      file_index <- input$load_file_index
      
      if (file_index < 1 || file_index > nrow(df)) {
        showNotification("Invalid file selection", type = "error", duration = 3)
        return()
      }
      
      file_info <- df[file_index, ]
      
      showModal(
        modalDialog(
          title = tagList(icon("folder-open"), " Load Processed Dataset"),
          size = "m",
          
          p("Are you sure you want to load this dataset?"),
          
          div(
            class = "alert alert-info",
            icon("info-circle"),
            tags$ul(
              class = "mb-0",
              tags$li(strong("File: "), file_info$filename),
              tags$li(strong("Format: "), file_info$format),
              tags$li(strong("Load Type: "), file_info$load_type),
              tags$li(strong("Modified: "), file_info$modified),
              tags$li(strong("Size: "), sprintf("%.2f MB", file_info$size_mb))
            )
          ),
          
          if (file_info$format == "RDS") {
            div(
              class = "alert alert-success",
              icon("check-circle"),
              " This RDS file contains full data and will be loaded into Review Endpoints."
            )
          } else {
            div(
              class = "alert alert-warning",
              icon("info-circle"),
              " This file contains report-ready data only. Use Report Builder to generate reports."
            )
          },
          
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            " Loading will replace any currently loaded data."
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_load"), "Load Dataset", class = "btn-primary", icon = icon("check"))
          ),
          easyClose = FALSE
        )
      )
      
      session$userData$load_filepath <- file_info$filepath
      session$userData$load_format <- file_info$format
    })
    
    observeEvent(input$confirm_load, {
      req(session$userData$load_filepath)
      
      filepath <- session$userData$load_filepath
      file_format <- session$userData$load_format
      
      cat(sprintf("[LOAD] Attempting to load: %s\n", filepath))
      
      result <- load_processed_data(filepath)
      
      if (result$success) {
        app_data$processed_data <- result$data
        
        cat(sprintf("[LOAD] Successfully loaded - Type: %s\n", result$data_type))
        
        showNotification(result$message, type = "message", duration = 5)
        
        # Only navigate to Review Endpoints for RDS (full data)
        if (result$data_type == "full") {
          app_data$navigate_to <- "review_endpoints"
          cat("[LOAD] Navigating to Review Endpoints\n")
        } else {
          cat("[LOAD] Report-only data - staying on Data Overview\n")
        }
        
        removeModal()
        
      } else {
        showNotification(result$message, type = "error", duration = 5)
        cat(sprintf("[LOAD] Failed: %s\n", result$message))
        removeModal()
      }
      
      session$userData$load_filepath <- NULL
      session$userData$load_format <- NULL
    })
    
    # =========================================================================
    # DELETE FILE HANDLER
    # =========================================================================
    
    observeEvent(input$delete_file_index, {
      req(input$delete_file_index)
      
      df <- saved_files_df()
      file_index <- input$delete_file_index
      
      if (file_index < 1 || file_index > nrow(df)) {
        showNotification("Invalid file selection", type = "error", duration = 3)
        return()
      }
      
      file_info <- df[file_index, ]
      
      showModal(
        modalDialog(
          title = tagList(icon("trash"), " Delete Processed Dataset"),
          size = "m",
          
          div(class = "alert alert-danger", icon("exclamation-triangle"), strong(" Warning: This action cannot be undone!")),
          p("Are you sure you want to permanently delete this file?"),
          div(class = "alert alert-secondary", tags$ul(class = "mb-0", tags$li(strong("File: "), file_info$filename), tags$li(strong("Modified: "), file_info$modified))),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_delete"), "Delete Permanently", class = "btn-danger", icon = icon("trash"))
          ),
          easyClose = FALSE
        )
      )
      
      session$userData$delete_filepath <- file_info$filepath
    })
    
    observeEvent(input$confirm_delete, {
      req(session$userData$delete_filepath)
      
      filepath <- session$userData$delete_filepath
      
      cat(sprintf("[DELETE] Attempting to delete: %s\n", filepath))
      
      result <- delete_processed_data(filepath)
      
      if (result$success) {
        showNotification(result$message, type = "message", duration = 3)
        saved_files_trigger(saved_files_trigger() + 1)
        cat(sprintf("[DELETE] Successfully deleted: %s\n", basename(filepath)))
      } else {
        showNotification(result$message, type = "error", duration = 5)
        cat(sprintf("[DELETE] Failed: %s\n", result$message))
      }
      
      removeModal()
      session$userData$delete_filepath <- NULL
    })
    
  })
}