# Data Overview Module - REDESIGNED (Bug Fixes)
# Multi-dataset support with improved UX

#' Data Overview UI
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI elements
mod_data_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
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
              p(class = "text-muted", "unprocessed datasets")
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
              icon("cogs"), " Processed Datasets"
            ),
            div(
              class = "card-body text-center",
              h1(
                class = "display-4 text-success",
                textOutput(ns("processed_count"))
              ),
              p(class = "text-muted", "datasets ready for review/reports")
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
      
      # ===================================================================
      # UPLOADED FILES SECTION (Combined raw + processed)
      # ===================================================================
      div(
        class = "card mb-3",
        div(
          class = "card-header d-flex justify-content-between align-items-center",
          div(
            icon("list"), " Uploaded Datasets"
          ),
          actionButton(
            ns("upload_btn"),
            "Upload New Raw Thermogram Data",
            icon = icon("upload"),
            class = "btn-primary"
          )
        ),
        div(
          class = "card-body",
          
          # Unprocessed files section
          h5(icon("database"), " Raw Thermogram Data (Unprocessed)"),
          uiOutput(ns("unprocessed_files_list")),
          
          tags$hr(),
          
          # Processed files section
          h5(icon("check-circle"), " Processed Thermogram Data"),
          uiOutput(ns("processed_files_list"))
        )
      ),
      
      # ===================================================================
      # SAVED FILES SECTION (from disk)
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
            " Load previously saved datasets from disk. ",
            "RDS files contain full data for Review Endpoints. ",
            "CSV/Excel files are for report generation only."
          ),
          uiOutput(ns("saved_files_ui"))
        )
      ),
      div(
        class = "card mt-3",
        div(
          class = "card-header",
          icon("file-chart-line"), " Generated Reports"
        ),
        div(
          class = "card-body",
          p(
            class = "text-muted",
            icon("info-circle"),
            " Reports generated during this session. ",
            "Download or delete reports as needed."
          ),
          uiOutput(ns("generated_reports_list"))
        )
      ),
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
    
    # =========================================================================
    # DATA STRUCTURES
    # =========================================================================
    
    # Track all uploaded files (both processed and unprocessed)
    uploaded_datasets <- reactiveVal(list())
    
    # Counter for unique IDs
    dataset_counter <- reactiveVal(0)
    
    # Track which observers have been created (to prevent duplicates)
    created_observers <- reactiveVal(character(0))
    
    # Reactive for saved files list (refreshable)
    saved_files_trigger <- reactiveVal(0)
    
    saved_files_df <- reactive({
      saved_files_trigger()
      list_processed_datasets()
    })
    
    # =========================================================================
    # SUMMARY COUNTS (BUG FIX: Handle empty list)
    # =========================================================================
    
    output$raw_count <- renderText({
      datasets <- uploaded_datasets()
      
      # Handle empty list case
      if (length(datasets) == 0) return("0")
      
      # Count unprocessed
      count <- length(Filter(function(d) d$status == "unprocessed", datasets))
      as.character(count)
    })
    
    output$processed_count <- renderText({
      datasets <- uploaded_datasets()
      
      # Handle empty list case
      if (length(datasets) == 0) return("0")
      
      # Count processed and loaded
      count <- length(Filter(function(d) d$status %in% c("processed", "loaded"), datasets))
      as.character(count)
    })
    
    output$reports_count <- renderText({
      reports <- app_data$generated_reports
      if (length(reports) == 0) return("0")
      as.character(length(reports))
    })
    
    # =========================================================================
    # RAW DATA UPLOAD
    # =========================================================================
    
    observeEvent(input$upload_btn, {
      showModal(
        modalDialog(
          title = tagList(icon("upload"), " Upload Raw Thermogram Data"),
          size = "l",
          easyClose = FALSE,
          
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
            actionButton(ns("confirm_upload"), "Add Dataset", class = "btn-primary", icon = icon("check"))
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
        # Generate unique ID
        new_id <- dataset_counter() + 1
        dataset_counter(new_id)
        
        # Create dataset entry
        new_dataset <- list(
          id = sprintf("dataset_%d", new_id),
          file_name = result$file_name,
          data = result$data,
          format_info = result$format_info,
          upload_time = Sys.time(),
          status = "unprocessed",
          processed_data = NULL
        )
        
        # Add to list
        current_datasets <- uploaded_datasets()
        current_datasets[[length(current_datasets) + 1]] <- new_dataset
        uploaded_datasets(current_datasets)
        
        showNotification(paste("Successfully added", result$file_name), type = "message", duration = 3)
        removeModal()
      } else {
        showNotification("Cannot add invalid file", type = "error", duration = 3)
      }
    })
    
    observeEvent(input$cancel_upload, { removeModal() })
    
    # =========================================================================
    # BUTTON CLICK DETECTION (FIX: Track processed clicks)
    # =========================================================================
    
    # Track which button clicks we've already processed
    processed_clicks <- reactiveVal(list())
    
    observe({
      datasets <- uploaded_datasets()
      if (length(datasets) == 0) return()
      
      input_names <- names(input)
      clicks <- processed_clicks()
      
      # Check for process button clicks
      process_buttons <- grep("^process_dataset_", input_names, value = TRUE)
      for (btn in process_buttons) {
        click_count <- input[[btn]]
        if (!is.null(click_count) && click_count > 0) {
          # Only process if this is a NEW click
          if (is.null(clicks[[btn]]) || clicks[[btn]] < click_count) {
            # Update tracking
            clicks[[btn]] <- click_count
            processed_clicks(clicks)
            
            # Extract dataset ID and process
            dataset_id <- sub("^process_", "", btn)
            dataset_idx <- which(sapply(datasets, function(d) d$id == dataset_id))
            
            if (length(dataset_idx) > 0 && datasets[[dataset_idx]]$status == "unprocessed") {
              dataset <- datasets[[dataset_idx]]
              
              showModal(modalDialog(
                title = tagList(icon("cogs"), " Processing Thermogram Data"),
                size = "m", easyClose = FALSE, footer = NULL,
                div(class = "text-center",
                    h5(sprintf("Processing: %s", dataset$file_name)),
                    br(),
                    div(class = "spinner-border text-primary", role = "status", style = "width: 3rem; height: 3rem;"),
                    br(), br(),
                    p(class = "text-muted", "Running baseline detection..."))
              ))
              
              Sys.sleep(0.1)
              result <- process_thermogram_data(dataset$data, dataset$format_info, NULL)
              
              current_datasets <- uploaded_datasets()
              current_datasets[[dataset_idx]]$status <- "processed"
              current_datasets[[dataset_idx]]$processed_data <- result
              uploaded_datasets(current_datasets)
              
              removeModal()
              showNotification(sprintf("Processed %s (%d samples)", dataset$file_name, result$summary$n_success), 
                               type = "message", duration = 5)
            }
          }
        }
      }
      
      # Check for review button clicks
      review_buttons <- grep("^review_", input_names, value = TRUE)
      for (btn in review_buttons) {
        click_count <- input[[btn]]
        if (!is.null(click_count) && click_count > 0) {
          # Only process if this is a NEW click
          if (is.null(clicks[[btn]]) || clicks[[btn]] < click_count) {
            # Update tracking
            clicks[[btn]] <- click_count
            processed_clicks(clicks)
            
            # Extract dataset ID and load
            dataset_id <- sub("^review_", "", btn)
            dataset_idx <- which(sapply(datasets, function(d) d$id == dataset_id))
            
            if (length(dataset_idx) > 0) {
              isolate({
                app_data$processed_data <- datasets[[dataset_idx]]$processed_data
                
                # ===== ADDED FOR BUG FIX =====
                app_data$current_dataset_id <- datasets[[dataset_idx]]$id
                app_data$current_dataset_name <- datasets[[dataset_idx]]$file_name
                app_data$dataset_load_trigger <- isolate(app_data$dataset_load_trigger) + 1
                # =============================
                
                app_data$navigate_to <- "review_endpoints"
              })
            }
          }
        }
      }
      
      # Check for report button clicks
      report_buttons <- grep("^report_", input_names, value = TRUE)
      for (btn in report_buttons) {
        click_count <- input[[btn]]
        if (!is.null(click_count) && click_count > 0) {
          # Only process if this is a NEW click
          if (is.null(clicks[[btn]]) || clicks[[btn]] < click_count) {
            # Update tracking
            clicks[[btn]] <- click_count
            processed_clicks(clicks)
            
            # Extract dataset ID and load
            dataset_id <- sub("^report_", "", btn)
            dataset_idx <- which(sapply(datasets, function(d) d$id == dataset_id))
            
            if (length(dataset_idx) > 0) {
              isolate({
                app_data$processed_data <- datasets[[dataset_idx]]$processed_data
                
                # ===== ADDED FOR BUG FIX =====
                app_data$current_dataset_id <- datasets[[dataset_idx]]$id
                app_data$current_dataset_name <- datasets[[dataset_idx]]$file_name
                app_data$dataset_load_trigger <- isolate(app_data$dataset_load_trigger) + 1
                # =============================
                
                app_data$navigate_to <- "report_builder"
              })
            }
          }
        }
      }
    })
    
    # =========================================================================
    # RENDER UNPROCESSED FILES LIST
    # =========================================================================
    
    output$unprocessed_files_list <- renderUI({
      datasets <- uploaded_datasets()
      unprocessed <- Filter(function(d) d$status == "unprocessed", datasets)
      
      if (length(unprocessed) == 0) {
        return(
          p(class = "text-muted", icon("info-circle"), " No unprocessed datasets. Upload raw data to get started.")
        )
      }
      
      rows <- lapply(datasets, function(dataset) {
        if (dataset$status != "unprocessed") return(NULL)
        
        div(
          class = "card mb-2 border-primary",
          div(
            class = "card-body p-3",
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                class = "flex-grow-1",
                div(
                  icon("file"), " ",
                  strong(dataset$file_name),
                  tags$span(class = "badge bg-warning text-dark ms-2", "Unprocessed")
                ),
                div(
                  class = "small text-muted mt-1",
                  sprintf(
                    "%d samples | Uploaded: %s",
                    dataset$format_info$n_samples,
                    format(dataset$upload_time, "%Y-%m-%d %H:%M")
                  )
                )
              ),
              div(
                class = "flex-shrink-0",
                actionButton(
                  ns(paste0("process_", dataset$id)),
                  "Process Data",
                  icon = icon("cogs"),
                  class = "btn-success btn-sm"
                )
              )
            )
          )
        )
      })
      
      # Filter out NULLs
      rows <- Filter(Negate(is.null), rows)
      tagList(rows)
    })
    
    # =========================================================================
    # RENDER PROCESSED FILES LIST
    # =========================================================================
    
    output$processed_files_list <- renderUI({
      datasets <- uploaded_datasets()
      processed <- Filter(function(d) d$status %in% c("processed", "loaded"), datasets)
      
      if (length(processed) == 0) {
        return(
          p(class = "text-muted", icon("info-circle"), " No processed datasets yet. Process raw data or load saved datasets.")
        )
      }
      
      rows <- lapply(datasets, function(dataset) {
        if (!dataset$status %in% c("processed", "loaded")) return(NULL)
        
        # Determine label and badge based on status
        if (dataset$status == "loaded") {
          status_label <- "Processed Thermogram Data"
          badge_class <- "bg-info"
          badge_text <- "Loaded"
        } else {
          status_label <- "Raw Thermogram Data (Processed)"
          badge_class <- "bg-success"
          badge_text <- "Processed"
        }
        
        # Get sample count
        n_samples <- if (!is.null(dataset$processed_data$summary)) {
          dataset$processed_data$summary$n_success
        } else if (!is.null(dataset$processed_data$n_samples)) {
          dataset$processed_data$n_samples
        } else {
          "?"
        }
        
        div(
          class = "card mb-2 border-success",
          div(
            class = "card-body p-3",
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                class = "flex-grow-1",
                div(
                  icon("check-circle"), " ",
                  strong(dataset$file_name),
                  tags$span(class = sprintf("badge %s ms-2", badge_class), badge_text)
                ),
                div(
                  class = "small text-muted mt-1",
                  sprintf(
                    "%s samples | %s: %s",
                    n_samples,
                    if(dataset$status == "loaded") "Loaded" else "Processed",
                    format(dataset$upload_time, "%Y-%m-%d %H:%M")
                  )
                )
              ),
              div(
                class = "flex-shrink-0",
                div(
                  class = "btn-group",
                  actionButton(
                    ns(paste0("review_", dataset$id)),
                    "Review Endpoints",
                    icon = icon("chart-line"),
                    class = "btn-primary btn-sm"
                  ),
                  actionButton(
                    ns(paste0("report_", dataset$id)),
                    "Create Reports",
                    icon = icon("file-export"),
                    class = "btn-info btn-sm"
                  )
                )
              )
            )
          )
        )
      })
      
      # Filter out NULLs
      rows <- Filter(Negate(is.null), rows)
      tagList(rows)
    })
    
    # =========================================================================
    # SAVED FILES SECTION
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
            p(class = "small", "Save processed datasets from the Review Endpoints tab")
          )
        )
      }
      
      rows <- lapply(seq_len(nrow(df)), function(i) {
        file_info <- df[i, ]
        
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
          
          p("Load this dataset into the application?"),
          
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
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_load"), "Load Dataset", class = "btn-primary", icon = icon("check"))
          ),
          easyClose = FALSE
        )
      )
      
      session$userData$load_filepath <- file_info$filepath
      session$userData$load_filename <- file_info$filename
    })
    
    observeEvent(input$confirm_load, {
      req(session$userData$load_filepath)
      
      filepath <- session$userData$load_filepath
      filename <- session$userData$load_filename
      
      cat(sprintf("[LOAD] Loading from disk: %s\n", filepath))
      
      result <- load_processed_data(filepath)
      
      if (result$success) {
        # Create new dataset entry
        new_id <- dataset_counter() + 1
        dataset_counter(new_id)
        
        new_dataset <- list(
          id = sprintf("loaded_%d", new_id),
          file_name = filename,
          data = NULL,  # No raw data for loaded files
          format_info = NULL,
          upload_time = Sys.time(),
          status = "loaded",
          processed_data = result$data
        )
        
        # Add to list
        current_datasets <- uploaded_datasets()
        current_datasets[[length(current_datasets) + 1]] <- new_dataset
        uploaded_datasets(current_datasets)
        
        # ===== ADDED FOR BUG FIX =====
        # Set processed data for immediate use
        app_data$processed_data <- result$data
        app_data$current_dataset_id <- new_dataset$id
        app_data$current_dataset_name <- filename
        app_data$dataset_load_trigger <- isolate(app_data$dataset_load_trigger) + 1
        
        # Navigate based on data type
        if (result$data_type == "full") {
          app_data$navigate_to <- "review_endpoints"
        }
        # =============================
        
        showNotification(result$message, type = "message", duration = 5)
        cat(sprintf("[LOAD] Successfully added to datasets list\n"))
        
        removeModal()
        
      } else {
        showNotification(result$message, type = "error", duration = 5)
        cat(sprintf("[LOAD] Failed: %s\n", result$message))
        removeModal()
      }
      
      session$userData$load_filepath <- NULL
      session$userData$load_filename <- NULL
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
          p("Permanently delete this file from disk?"),
          div(class = "alert alert-secondary", tags$ul(class = "mb-0", tags$li(strong("File: "), file_info$filename))),
          
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
      
      cat(sprintf("[DELETE] Deleting from disk: %s\n", filepath))
      
      result <- delete_processed_data(filepath)
      
      if (result$success) {
        showNotification(result$message, type = "message", duration = 3)
        saved_files_trigger(saved_files_trigger() + 1)
        cat(sprintf("[DELETE] Successfully deleted\n"))
      } else {
        showNotification(result$message, type = "error", duration = 5)
        cat(sprintf("[DELETE] Failed: %s\n", result$message))
      }
      
      removeModal()
      session$userData$delete_filepath <- NULL
    })

    # ---- Generated Reports Display (Phase 8) ----
    
    # Track which report to download (for downloadHandler)
    report_to_download <- reactiveVal(NULL)
    
    output$generated_reports_list <- renderUI({
      
      # Access the generated reports from app_data
      reports <- app_data$generated_reports
      
      if (length(reports) == 0) {
        return(
          div(
            class = "alert alert-info",
            icon("info-circle"), " ",
            "No reports have been generated yet. Use the Report Builder to create reports."
          )
        )
      }
      
      # Sort reports by generation time (most recent first)
      reports_sorted <- reports[order(
        sapply(reports, function(r) as.POSIXct(r$generated_at)),
        decreasing = TRUE
      )]
      
      # Create UI for each report
      report_cards <- lapply(reports_sorted, function(report) {
        
        # Format timestamp
        timestamp <- format(report$generated_at, "%Y-%m-%d %H:%M:%S")
        
        # Format badge based on format
        format_badge <- switch(
          report$format,
          "csv" = "badge bg-primary",
          "xlsx" = "badge bg-success",
          "badge bg-secondary"
        )
        
        div(
          class = "card mb-2",
          div(
            class = "card-body p-3",
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                class = "flex-grow-1",
                div(
                  icon("file-chart-line"), " ",
                  strong(report$name),
                  tags$span(class = sprintf("%s ms-2", format_badge), toupper(report$format))
                ),
                div(
                  class = "small text-muted mt-1",
                  sprintf(
                    "Dataset: %s | %d samples, %d metrics | Generated: %s",
                    report$dataset_name,
                    report$n_samples,
                    report$n_metrics,
                    timestamp
                  )
                )
              ),
              div(
                class = "flex-shrink-0",
                div(
                  class = "btn-group",
                  downloadLink(
                    ns(paste0("download_report_", report$id)),
                    label = tagList(icon("download"), " Download"),
                    class = "btn btn-info btn-sm"
                  ),
                  actionButton(
                    ns(paste0("delete_report_", report$id)),
                    "Delete",
                    icon = icon("trash"),
                    class = "btn-danger btn-sm"
                  )
                )
              )
            )
          )
        )
      })
      
      tagList(report_cards)
    })
    
    # ---- Dynamic Download Handlers ----
    
    observe({
      reports <- app_data$generated_reports
      
      if (length(reports) == 0) return()
      
      # Create download handlers for each report
      lapply(reports, function(report) {
        
        download_id <- paste0("download_report_", report$id)
        
        # Check if handler already exists
        if (download_id %in% created_observers()) return()
        
        output[[download_id]] <- downloadHandler(
          filename = function() {
            basename(report$filepath)
          },
          content = function(file) {
            cat(sprintf("\n[DATA_OVERVIEW] Downloading report: %s\n", report$name))
            
            if (!file.exists(report$filepath)) {
              showNotification(
                sprintf("Report file not found: %s", basename(report$filepath)),
                type = "error",
                duration = 5
              )
              return()
            }
            
            # Copy file to download location
            file.copy(report$filepath, file, overwrite = TRUE)
            
            cat(sprintf("[DATA_OVERVIEW] ✅ Report downloaded: %s\n", basename(report$filepath)))
          }
        )
        
        # Mark as created
        created_observers(c(created_observers(), download_id))
      })
    })
    
    # ---- Dynamic Report Delete Handlers ----
    
    observe({
      reports <- app_data$generated_reports
      
      if (length(reports) == 0) return()
      
      lapply(reports, function(report) {
        
        button_id <- paste0("delete_report_", report$id)
        
        # Check if observer already exists
        observer_key <- paste0("delete_obs_", report$id)
        if (observer_key %in% created_observers()) return()
        
        observeEvent(input[[button_id]], {
          
          cat(sprintf("\n[DATA_OVERVIEW] Delete report button clicked: %s\n", report$id))
          
          # Show confirmation modal
          showModal(
            modalDialog(
              title = "Delete Report",
              div(
                p(strong("Are you sure you want to delete this report?")),
                p(sprintf("Report: %s", report$name)),
                p(sprintf("Format: %s", toupper(report$format))),
                p(sprintf("File: %s", basename(report$filepath))),
                div(
                  class = "alert alert-warning",
                  icon("exclamation-triangle"), " ",
                  "This action cannot be undone."
                )
              ),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(
                  ns(paste0("confirm_delete_report_", report$id)),
                  "Delete Report",
                  class = "btn-danger",
                  icon = icon("trash")
                )
              ),
              easyClose = TRUE
            )
          )
          
        }, ignoreInit = TRUE)
        
        # Mark as created
        created_observers(c(created_observers(), observer_key))
      })
    })
    
    # ---- Confirm Delete Report ----
    
    observe({
      reports <- app_data$generated_reports
      
      if (length(reports) == 0) return()
      
      lapply(reports, function(report) {
        
        button_id <- paste0("confirm_delete_report_", report$id)
        
        # Check if observer already exists
        observer_key <- paste0("confirm_delete_obs_", report$id)
        if (observer_key %in% created_observers()) return()
        
        observeEvent(input[[button_id]], {
          
          cat(sprintf("\n[DATA_OVERVIEW] Confirming delete for report: %s\n", report$id))
          
          # Try to delete the file
          if (file.exists(report$filepath)) {
            tryCatch({
              file.remove(report$filepath)
              cat(sprintf("[DATA_OVERVIEW] ✅ Deleted file: %s\n", report$filepath))
            }, error = function(e) {
              cat(sprintf("[DATA_OVERVIEW] ❌ Error deleting file: %s\n", e$message))
              showNotification(
                sprintf("Error deleting file: %s", e$message),
                type = "error",
                duration = 5
              )
              return()
            })
          }
          
          # Remove from app_data
          app_data$generated_reports[[report$id]] <- NULL
          
          cat(sprintf("[DATA_OVERVIEW] Report removed from session\n"))
          cat(sprintf("[DATA_OVERVIEW] Remaining reports: %d\n", 
                      length(app_data$generated_reports)))
          
          # Close modal
          removeModal()
          
          # Show success notification
          showNotification(
            sprintf("Report deleted: %s", report$name),
            type = "message",
            duration = 3
          )
          
        }, ignoreInit = TRUE)
        
        # Mark as created
        created_observers(c(created_observers(), observer_key))
      })
    })
    NULL
  })
}