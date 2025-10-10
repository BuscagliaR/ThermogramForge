# Data Overview Module
# Displays summary cards and file upload interface

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
          actionButton(
            ns("upload_btn"),
            "Upload New Raw Thermogram Data",
            icon = icon("upload"),
            class = "btn-primary btn-lg"
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
    
    # Render summary counts
    output$raw_count <- renderText({
      length(uploaded_files())
    })
    
    output$processed_count <- renderText({
      if (is.null(app_data$processed_data)) {
        "0"
      } else {
        # TODO: Count processed samples properly
        "0"
      }
    })
    
    output$reports_count <- renderText({
      # TODO: Track generated reports
      "0"
    })
    
    # Upload button handler
    observeEvent(input$upload_btn, {
      showModal(
        modalDialog(
          title = tagList(icon("upload"), " Upload Thermogram Data"),
          size = "l",
          easyClose = FALSE,
          
          # File input with drag-and-drop styling
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
          
          # File info display
          uiOutput(ns("upload_status")),
          
          # Preview of uploaded data (if successful)
          uiOutput(ns("data_preview")),
          
          footer = tagList(
            actionButton(
              ns("cancel_upload"),
              "Cancel",
              class = "btn-secondary"
            ),
            actionButton(
              ns("confirm_upload"),
              "Add to Dataset",
              class = "btn-primary",
              icon = icon("check")
            )
          )
        )
      )
    })
    
    # Handle file upload
    upload_result <- reactive({
      req(input$file_upload)
      
      tryCatch({
        result <- process_upload(
          input$file_upload$datapath,
          input$file_upload$name
        )
        result
      }, error = function(e) {
        list(
          success = FALSE,
          validation = list(
            valid = FALSE,
            errors = c(as.character(e)),
            warnings = character()
          )
        )
      })
    })
    
    # Display upload status
    output$upload_status <- renderUI({
      req(upload_result())
      
      result <- upload_result()
      
      if (result$success) {
        # Format description
        format_desc <- switch(
          result$format_info$format_type,
          "single_sample" = "Single sample",
          "multi_sample_long" = "Multiple samples (long format)",
          "multi_sample_wide" = "Multiple samples (wide format)",
          "Unknown format"
        )
        
        # Calculate data point range
        if (length(result$format_info$data_points) > 1) {
          data_point_range <- sprintf(
            "%d - %d",
            min(result$format_info$data_points),
            max(result$format_info$data_points)
          )
        } else {
          data_point_range <- as.character(result$format_info$data_points[1])
        }
        
        # Prepare sample ID display (limit to 10)
        sample_ids_display <- NULL
        if (!is.null(result$format_info$sample_ids)) {
          n_ids <- length(result$format_info$sample_ids)
          if (n_ids <= 10) {
            sample_ids_display <- paste(result$format_info$sample_ids, collapse = ", ")
          } else {
            sample_ids_display <- paste(
              paste(result$format_info$sample_ids[1:10], collapse = ", "),
              "..."
            )
          }
        }
        
        div(
          class = "alert alert-success",
          icon("check-circle"), " File uploaded successfully!",
          tags$ul(
            tags$li(
              sprintf("Format: %s", format_desc)
            ),
            tags$li(
              sprintf("Samples: %d", result$format_info$n_samples)
            ),
            tags$li(
              sprintf("Data points per sample: %s", data_point_range)
            ),
            if (!is.null(sample_ids_display)) {
              tags$li(
                "Sample IDs: ", sample_ids_display
              )
            },
            if (!is.null(result$format_info$empty_sample_ids) && 
                length(result$format_info$empty_sample_ids) > 0) {
              tags$li(
                class = "text-warning",
                sprintf(
                  "%d empty sample(s) skipped: %s",
                  result$format_info$n_samples_empty,
                  paste(head(result$format_info$empty_sample_ids, 5), collapse = ", "),
                  if (length(result$format_info$empty_sample_ids) > 5) "..." else ""
                )
              )
            }
          ),
          if (length(result$validation$warnings) > 0) {
            div(
              class = "mt-2",
              tags$strong("Warnings:"),
              tags$ul(
                lapply(result$validation$warnings, function(w) {
                  tags$li(class = "text-warning", w)
                })
              )
            )
          }
        )
      } else {
        div(
          class = "alert alert-danger",
          icon("exclamation-circle"), " Upload failed",
          tags$ul(
            lapply(result$validation$errors, function(e) {
              tags$li(e)
            })
          )
        )
      }
    })
    
    # Display data preview
    output$data_preview <- renderUI({
      req(upload_result())
      
      result <- upload_result()
      
      if (result$success && !is.null(result$data)) {
        div(
          class = "mt-3",
          h5("Data Preview (first 10 rows)"),
          div(
            style = "max-height: 300px; overflow-y: auto;",
            DT::dataTableOutput(ns("preview_table"))
          )
        )
      }
    })
    
    # Render the preview table separately
    output$preview_table <- DT::renderDataTable({
      req(upload_result())
      result <- upload_result()
      
      if (result$success && !is.null(result$data)) {
        DT::datatable(
          head(result$data, 10),
          options = list(
            dom = 't',
            pageLength = 10,
            scrollX = TRUE
          ),
          rownames = FALSE,
          class = "compact"
        )
      }
    })
    
    # Confirm upload
    observeEvent(input$confirm_upload, {
      req(upload_result())
      
      result <- upload_result()
      
      if (result$success) {
        # Add to uploaded files list
        current_files <- uploaded_files()
        current_files[[length(current_files) + 1]] <- result
        uploaded_files(current_files)
        
        # Store in app_data for other modules
        app_data$raw_data <- result$data
        
        # Show success message
        showNotification(
          paste("Successfully added", result$file_name),
          type = "message",
          duration = 3
        )
        
        # Close modal
        removeModal()
      } else {
        showNotification(
          "Cannot add invalid file",
          type = "error",
          duration = 3
        )
      }
    })
    
    # Cancel upload
    observeEvent(input$cancel_upload, {
      removeModal()
    })
    
    # Render uploaded files list
    output$uploaded_files_list <- renderUI({
      files <- uploaded_files()
      
      if (length(files) == 0) {
        return(
          p(
            class = "text-muted text-center",
            "No files uploaded yet. Click the button above to get started."
          )
        )
      }
      
      # Create list of uploaded files
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
                  icon("file"), " ", 
                  tags$strong(file_info$file_name),
                  tags$small(
                    class = "text-muted ms-2",
                    sprintf(
                      "(%d samples, %d rows)", 
                      file_info$format_info$n_samples,
                      file_info$format_info$n_rows
                    )
                  )
                ),
                tags$small(
                  class = "text-muted",
                  format(file_info$upload_time, "%Y-%m-%d %H:%M")
                )
              )
            )
          )
        })
      )
    })
  })
}