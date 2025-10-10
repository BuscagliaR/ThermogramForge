# Review Endpoints Module
# Interactive thermogram review and manual endpoint adjustment

#' Review Endpoints UI
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI elements
mod_review_endpoints_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container-fluid p-3",
      
      # Check if data is available
      uiOutput(ns("review_content"))
    )
  )
}

#' Review Endpoints Server
#'
#' @param id Module namespace ID
#' @param app_data Reactive values object containing application data
#'
#' @return Server logic (no return value)
mod_review_endpoints_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Track currently selected sample
    selected_sample <- reactiveVal(NULL)
    
    # Flag to prevent observer triggering during programmatic selection
    programmatic_selection <- reactiveVal(FALSE)
    
    # Render main content
    output$review_content <- renderUI({
      
      # Check if processed data exists
      if (is.null(app_data$processed_data)) {
        return(
          div(
            class = "card",
            div(
              class = "card-body text-center",
              icon("info-circle", class = "fa-3x text-muted mb-3"),
              h4("No Processed Data Available"),
              p(
                "Please upload and process thermogram data in the ",
                tags$strong("Data Overview"),
                " tab before reviewing endpoints."
              ),
              actionButton(
                ns("goto_overview"),
                "Go to Data Overview",
                icon = icon("arrow-left"),
                class = "btn-primary"
              )
            )
          )
        )
      }
      
      # Data is available - show review interface
      tagList(
        # Header row
        div(
          class = "row mb-3",
          div(
            class = "col-12",
            div(
              class = "card",
              div(
                class = "card-body",
                div(
                  class = "d-flex justify-content-between align-items-center",
                  div(
                    h4(
                      icon("chart-line"), 
                      " Review Baseline Endpoints",
                      class = "mb-0"
                    ),
                    p(
                      class = "text-muted mb-0",
                      sprintf(
                        "Dataset: %d samples processed",
                        app_data$processed_data$summary$n_success
                      )
                    )
                  ),
                  div(
                    actionButton(
                      ns("save_btn"),
                      "Save Processed Data",
                      icon = icon("save"),
                      class = "btn-success"
                    )
                  )
                )
              )
            )
          )
        ),
        
        # Main content row
        div(
          class = "row",
          
          # Left column: Sample grid
          div(
            class = "col-md-5",
            div(
              class = "card",
              div(
                class = "card-header",
                icon("table"), " Sample Overview"
              ),
              div(
                class = "card-body",
                DT::dataTableOutput(ns("sample_grid"))
              )
            )
          ),
          
          # Right column: Thermogram plot and controls
          div(
            class = "col-md-7",
            
            # Plot card
            div(
              class = "card mb-3",
              div(
                class = "card-header",
                uiOutput(ns("plot_header"))
              ),
              div(
                class = "card-body",
                uiOutput(ns("plot_area"))
              )
            ),
            
            # Controls card
            div(
              class = "card",
              div(
                class = "card-header",
                icon("sliders-h"), " Review Controls"
              ),
              div(
                class = "card-body",
                uiOutput(ns("review_controls"))
              )
            )
          )
        )
      )
    })
    
    # Navigate to Data Overview
    observeEvent(input$goto_overview, {
      app_data$navigate_to <- "data_overview"
    })
    
    # Create sample grid data
    sample_grid_data <- reactive({
      req(app_data$processed_data)
      
      samples <- app_data$processed_data$samples
      
      # Create data frame for grid with human-readable column names
      grid_df <- data.frame(
        Sample_ID = character(),
        Signal_Quality = character(),
        Reviewed = character(),
        Excluded = character(),
        stringsAsFactors = FALSE
      )
      
      for (sample_id in names(samples)) {
        sample <- samples[[sample_id]]
        
        if (sample$success) {
          grid_df <- rbind(grid_df, data.frame(
            Sample_ID = sample_id,
            Signal_Quality = if (sample$has_signal) "Signal" else "No Signal",
            Reviewed = if (sample$reviewed) "Yes" else "No",
            Excluded = if (sample$excluded) "Yes" else "No",
            stringsAsFactors = FALSE
          ))
        }
      }
      
      # Set column names for display
      colnames(grid_df) <- c("Sample ID", "Signal Quality", "Reviewed", "Excluded")
      
      grid_df
    })
    
    # Render sample grid
    output$sample_grid <- DT::renderDataTable({
      
      grid_data <- sample_grid_data()
      
      DT::datatable(
        grid_data,
        selection = list(mode = "single", selected = 1),
        rownames = FALSE,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 25, 50),
          dom = 'ftp',
          scrollY = "400px",
          scrollCollapse = TRUE,
          columnDefs = list(
            list(
              targets = 0,
              className = "dt-left"
            ),
            list(
              targets = 1:3,
              className = "dt-center"
            )
          )
        ),
        class = "compact hover row-border"
      ) %>%
        DT::formatStyle(
          'Signal Quality',
          backgroundColor = DT::styleEqual(
            c('Signal', 'No Signal'),
            c('#d4edda', '#fff3cd')
          ),
          color = DT::styleEqual(
            c('Signal', 'No Signal'),
            c('#155724', '#856404')
          ),
          fontWeight = 'bold'
        ) %>%
        DT::formatStyle(
          'Reviewed',
          color = DT::styleEqual(
            c('Yes', 'No'),
            c('#198754', '#6c757d')
          ),
          fontWeight = 'bold'
        ) %>%
        DT::formatStyle(
          'Excluded',
          color = DT::styleEqual(
            c('Yes', 'No'),
            c('#dc3545', '#6c757d')
          ),
          fontWeight = 'bold'
        )
    })
    
    # Update selected sample when row is clicked
    observeEvent(input$sample_grid_rows_selected, {
      req(input$sample_grid_rows_selected)
      
      # Skip if this is a programmatic selection
      if (isolate(programmatic_selection())) {
        return()
      }
      
      grid_data <- sample_grid_data()
      selected_row <- input$sample_grid_rows_selected
      
      if (selected_row <= nrow(grid_data)) {
        sample_id <- grid_data[[1]][selected_row]  # First column is Sample ID
        
        # Only update if the sample actually changed (prevent reactivity loop)
        if (is.null(selected_sample()) || selected_sample() != sample_id) {
          selected_sample(sample_id)
        }
      }
    }, ignoreNULL = TRUE, ignoreInit = FALSE, priority = 1)
    
    # Initialize first sample selection
    observe({
      req(app_data$processed_data)
      
      if (is.null(selected_sample())) {
        grid_data <- sample_grid_data()
        if (nrow(grid_data) > 0) {
          selected_sample(grid_data$sample_id[1])
        }
      }
    })
    
    # Render plot header
    output$plot_header <- renderUI({
      req(selected_sample())
      
      sample <- app_data$processed_data$samples[[selected_sample()]]
      
      tagList(
        icon("chart-line"), 
        sprintf(" Thermogram: %s", selected_sample()),
        if (!sample$has_signal) {
          tags$span(
            class = "badge bg-warning ms-2",
            "No Signal Detected"
          )
        }
      )
    })
    
    # Render plot area
    output$plot_area <- renderUI({
      req(selected_sample())
      
      sample <- app_data$processed_data$samples[[selected_sample()]]
      
      if (!sample$success) {
        return(
          div(
            class = "alert alert-danger",
            icon("exclamation-triangle"),
            " Error processing this sample: ",
            sample$error
          )
        )
      }
      
      # Render the plotly output
      plotly::plotlyOutput(ns("thermogram_plot"), height = "400px")
    })
    
    # Render thermogram plot
    output$thermogram_plot <- plotly::renderPlotly({
      req(selected_sample())
      
      sample <- app_data$processed_data$samples[[selected_sample()]]
      
      if (!sample$success) return(NULL)
      
      # Prepare data for plotting - ensure vectors are clean
      temp <- as.numeric(sample$temperature)
      dcp_orig <- as.numeric(sample$dcp_original)
      
      # Remove any remaining NAs
      valid_idx <- !is.na(temp) & !is.na(dcp_orig)
      temp <- temp[valid_idx]
      dcp_orig <- dcp_orig[valid_idx]
      
      if (length(temp) < 2) {
        return(plotly::plot_ly() %>%
                 plotly::layout(
                   title = "Insufficient data to plot",
                   xaxis = list(title = "Temperature (°C)"),
                   yaxis = list(title = "dCp")
                 ))
      }
      
      # Get y-axis range for vertical lines
      y_min <- min(dcp_orig, na.rm = TRUE)
      y_max <- max(dcp_orig, na.rm = TRUE)
      y_range <- y_max - y_min
      y_padding <- y_range * 0.1
      
      # Create the base plot
      p <- plotly::plot_ly()
      
      # Add baseline endpoints as vertical lines
      lower_endpoint <- as.numeric(sample$lower_endpoint)
      upper_endpoint <- as.numeric(sample$upper_endpoint)
      
      # Add transition region shading first (so it's behind)
      p <- p %>%
        plotly::add_polygons(
          x = c(lower_endpoint, upper_endpoint, upper_endpoint, lower_endpoint),
          y = c(y_min - y_padding, y_min - y_padding, y_max + y_padding, y_max + y_padding),
          fillcolor = "rgba(200, 200, 200, 0.2)",
          line = list(width = 0),
          name = "Transition Region",
          showlegend = TRUE,
          hoverinfo = "skip"
        )
      
      # Add lower endpoint line
      p <- p %>%
        plotly::add_segments(
          x = lower_endpoint, 
          xend = lower_endpoint,
          y = y_min - y_padding,
          yend = y_max + y_padding,
          line = list(color = "#2ca02c", width = 2, dash = "dash"),
          name = "Lower Endpoint",
          showlegend = TRUE,
          hovertemplate = paste0(
            "<b>Lower Endpoint</b><br>",
            "Temperature: ", sprintf("%.1f", lower_endpoint), "°C<br>",
            "<extra></extra>"
          )
        )
      
      # Add upper endpoint line
      p <- p %>%
        plotly::add_segments(
          x = upper_endpoint,
          xend = upper_endpoint,
          y = y_min - y_padding,
          yend = y_max + y_padding,
          line = list(color = "#9467bd", width = 2, dash = "dash"),
          name = "Upper Endpoint",
          showlegend = TRUE,
          hovertemplate = paste0(
            "<b>Upper Endpoint</b><br>",
            "Temperature: ", sprintf("%.1f", upper_endpoint), "°C<br>",
            "<extra></extra>"
          )
        )
      
      # Add raw data line on top
      p <- p %>%
        plotly::add_lines(
          x = temp,
          y = dcp_orig,
          name = "Raw Data",
          line = list(color = "#1f77b4", width = 2),
          hovertemplate = paste(
            "<b>Temperature:</b> %{x:.1f}°C<br>",
            "<b>dCp:</b> %{y:.4f}<br>",
            "<extra></extra>"
          )
        )
      
      # Configure layout
      p <- p %>%
        plotly::layout(
          xaxis = list(
            title = "Temperature (°C)",
            showgrid = TRUE,
            gridcolor = "#e0e0e0",
            zeroline = FALSE
          ),
          yaxis = list(
            title = "dCp (kcal/mol/°C)",
            showgrid = TRUE,
            gridcolor = "#e0e0e0",
            zeroline = TRUE,
            zerolinecolor = "#808080",
            zerolinewidth = 1,
            range = c(y_min - y_padding, y_max + y_padding)
          ),
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          hovermode = "closest",
          showlegend = TRUE,
          legend = list(
            x = 0.02,
            y = 0.98,
            bgcolor = "rgba(255, 255, 255, 0.8)",
            bordercolor = "#dee2e6",
            borderwidth = 1
          ),
          margin = list(l = 60, r = 30, t = 30, b = 50)
        ) %>%
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = list(
            "pan2d", "lasso2d", "select2d", "autoScale2d",
            "hoverClosestCartesian", "hoverCompareCartesian",
            "toggleSpikelines"
          ),
          displaylogo = FALSE
        )
      
      p
    })
    
    # Render review controls
    output$review_controls <- renderUI({
      req(selected_sample())
      
      sample <- app_data$processed_data$samples[[selected_sample()]]
      
      if (!sample$success) {
        return(
          p(class = "text-muted", "No controls available for failed samples.")
        )
      }
      
      tagList(
        div(
          class = "row mb-3",
          div(
            class = "col-md-6",
            checkboxInput(
              ns("mark_reviewed"),
              "Mark as Reviewed",
              value = sample$reviewed
            )
          ),
          div(
            class = "col-md-6",
            checkboxInput(
              ns("mark_excluded"),
              "Exclude from Analysis",
              value = sample$excluded
            )
          )
        ),
        
        hr(),
        
        div(
          class = "d-flex justify-content-between",
          div(
            actionButton(
              ns("prev_sample"),
              "Previous",
              icon = icon("arrow-left"),
              class = "btn-secondary"
            ),
            actionButton(
              ns("next_sample"),
              "Next",
              icon = icon("arrow-right"),
              class = "btn-secondary ms-2"
            )
          ),
          div(
            actionButton(
              ns("undo_btn"),
              "Undo",
              icon = icon("undo"),
              class = "btn-outline-secondary",
              disabled = "disabled"
            ),
            actionButton(
              ns("redo_btn"),
              "Redo",
              icon = icon("redo"),
              class = "btn-outline-secondary ms-2",
              disabled = "disabled"
            )
          )
        ),
        
        hr(),
        
        p(
          class = "text-muted small mb-0",
          icon("info-circle"),
          " Endpoint adjustment coming in Stage 3"
        )
      )
    })
    
    # Handle reviewed checkbox
    observeEvent(input$mark_reviewed, {
      req(selected_sample())
      
      # Update the data
      app_data$processed_data$samples[[selected_sample()]]$reviewed <- input$mark_reviewed
      
      # Get current selection index before replacing data
      grid_data <- isolate(sample_grid_data())
      current_idx <- which(grid_data[[1]] == selected_sample())  # First column is Sample ID
      
      # Refresh grid data
      proxy <- DT::dataTableProxy("sample_grid")
      DT::replaceData(
        proxy = proxy,
        data = grid_data,
        resetPaging = FALSE,
        rownames = FALSE
      )
      
      # Restore selection after a brief delay, using flag to prevent observer trigger
      shinyjs::delay(50, {
        programmatic_selection(TRUE)
        DT::selectRows(proxy, current_idx)
        shinyjs::delay(10, {
          programmatic_selection(FALSE)
        })
      })
    }, priority = 10)
    
    # Handle excluded checkbox
    observeEvent(input$mark_excluded, {
      req(selected_sample())
      
      # Update the data
      app_data$processed_data$samples[[selected_sample()]]$excluded <- input$mark_excluded
      
      # Get current selection index before replacing data
      grid_data <- isolate(sample_grid_data())
      current_idx <- which(grid_data[[1]] == selected_sample())  # First column is Sample ID
      
      # Refresh grid data
      proxy <- DT::dataTableProxy("sample_grid")
      DT::replaceData(
        proxy = proxy,
        data = grid_data,
        resetPaging = FALSE,
        rownames = FALSE
      )
      
      # Restore selection after a brief delay, using flag to prevent observer trigger
      shinyjs::delay(50, {
        programmatic_selection(TRUE)
        DT::selectRows(proxy, current_idx)
        shinyjs::delay(10, {
          programmatic_selection(FALSE)
        })
      })
    }, priority = 10)
    
    # Navigate to previous sample
    observeEvent(input$prev_sample, {
      req(selected_sample())
      
      grid_data <- sample_grid_data()
      current_idx <- which(grid_data[[1]] == selected_sample())  # First column is Sample ID
      
      if (current_idx > 1) {
        new_idx <- current_idx - 1
        new_sample <- grid_data[[1]][new_idx]
        selected_sample(new_sample)
        
        # Update grid selection with flag to prevent observer trigger
        proxy <- DT::dataTableProxy("sample_grid")
        programmatic_selection(TRUE)
        DT::selectRows(proxy, new_idx)
        shinyjs::delay(10, {
          programmatic_selection(FALSE)
        })
      }
    })
    
    # Navigate to next sample
    observeEvent(input$next_sample, {
      req(selected_sample())
      
      grid_data <- sample_grid_data()
      current_idx <- which(grid_data[[1]] == selected_sample())  # First column is Sample ID
      
      if (current_idx < nrow(grid_data)) {
        new_idx <- current_idx + 1
        new_sample <- grid_data[[1]][new_idx]
        selected_sample(new_sample)
        
        # Update grid selection with flag to prevent observer trigger
        proxy <- DT::dataTableProxy("sample_grid")
        programmatic_selection(TRUE)
        DT::selectRows(proxy, new_idx)
        shinyjs::delay(10, {
          programmatic_selection(FALSE)
        })
      }
    })
    
    # Save processed data (placeholder)
    observeEvent(input$save_btn, {
      showModal(
        modalDialog(
          title = "Save Processed Data",
          "Save functionality will be implemented in Phase 7 (Data Management).",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
  })
}