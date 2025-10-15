# Review Endpoints Module - Final Fixed Version
# Interactive thermogram review and manual endpoint adjustment with full history tracking

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
    
    # Track plot view mode (raw or baseline_subtracted) - DEFAULT TO BASELINE SUBTRACTED
    plot_view <- reactiveVal("baseline_subtracted")
    
    # Track endpoint adjustment mode
    adjustment_mode <- reactiveVal(NULL)  # NULL, "lower", or "upper"
    
    # Track last processed click to prevent duplicate processing
    last_click_key <- reactiveVal(NULL)
    
    # =============================================================================
    # DIAGNOSTIC OBSERVERS (Remove after testing)
    # =============================================================================
    
    # Monitor all clicks regardless of mode
    observe({
      click <- plotly::event_data("plotly_click", source = "thermogram_plot")
      
      if (!is.null(click)) {
        cat("\n╔═══════════════════════════════════════╗\n")
        cat("║  PLOTLY CLICK EVENT CAPTURED!        ║\n")
        cat("╚═══════════════════════════════════════╝\n")
        cat("  X (Temperature):", click$x, "\n")
        cat("  Y (Value):", click$y, "\n")
        cat("  Curve Number:", ifelse(is.null(click$curveNumber), "NULL", click$curveNumber), "\n")
        cat("  Current adjustment mode:", ifelse(is.null(adjustment_mode()), "NONE", adjustment_mode()), "\n")
        cat("  Selected sample:", ifelse(is.null(selected_sample()), "NONE", selected_sample()), "\n")
        cat("─────────────────────────────────────────\n\n")
      }
    })
    
    # Monitor adjustment mode changes
    observe({
      mode <- adjustment_mode()
      cat("\n[MODE] Adjustment mode changed to:", ifelse(is.null(mode), "OFF", mode), "\n\n")
    })
    
    # =============================================================================
    # END DIAGNOSTIC OBSERVERS
    # =============================================================================
    
    # Helper function to push state to undo stack
    push_undo_state <- function(sample_id, action_type, previous_state) {
      state_entry <- list(
        sample_id = sample_id,
        action_type = action_type,
        previous_state = previous_state,
        timestamp = Sys.time()
      )
      
      # Add to undo stack
      app_data$undo_stack <- c(app_data$undo_stack, list(state_entry))
      
      # Clear redo stack (new action invalidates redo history)
      app_data$redo_stack <- list()
    }
    
    # Helper function to capture current sample state
    capture_sample_state <- function(sample_id) {
      sample <- app_data$processed_data$samples[[sample_id]]
      
      list(
        lower_endpoint = sample$lower_endpoint,
        upper_endpoint = sample$upper_endpoint,
        baseline_subtracted = sample$baseline_subtracted,
        manual_adjustment = sample$manual_adjustment,
        lower_manual = if(is.null(sample$lower_manual)) FALSE else sample$lower_manual,
        upper_manual = if(is.null(sample$upper_manual)) FALSE else sample$upper_manual,
        reviewed = sample$reviewed,
        excluded = sample$excluded
      )
    }
    
    # Helper function to restore sample state
    restore_sample_state <- function(sample_id, state) {
      app_data$processed_data$samples[[sample_id]]$lower_endpoint <- state$lower_endpoint
      app_data$processed_data$samples[[sample_id]]$upper_endpoint <- state$upper_endpoint
      app_data$processed_data$samples[[sample_id]]$baseline_subtracted <- state$baseline_subtracted
      app_data$processed_data$samples[[sample_id]]$manual_adjustment <- state$manual_adjustment
      app_data$processed_data$samples[[sample_id]]$lower_manual <- state$lower_manual
      app_data$processed_data$samples[[sample_id]]$upper_manual <- state$upper_manual
      app_data$processed_data$samples[[sample_id]]$reviewed <- state$reviewed
      app_data$processed_data$samples[[sample_id]]$excluded <- state$excluded
    }
    
    # Check if undo stack has entries
    can_undo <- reactive({
      length(app_data$undo_stack) > 0
    })
    
    # Check if redo stack has entries
    can_redo <- reactive({
      length(app_data$redo_stack) > 0
    })
    
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
                class = "card-header d-flex justify-content-between align-items-center",
                div(
                  uiOutput(ns("plot_header"))
                ),
                div(
                  uiOutput(ns("view_toggle_buttons"))
                )
              ),
              div(
                class = "card-body",
                # Adjustment mode notification
                uiOutput(ns("adjustment_notification")),
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
    
    # Handle view toggle
    observeEvent(input$view_raw, {
      plot_view("raw")
    })
    
    observeEvent(input$view_baseline, {
      plot_view("baseline_subtracted")
    })
    
    # Create sample grid data
    sample_grid_data <- reactive({
      req(app_data$processed_data)
      
      samples <- app_data$processed_data$samples
      
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
      
      colnames(grid_df) <- c("Sample ID", "Signal Quality", "Reviewed", "Excluded")
      grid_df
    })
    
    # Render sample grid - NON-REACTIVE initial render
    output$sample_grid <- DT::renderDataTable({
      
      # Use isolate to prevent reactive updates
      grid_data <- isolate(sample_grid_data())
      
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
            list(targets = 0, className = "dt-left"),
            list(targets = 1:3, className = "dt-center")
          )
        ),
        class = "compact hover row-border"
      ) %>%
        DT::formatStyle(
          'Signal Quality',
          backgroundColor = DT::styleEqual(c('Signal', 'No Signal'), c('#d4edda', '#fff3cd')),
          color = DT::styleEqual(c('Signal', 'No Signal'), c('#155724', '#856404')),
          fontWeight = 'bold'
        ) %>%
        DT::formatStyle(
          'Reviewed',
          color = DT::styleEqual(c('Yes', 'No'), c('#198754', '#6c757d')),
          fontWeight = 'bold'
        ) %>%
        DT::formatStyle(
          'Excluded',
          color = DT::styleEqual(c('Yes', 'No'), c('#dc3545', '#6c757d')),
          fontWeight = 'bold'
        )
    })
    
    # Update selected sample when row is clicked
    observeEvent(input$sample_grid_rows_selected, {
      req(input$sample_grid_rows_selected)
      
      if (isolate(programmatic_selection())) return()
      
      grid_data <- sample_grid_data()
      selected_row <- input$sample_grid_rows_selected
      
      if (selected_row <= nrow(grid_data)) {
        sample_id <- grid_data[[1]][selected_row]
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
          selected_sample(grid_data[[1]][1])
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
          tags$span(class = "badge bg-warning ms-2", "No Signal Detected")
        }
      )
    })
    
    # Render view toggle buttons
    output$view_toggle_buttons <- renderUI({
      current_view <- plot_view()
      
      div(
        class = "btn-group btn-group-sm",
        role = "group",
        actionButton(
          ns("view_raw"),
          "Raw Thermogram",
          class = if (current_view == "raw") "btn-primary" else "btn-outline-primary"
        ),
        actionButton(
          ns("view_baseline"),
          "Baseline Subtracted",
          class = if (current_view == "baseline_subtracted") "btn-primary" else "btn-outline-primary"
        )
      )
    })
    
    # Render adjustment mode notification
    output$adjustment_notification <- renderUI({
      mode <- adjustment_mode()
      
      if (!is.null(mode)) {
        div(
          class = "alert alert-info mb-3",
          icon("hand-pointer"), " ",
          tags$strong(
            sprintf(
              "Click on the plot to set the %s endpoint",
              if (mode == "lower") "LOWER" else "UPPER"
            )
          ),
          tags$button(
            type = "button",
            class = "btn-close float-end",
            `data-bs-dismiss` = "alert",
            onclick = sprintf("Shiny.setInputValue('%s', 'cancel', {priority: 'event'});", ns("cancel_adjustment"))
          )
        )
      }
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
      
      plotly::plotlyOutput(ns("thermogram_plot"), height = "400px")
    })
    
    # Reactive to get plot click data
    plot_click <- reactive({
      click_data <- plotly::event_data("plotly_click", source = "thermogram_plot")
      click_data
    })
    
    # Render thermogram plot
    output$thermogram_plot <- plotly::renderPlotly({
      req(selected_sample())
      plot_view()
      
      sample <- app_data$processed_data$samples[[selected_sample()]]
      
      if (!sample$success) return(NULL)
      
      # Determine which data to plot
      current_view <- plot_view()
      
      if (current_view == "baseline_subtracted" && 
          !is.null(sample$baseline_subtracted) && 
          length(sample$baseline_subtracted) > 0) {
        # Plot baseline-subtracted data (uses interpolated grid)
        temp <- as.numeric(sample$temperature)
        dcp_data <- as.numeric(sample$baseline_subtracted)
        min_len <- min(length(temp), length(dcp_data))
        temp <- temp[1:min_len]
        dcp_data <- dcp_data[1:min_len]
        plot_title_suffix <- " (Baseline Subtracted)"
      } else {
        # Plot raw data (uses original temperature values)
        # Fallback to interpolated temperature if original not available (backwards compatibility)
        if (!is.null(sample$temperature_original)) {
          temp <- as.numeric(sample$temperature_original)
        } else {
          temp <- as.numeric(sample$temperature)
          cat("[WARNING] Using interpolated temperature for raw plot (re-process data recommended)\n")
        }
        dcp_data <- as.numeric(sample$dcp_original)
        min_len <- min(length(temp), length(dcp_data))
        temp <- temp[1:min_len]
        dcp_data <- dcp_data[1:min_len]
        plot_title_suffix <- " (Raw)"
      }
      
      valid_idx <- !is.na(temp) & !is.na(dcp_data)
      temp <- temp[valid_idx]
      dcp_data <- dcp_data[valid_idx]
      
      if (length(temp) < 2) {
        return(plotly::plot_ly() %>%
                 plotly::layout(
                   title = "Insufficient data to plot",
                   xaxis = list(title = "Temperature (°C)"),
                   yaxis = list(title = "dCp")
                 ))
      }
      
      y_min <- min(dcp_data, na.rm = TRUE)
      y_max <- max(dcp_data, na.rm = TRUE)
      y_range <- y_max - y_min
      y_padding <- y_range * 0.1
      
      p <- plotly::plot_ly()
      
      lower_endpoint <- as.numeric(sample$lower_endpoint)
      upper_endpoint <- as.numeric(sample$upper_endpoint)
      
      p <- p %>%
        plotly::add_polygons(
          x = c(lower_endpoint, upper_endpoint, upper_endpoint, lower_endpoint),
          y = c(y_min - y_padding, y_min - y_padding, y_max + y_padding, y_max + y_padding),
          fillcolor = "rgba(200, 200, 200, 0.2)",
          line = list(width = 0),
          name = "Transition Region",
          showlegend = TRUE,
          hoverinfo = "skip"
        ) %>%
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
        ) %>%
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
        ) %>%
        plotly::add_lines(
          x = temp,
          y = dcp_data,
          name = paste0("Thermogram", plot_title_suffix),
          line = list(color = "#1f77b4", width = 2),
          hovertemplate = paste(
            "<b>Temperature:</b> %{x:.1f}°C<br>",
            "<b>dCp:</b> %{y:.4f}<br>",
            "<extra></extra>"
          )
        ) %>%
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
            x = 0.98,
            y = 0.98,
            xanchor = "right",
            bgcolor = "rgba(255, 255, 255, 0.8)",
            bordercolor = "#dee2e6",
            borderwidth = 1
          ),
          margin = list(l = 60, r = 30, t = 30, b = 50)
        ) %>%
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = list(
            "lasso2d", "select2d", "autoScale2d",
            "hoverClosestCartesian", "hoverCompareCartesian",
            "toggleSpikelines"
          ),
          displaylogo = FALSE
        )
      
      # Set source for click event tracking
      p$x$source <- "thermogram_plot"
      
      # Verify source is set
      cat("\n[PLOT] Rendering plot for sample:", selected_sample(), "\n")
      cat("[PLOT] Source set to:", p$x$source, "\n")
      
      p
    })
    
    # Handle plot clicks for manual endpoint adjustment
    observeEvent(plot_click(), {
      # Only process when in adjustment mode
      req(adjustment_mode())
      req(selected_sample())
      
      click_data <- plot_click()
      req(click_data)
      req(!is.null(click_data$x))
      
      # Debug output
      cat("\n=== PLOT CLICK DETECTED ===\n")
      cat("  Temperature:", click_data$x, "\n")
      cat("  Mode:", adjustment_mode(), "\n")
      cat("  Sample:", selected_sample(), "\n")
      
      # Create a unique key for this click event
      click_key <- paste(click_data$x, click_data$y, 
                         ifelse(is.null(click_data$curveNumber), 0, click_data$curveNumber), 
                         sep = "_")
      
      # Skip if we've already processed this exact click
      if (!is.null(isolate(last_click_key())) && isolate(last_click_key()) == click_key) {
        cat("  Skipping duplicate click\n")
        return()
      }
      
      # Update last processed click
      last_click_key(click_key)
      
      clicked_temp <- click_data$x
      mode <- isolate(adjustment_mode())
      sample_id <- isolate(selected_sample())
      sample <- isolate(app_data$processed_data$samples[[sample_id]])
      
      cat("  Processing click...\n")
      
      # Check if we have original temperature data (for backwards compatibility)
      if (is.null(sample$temperature_original)) {
        showNotification(
          "Cannot adjust endpoints: Original temperature data not available. Please re-process the data.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      # Capture state before modification (for undo)
      previous_state <- capture_sample_state(sample_id)
      
      # Validate click based on mode
      if (mode == "lower") {
        if (clicked_temp >= sample$upper_endpoint) {
          showNotification(
            sprintf(
              "Lower endpoint (%.1f°C) must be less than upper endpoint (%.1f°C)",
              clicked_temp, sample$upper_endpoint
            ),
            type = "error",
            duration = 3
          )
          return()
        }
        new_lower <- clicked_temp
        new_upper <- sample$upper_endpoint
      } else if (mode == "upper") {
        if (clicked_temp <= sample$lower_endpoint) {
          showNotification(
            sprintf(
              "Upper endpoint (%.1f°C) must be greater than lower endpoint (%.1f°C)",
              clicked_temp, sample$lower_endpoint
            ),
            type = "error",
            duration = 3
          )
          return()
        }
        new_lower <- sample$lower_endpoint
        new_upper <- clicked_temp
      } else {
        return()
      }
      
      # Re-process with new endpoints
      result <- reprocess_with_manual_endpoints(
        temperature = sample$temperature_original,
        dcp = sample$dcp_original,
        lower_endpoint = new_lower,
        upper_endpoint = new_upper
      )
      
      if (!result$success) {
        showNotification(
          paste("Error re-processing:", result$error),
          type = "error",
          duration = 5
        )
        return()
      }
      
      cat("  Successfully reprocessed with new endpoint\n")
      
      # Push to undo stack BEFORE making changes
      push_undo_state(
        sample_id = sample_id,
        action_type = "endpoint_adjustment",
        previous_state = previous_state
      )
      
      # CRITICAL: Save current sample to prevent grid reactivity from changing selection
      current_sample <- isolate(selected_sample())
      
      # Set flag to block grid observer
      programmatic_selection(TRUE)
      
      # Update the sample with new results
      isolate({
        app_data$processed_data$samples[[sample_id]]$lower_endpoint <- result$lower_endpoint
        app_data$processed_data$samples[[sample_id]]$upper_endpoint <- result$upper_endpoint
        app_data$processed_data$samples[[sample_id]]$baseline_subtracted <- result$baseline_subtracted
        
        # Set manual flags - only mark the endpoint that was actually adjusted
        if (mode == "lower") {
          app_data$processed_data$samples[[sample_id]]$lower_manual <- TRUE
        } else if (mode == "upper") {
          app_data$processed_data$samples[[sample_id]]$upper_manual <- TRUE
        }
        
        # Set global manual flag if either endpoint is manual
        app_data$processed_data$samples[[sample_id]]$manual_adjustment <- 
          app_data$processed_data$samples[[sample_id]]$lower_manual || 
          app_data$processed_data$samples[[sample_id]]$upper_manual
      })
      
      # Force selection to stay on current sample
      selected_sample(current_sample)
      
      # Restore grid selection explicitly
      grid_data <- isolate(sample_grid_data())
      current_idx <- which(grid_data[[1]] == current_sample)
      if (length(current_idx) > 0) {
        proxy <- DT::dataTableProxy("sample_grid")
        DT::selectRows(proxy, current_idx[1])
      }
      
      # Small delay before allowing grid to react again
      shinyjs::delay(50, {
        programmatic_selection(FALSE)
      })
      
      # Exit adjustment mode and remove ALL notifications
      adjustment_mode(NULL)
      removeNotification(id = "adjustment_notification")
      
      showNotification(
        sprintf(
          "%s endpoint updated to %.1f°C",
          if (mode == "lower") "Lower" else "Upper",
          if (mode == "lower") new_lower else new_upper
        ),
        type = "message",
        duration = 3
      )
      
      cat("  Endpoint adjustment complete!\n\n")
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Cancel adjustment mode
    observeEvent(input$cancel_adjustment, {
      adjustment_mode(NULL)
      last_click_key(NULL)
      removeNotification(id = "adjustment_notification")
    })
    
    # Render review controls
    output$review_controls <- renderUI({
      req(selected_sample())
      
      sample <- app_data$processed_data$samples[[selected_sample()]]
      
      if (!sample$success) {
        return(p(class = "text-muted", "No controls available for failed samples."))
      }
      
      # Check if sample has been manually adjusted
      is_manual <- !is.null(sample$manual_adjustment) && sample$manual_adjustment
      lower_manual <- if(is.null(sample$lower_manual)) FALSE else sample$lower_manual
      upper_manual <- if(is.null(sample$upper_manual)) FALSE else sample$upper_manual
      
      tagList(
        # Endpoint information
        div(
          class = "mb-3",
          h6(class = "text-muted mb-2", icon("crosshairs"), " Baseline Endpoints"),
          div(
            class = "card bg-light",
            div(
              class = "card-body p-2",
              div(
                class = "row mb-2",
                div(
                  class = "col-6",
                  tags$small(class = "text-muted", "Lower:"),
                  div(
                    style = "font-family: monospace; font-size: 1.1rem; color: #2ca02c;",
                    tags$span(
                      class = if (lower_manual) "badge bg-warning me-1" else "badge bg-success me-1",
                      style = "font-size: 0.6rem; vertical-align: middle;",
                      if (lower_manual) icon("hand-pointer") else icon("robot")
                    ),
                    sprintf("%.1f°C", sample$lower_endpoint)
                  )
                ),
                div(
                  class = "col-6",
                  tags$small(class = "text-muted", "Upper:"),
                  div(
                    style = "font-family: monospace; font-size: 1.1rem; color: #9467bd;",
                    tags$span(
                      class = if (upper_manual) "badge bg-warning me-1" else "badge bg-success me-1",
                      style = "font-size: 0.6rem; vertical-align: middle;",
                      if (upper_manual) icon("hand-pointer") else icon("robot")
                    ),
                    sprintf("%.1f°C", sample$upper_endpoint)
                  )
                )
              )
            )
          )
        ),
        
        # Manual adjustment buttons
        div(
          class = "row mb-3",
          div(
            class = "col-6",
            actionButton(
              ns("adjust_lower"),
              "Adjust Lower",
              icon = icon("edit"),
              class = "btn-outline-success btn-sm w-100"
            )
          ),
          div(
            class = "col-6",
            actionButton(
              ns("adjust_upper"),
              "Adjust Upper",
              icon = icon("edit"),
              class = "btn-outline-secondary btn-sm w-100"
            )
          )
        ),
        
        # Discard changes button
        if (is_manual) {
          div(
            class = "mb-3",
            actionButton(
              ns("discard_changes"),
              "Discard Manual Changes",
              icon = icon("undo"),
              class = "btn-outline-warning btn-sm w-100"
            )
          )
        },
        
        hr(),
        
        # Review status checkboxes
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
        
        # Navigation and undo/redo buttons
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
              class = if (can_undo()) "btn-outline-secondary" else "btn-outline-secondary disabled"
            ),
            actionButton(
              ns("redo_btn"),
              "Redo",
              icon = icon("redo"),
              class = if (can_redo()) "btn-outline-secondary ms-2" else "btn-outline-secondary ms-2 disabled"
            )
          )
        )
      )
    })
    
    # Handle "Adjust Lower" button
    observeEvent(input$adjust_lower, {
      cat("\n[BUTTON] Adjust Lower clicked\n")
      adjustment_mode("lower")
      cat("[BUTTON] Mode set to:", adjustment_mode(), "\n")
      last_click_key(NULL)
      showNotification(
        "Click on the plot to set the lower endpoint",
        id = "adjustment_notification",
        type = "message",
        duration = NULL
      )
    })
    
    # Handle "Adjust Upper" button
    observeEvent(input$adjust_upper, {
      cat("\n[BUTTON] Adjust Upper clicked\n")
      adjustment_mode("upper")
      cat("[BUTTON] Mode set to:", adjustment_mode(), "\n")
      last_click_key(NULL)
      showNotification(
        "Click on the plot to set the upper endpoint",
        id = "adjustment_notification",
        type = "message",
        duration = NULL
      )
    })
    
    # Handle "Discard Changes" button
    observeEvent(input$discard_changes, {
      req(selected_sample())
      
      showModal(
        modalDialog(
          title = tagList(icon("exclamation-triangle"), " Discard Manual Changes?"),
          "This will revert to automatically detected endpoints. Are you sure?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_discard"),
              "Discard",
              class = "btn-warning",
              icon = icon("undo")
            )
          )
        )
      )
    })
    
    # Confirm discard changes
    observeEvent(input$confirm_discard, {
      req(selected_sample())
      
      sample_id <- selected_sample()
      sample <- app_data$processed_data$samples[[sample_id]]
      
      # Capture state before modification (for undo)
      previous_state <- capture_sample_state(sample_id)
      
      if (!is.null(sample$auto_lower_endpoint) && !is.null(sample$auto_upper_endpoint)) {
        result <- reprocess_with_manual_endpoints(
          temperature = sample$temperature_original,
          dcp = sample$dcp_original,
          lower_endpoint = sample$auto_lower_endpoint,
          upper_endpoint = sample$auto_upper_endpoint
        )
        
        if (result$success) {
          # Push to undo stack
          push_undo_state(
            sample_id = sample_id,
            action_type = "discard_manual",
            previous_state = previous_state
          )
          
          # CRITICAL: Save current sample and block grid reactivity
          current_sample <- isolate(selected_sample())
          programmatic_selection(TRUE)
          
          # Update the sample
          isolate({
            app_data$processed_data$samples[[sample_id]]$lower_endpoint <- result$lower_endpoint
            app_data$processed_data$samples[[sample_id]]$upper_endpoint <- result$upper_endpoint
            app_data$processed_data$samples[[sample_id]]$baseline_subtracted <- result$baseline_subtracted
            app_data$processed_data$samples[[sample_id]]$manual_adjustment <- FALSE
            app_data$processed_data$samples[[sample_id]]$lower_manual <- FALSE
            app_data$processed_data$samples[[sample_id]]$upper_manual <- FALSE
          })
          
          # Force selection to stay
          selected_sample(current_sample)
          
          # Restore grid selection
          grid_data <- isolate(sample_grid_data())
          current_idx <- which(grid_data[[1]] == current_sample)
          if (length(current_idx) > 0) {
            proxy <- DT::dataTableProxy("sample_grid")
            DT::selectRows(proxy, current_idx[1])
          }
          
          shinyjs::delay(50, {
            programmatic_selection(FALSE)
          })
          
          showNotification(
            "Manual changes discarded - reverted to auto-detected endpoints",
            type = "message",
            duration = 3
          )
        }
      }
      
      removeModal()
    })
    
    # Handle reviewed checkbox
    observeEvent(input$mark_reviewed, {
      req(selected_sample())
      
      sample_id <- selected_sample()
      previous_state <- capture_sample_state(sample_id)
      
      # Push to undo stack only if value actually changed
      if (previous_state$reviewed != input$mark_reviewed) {
        push_undo_state(
          sample_id = sample_id,
          action_type = "review_status",
          previous_state = previous_state
        )
      }
      
      # CRITICAL: Save current sample and block grid reactivity
      current_sample <- isolate(selected_sample())
      programmatic_selection(TRUE)
      
      # Update the data in isolation
      isolate({
        app_data$processed_data$samples[[sample_id]]$reviewed <- input$mark_reviewed
      })
      
      # Update grid data to show new status
      grid_data <- isolate(sample_grid_data())
      current_idx <- which(grid_data[[1]] == current_sample)
      
      proxy <- DT::dataTableProxy("sample_grid")
      DT::replaceData(proxy = proxy, data = grid_data, resetPaging = FALSE, rownames = FALSE, clearSelection = FALSE)
      
      # Force selection to stay on current sample
      selected_sample(current_sample)
      
      # Restore grid selection explicitly after data replacement
      if (length(current_idx) > 0) {
        shinyjs::delay(20, {
          DT::selectRows(proxy, current_idx[1])
        })
      }
      
      # Release block after delay
      shinyjs::delay(50, {
        programmatic_selection(FALSE)
      })
    }, priority = 10)
    
    # Handle excluded checkbox
    observeEvent(input$mark_excluded, {
      req(selected_sample())
      
      sample_id <- selected_sample()
      previous_state <- capture_sample_state(sample_id)
      
      # Push to undo stack only if value actually changed
      if (previous_state$excluded != input$mark_excluded) {
        push_undo_state(
          sample_id = sample_id,
          action_type = "review_status",
          previous_state = previous_state
        )
      }
      
      # CRITICAL: Save current sample and block grid reactivity
      current_sample <- isolate(selected_sample())
      programmatic_selection(TRUE)
      
      # Update the data in isolation
      isolate({
        app_data$processed_data$samples[[sample_id]]$excluded <- input$mark_excluded
      })
      
      # Update grid data to show new status
      grid_data <- isolate(sample_grid_data())
      current_idx <- which(grid_data[[1]] == current_sample)
      
      proxy <- DT::dataTableProxy("sample_grid")
      DT::replaceData(proxy = proxy, data = grid_data, resetPaging = FALSE, rownames = FALSE, clearSelection = FALSE)
      
      # Force selection to stay on current sample
      selected_sample(current_sample)
      
      # Restore grid selection explicitly after data replacement
      if (length(current_idx) > 0) {
        shinyjs::delay(20, {
          DT::selectRows(proxy, current_idx[1])
        })
      }
      
      # Release block after delay
      shinyjs::delay(50, {
        programmatic_selection(FALSE)
      })
    }, priority = 10)
    
    # Handle Undo button
    observeEvent(input$undo_btn, {
      req(can_undo())
      
      # Pop last entry from undo stack
      last_entry <- app_data$undo_stack[[length(app_data$undo_stack)]]
      app_data$undo_stack <- app_data$undo_stack[-length(app_data$undo_stack)]
      
      # Capture current state for redo
      current_state <- capture_sample_state(last_entry$sample_id)
      
      # Block grid reactivity
      programmatic_selection(TRUE)
      
      # Restore previous state
      isolate({
        restore_sample_state(last_entry$sample_id, last_entry$previous_state)
      })
      
      # Push current state to redo stack
      redo_entry <- list(
        sample_id = last_entry$sample_id,
        action_type = last_entry$action_type,
        previous_state = current_state,
        timestamp = Sys.time()
      )
      app_data$redo_stack <- c(app_data$redo_stack, list(redo_entry))
      
      # Only navigate if we're not already on that sample
      if (isolate(selected_sample()) != last_entry$sample_id) {
        selected_sample(last_entry$sample_id)
      }
      
      # If this was a review status change, update the grid data
      if (last_entry$action_type == "review_status") {
        grid_data <- isolate(sample_grid_data())
        proxy <- DT::dataTableProxy("sample_grid")
        DT::replaceData(proxy = proxy, data = grid_data, resetPaging = FALSE, rownames = FALSE, clearSelection = FALSE)
      }
      
      # Restore grid selection
      grid_data <- isolate(sample_grid_data())
      idx <- which(grid_data[[1]] == last_entry$sample_id)
      if (length(idx) > 0) {
        proxy <- DT::dataTableProxy("sample_grid")
        shinyjs::delay(20, {
          DT::selectRows(proxy, idx[1])
        })
      }
      
      shinyjs::delay(50, {
        programmatic_selection(FALSE)
      })
      
      showNotification(
        sprintf("Undid %s for sample %s", 
                gsub("_", " ", last_entry$action_type), 
                last_entry$sample_id),
        type = "message",
        duration = 2
      )
    })
    
    # Handle Redo button
    observeEvent(input$redo_btn, {
      req(can_redo())
      
      # Pop last entry from redo stack
      last_entry <- app_data$redo_stack[[length(app_data$redo_stack)]]
      app_data$redo_stack <- app_data$redo_stack[-length(app_data$redo_stack)]
      
      # Capture current state for undo
      current_state <- capture_sample_state(last_entry$sample_id)
      
      # Block grid reactivity
      programmatic_selection(TRUE)
      
      # Restore previous state
      isolate({
        restore_sample_state(last_entry$sample_id, last_entry$previous_state)
      })
      
      # Push current state back to undo stack
      undo_entry <- list(
        sample_id = last_entry$sample_id,
        action_type = last_entry$action_type,
        previous_state = current_state,
        timestamp = Sys.time()
      )
      app_data$undo_stack <- c(app_data$undo_stack, list(undo_entry))
      
      # Only navigate if we're not already on that sample
      if (isolate(selected_sample()) != last_entry$sample_id) {
        selected_sample(last_entry$sample_id)
      }
      
      # If this was a review status change, update the grid data
      if (last_entry$action_type == "review_status") {
        grid_data <- isolate(sample_grid_data())
        proxy <- DT::dataTableProxy("sample_grid")
        DT::replaceData(proxy = proxy, data = grid_data, resetPaging = FALSE, rownames = FALSE, clearSelection = FALSE)
      }
      
      # Restore grid selection
      grid_data <- isolate(sample_grid_data())
      idx <- which(grid_data[[1]] == last_entry$sample_id)
      if (length(idx) > 0) {
        proxy <- DT::dataTableProxy("sample_grid")
        shinyjs::delay(20, {
          DT::selectRows(proxy, idx[1])
        })
      }
      
      shinyjs::delay(50, {
        programmatic_selection(FALSE)
      })
      
      showNotification(
        sprintf("Redid %s for sample %s", 
                gsub("_", " ", last_entry$action_type), 
                last_entry$sample_id),
        type = "message",
        duration = 2
      )
    })
    
    # Navigate to previous sample
    observeEvent(input$prev_sample, {
      req(selected_sample())
      
      grid_data <- sample_grid_data()
      current_idx <- which(grid_data[[1]] == selected_sample())
      
      if (current_idx > 1) {
        new_idx <- current_idx - 1
        new_sample <- grid_data[[1]][new_idx]
        selected_sample(new_sample)
        
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
      current_idx <- which(grid_data[[1]] == selected_sample())
      
      if (current_idx < nrow(grid_data)) {
        new_idx <- current_idx + 1
        new_sample <- grid_data[[1]][new_idx]
        selected_sample(new_sample)
        
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