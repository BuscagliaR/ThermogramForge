# Review Endpoints Module - Final Version v3
# Interactive thermogram review with manual endpoint adjustment and full history tracking

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
    
    # =========================================================================
    # REACTIVE VALUES
    # =========================================================================
    
    selected_sample <- reactiveVal(NULL)
    programmatic_selection <- reactiveVal(FALSE)
    plot_view <- reactiveVal("baseline_subtracted")
    adjustment_mode <- reactiveVal(NULL)
    last_click_key <- reactiveVal(NULL)
    
    updating_checkboxes <- reactiveVal(FALSE)
    last_reviewed_value <- reactiveVal(NULL)
    last_excluded_value <- reactiveVal(NULL)
    updating_from_checkbox <- reactiveVal(FALSE)
    processing_adjustment <- reactiveVal(FALSE)
    
    ui_refresh_trigger <- reactiveVal(0)
    
    # =========================================================================
    # HELPER FUNCTIONS
    # =========================================================================
    
    push_undo_state <- function(sample_id, action_type, previous_state) {
      state_entry <- list(
        sample_id = sample_id,
        action_type = action_type,
        previous_state = previous_state,
        timestamp = Sys.time()
      )
      app_data$undo_stack <- c(app_data$undo_stack, list(state_entry))
      app_data$redo_stack <- list()
      cat(sprintf("[UNDO] Pushed state for sample %s (action: %s)\n", 
                  sample_id, action_type))
    }
    
    capture_sample_state <- function(sample_id) {
      sample <- app_data$processed_data$samples[[sample_id]]
      list(
        lower_endpoint = sample$lower_endpoint,
        upper_endpoint = sample$upper_endpoint,
        baseline_subtracted = sample$baseline_subtracted,
        manual_adjustment = if(is.null(sample$manual_adjustment)) FALSE else sample$manual_adjustment,
        lower_manual = if(is.null(sample$lower_manual)) FALSE else sample$lower_manual,
        upper_manual = if(is.null(sample$upper_manual)) FALSE else sample$upper_manual,
        reviewed = if(is.null(sample$reviewed)) FALSE else sample$reviewed,
        excluded = if(is.null(sample$excluded)) FALSE else sample$excluded,
        lower_endpoint_auto = sample$lower_endpoint_auto,
        upper_endpoint_auto = sample$upper_endpoint_auto
      )
    }
    
    restore_sample_state <- function(sample_id, state) {
      app_data$processed_data$samples[[sample_id]]$lower_endpoint <- state$lower_endpoint
      app_data$processed_data$samples[[sample_id]]$upper_endpoint <- state$upper_endpoint
      app_data$processed_data$samples[[sample_id]]$baseline_subtracted <- state$baseline_subtracted
      app_data$processed_data$samples[[sample_id]]$manual_adjustment <- state$manual_adjustment
      app_data$processed_data$samples[[sample_id]]$lower_manual <- state$lower_manual
      app_data$processed_data$samples[[sample_id]]$upper_manual <- state$upper_manual
      app_data$processed_data$samples[[sample_id]]$reviewed <- state$reviewed
      app_data$processed_data$samples[[sample_id]]$excluded <- state$excluded
      if (!is.null(state$lower_endpoint_auto)) {
        app_data$processed_data$samples[[sample_id]]$lower_endpoint_auto <- state$lower_endpoint_auto
      }
      if (!is.null(state$upper_endpoint_auto)) {
        app_data$processed_data$samples[[sample_id]]$upper_endpoint_auto <- state$upper_endpoint_auto
      }
      cat(sprintf("[RESTORE] Restored state for sample %s\n", sample_id))
    }
    
    can_undo <- reactive({
      length(app_data$undo_stack) > 0
    })
    
    can_redo <- reactive({
      length(app_data$redo_stack) > 0
    })
    
    reprocess_with_manual_endpoints <- function(temperature, dcp, lower_endpoint, upper_endpoint) {
      tryCatch({
        valid_idx <- !is.na(temperature) & !is.na(dcp)
        if (sum(valid_idx) < 10) {
          return(list(success = FALSE, message = "Insufficient data points"))
        }
        
        input_data <- data.frame(
          Temperature = temperature[valid_idx],
          dCp = dcp[valid_idx]
        )
        
        baseline_result <- ThermogramBaseline::baseline.subtraction.byhand(
          x = input_data,
          lwr.temp = lower_endpoint,
          upr.temp = upper_endpoint,
          plot.on = FALSE
        )
        
        final_result <- ThermogramBaseline::final.sample.interpolate(
          x = baseline_result,
          grid.temp = seq(45, 90, 0.1),
          plot.on = FALSE
        )
        
        list(
          success = TRUE,
          lower_endpoint = lower_endpoint,
          upper_endpoint = upper_endpoint,
          baseline_subtracted = final_result$dCp
        )
      }, error = function(e) {
        cat(sprintf("[ERROR] Reprocessing failed: %s\n", e$message))
        list(success = FALSE, message = e$message)
      })
    }
    
    # =========================================================================
    # DATASET RELOAD OBSERVER (FIX FOR DATASET SWITCHING BUG)
    # =========================================================================
    
    observeEvent(app_data$dataset_load_trigger, {
      
      req(app_data$dataset_load_trigger > 0)
      
      cat(sprintf("\n[REVIEW_ENDPOINTS] Dataset reload triggered (trigger=%d)\n", 
                  app_data$dataset_load_trigger))
      cat(sprintf("[REVIEW_ENDPOINTS] Loading dataset: %s\n", 
                  app_data$current_dataset_name))
      
      if (is.null(app_data$processed_data)) {
        cat("[REVIEW_ENDPOINTS] WARNING: No processed data available\n")
        return()
      }
      
      # Reset module state for new dataset
      cat("[REVIEW_ENDPOINTS] Resetting module state for new dataset\n")
      
      # Clear selection
      selected_sample(NULL)
      
      # Clear undo/redo stacks
      app_data$undo_stack <- list()
      app_data$redo_stack <- list()
      
      # Reset flags
      adjustment_mode(NULL)
      last_click_key(NULL)
      last_reviewed_value(NULL)
      last_excluded_value(NULL)
      plot_view("baseline_subtracted")
      
      # Reset boolean flags
      programmatic_selection(FALSE)
      updating_checkboxes(FALSE)
      updating_from_checkbox(FALSE)
      processing_adjustment(FALSE)
      
      # Log successful reload
      cat(sprintf("[REVIEW_ENDPOINTS] Dataset '%s' loaded successfully\n", 
                  app_data$current_dataset_name))
      cat(sprintf("[REVIEW_ENDPOINTS] Samples available: %d\n", 
                  length(app_data$processed_data$samples)))
      
      # Show notification to user
      showNotification(
        sprintf("Loaded dataset: %s (%d samples)", 
                app_data$current_dataset_name,
                length(app_data$processed_data$samples)),
        type = "message",
        duration = 3
      )
      
      # Force UI refresh by incrementing trigger
      ui_refresh_trigger(isolate(ui_refresh_trigger()) + 1)
      cat("[REVIEW_ENDPOINTS] UI refresh triggered\n")
      
    }, ignoreInit = TRUE, priority = 100)
    
    
    # =========================================================================
    # MAIN CONTENT RENDERING
    # =========================================================================
    
    output$review_content <- renderUI({
      if (is.null(app_data$processed_data)) {
        return(
          div(
            class = "alert alert-info",
            icon("info-circle"), " No processed data available. Please upload and process data in the Data Overview tab.",
            p(
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
      
      # ===== ADDED FOR BUG FIX - Force UI refresh when dataset changes =====
      ui_refresh_trigger()  # Depend on the trigger to force refresh
      # ======================================================================
      
      tagList(
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
                    h4(icon("chart-line"), " Review Baseline Endpoints", class = "mb-0"),
                    p(
                      class = "text-muted mb-0",
                      sprintf("Dataset: %d samples processed", 
                              app_data$processed_data$summary$n_success)
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
        
        div(
          class = "row",
          div(
            class = "col-md-5",
            div(
              class = "card",
              div(class = "card-header", icon("table"), " Sample Overview"),
              div(class = "card-body", DT::dataTableOutput(ns("sample_grid")))
            )
          ),
          
          div(
            class = "col-md-7",
            div(
              class = "card mb-3",
              div(
                class = "card-header d-flex justify-content-between align-items-center",
                div(uiOutput(ns("plot_header"))),
                div(uiOutput(ns("view_toggle_buttons")))
              ),
              div(
                class = "card-body",
                uiOutput(ns("adjustment_notification")),
                uiOutput(ns("plot_area"))
              )
            ),
            
            div(
              class = "card",
              div(class = "card-header", icon("sliders-h"), " Review Controls"),
              div(class = "card-body", uiOutput(ns("review_controls")))
            )
          )
        )
      )
    })
    
    # =========================================================================
    # NAVIGATION
    # =========================================================================
    
    observeEvent(input$goto_overview, {
      app_data$navigate_to <- "data_overview"
    })
    
    observeEvent(input$view_raw, {
      plot_view("raw")
    })
    
    observeEvent(input$view_baseline, {
      plot_view("baseline_subtracted")
    })
    
    # =========================================================================
    # SAMPLE GRID
    # =========================================================================
    
    sample_grid_data <- reactive({
      req(app_data$processed_data)
      
      samples <- app_data$processed_data$samples
      
      grid_df <- data.frame(
        ID = character(),
        Signal = character(),
        Lower = character(),
        Upper = character(),
        Reviewed = character(),
        Exclude = character(),
        stringsAsFactors = FALSE
      )
      
      for (sample_id in names(samples)) {
        sample <- samples[[sample_id]]
        
        if (sample$success) {
          signal_icon <- if (sample$has_signal) "✓" else "✗"
          
          lower_manual <- if(is.null(sample$lower_manual)) FALSE else sample$lower_manual
          upper_manual <- if(is.null(sample$upper_manual)) FALSE else sample$upper_manual
          
          lower_text <- sprintf("%.1f°C (%s)", 
                                sample$lower_endpoint, 
                                if(lower_manual) "Manual" else "Auto")
          upper_text <- sprintf("%.1f°C (%s)", 
                                sample$upper_endpoint, 
                                if(upper_manual) "Manual" else "Auto")
          
          reviewed_icon <- if (is.null(sample$reviewed) || !sample$reviewed) "—" else "✓"
          excluded_icon <- if (is.null(sample$excluded) || !sample$excluded) "" else "✗"
          
          grid_df <- rbind(grid_df, data.frame(
            ID = sample_id,
            Signal = signal_icon,
            Lower = lower_text,
            Upper = upper_text,
            Reviewed = reviewed_icon,
            Exclude = excluded_icon,
            stringsAsFactors = FALSE
          ))
        }
      }
      
      grid_df
    })
    
    output$sample_grid <- DT::renderDataTable({
      req(app_data$processed_data)
      
      # ===== ADDED FOR BUG FIX =====
      ui_refresh_trigger()  # Force grid refresh when dataset changes
      # =============================
      
      grid_data <- sample_grid_data()  # REMOVED isolate() wrapper
      
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
            list(targets = 0, className = "dt-left", width = "100px"),
            list(targets = 1, className = "dt-center", width = "60px"),
            list(targets = 2, className = "dt-left", width = "120px"),
            list(targets = 3, className = "dt-left", width = "120px"),
            list(targets = 4, className = "dt-center", width = "80px"),
            list(targets = 5, className = "dt-center", width = "70px")
          ),
          autoWidth = FALSE
        ),
        class = "compact hover row-border"
      ) %>%
        DT::formatStyle(
          'Signal',
          color = DT::styleEqual(c('✓', '✗'), c('#198754', '#dc3545')),
          fontWeight = 'bold',
          fontSize = '16px'
        ) %>%
        DT::formatStyle(
          'Reviewed',
          color = DT::styleEqual(c('✓', '—'), c('#198754', '#6c757d')),
          fontWeight = 'bold',
          fontSize = '16px'
        ) %>%
        DT::formatStyle(
          'Exclude',
          color = '#dc3545',
          fontWeight = 'bold',
          fontSize = '16px'
        )
    })
    
    # =========================================================================
    # GRID SELECTION HANDLER
    # =========================================================================
    
    observeEvent(input$sample_grid_rows_selected, {
      if (isolate(updating_from_checkbox())) {
        cat("[GRID] Ignoring row selection - checkbox update in progress\n")
        return()
      }
      
      if (isolate(programmatic_selection())) {
        cat("[GRID] Ignoring row selection - programmatic\n")
        return()
      }
      
      req(input$sample_grid_rows_selected)
      req(app_data$processed_data)
      
      grid_data <- sample_grid_data()
      req(nrow(grid_data) > 0)
      
      selected_idx <- input$sample_grid_rows_selected
      if (selected_idx < 1 || selected_idx > nrow(grid_data)) return()
      
      new_sample_id <- as.character(grid_data[[1]][selected_idx])
      old_sample_id <- isolate(selected_sample())
      
      if (!is.null(old_sample_id) && old_sample_id == new_sample_id) return()
      
      cat(sprintf("[GRID] User selected row %d: sample %s\n", selected_idx, new_sample_id))
      
      if (!is.null(adjustment_mode())) {
        adjustment_mode(NULL)
        removeNotification(id = "adjustment_notification")
      }
      
      sample <- app_data$processed_data$samples[[new_sample_id]]
      last_reviewed_value(if(is.null(sample$reviewed)) FALSE else sample$reviewed)
      last_excluded_value(if(is.null(sample$excluded)) FALSE else sample$excluded)
      
      selected_sample(new_sample_id)
      
    }, priority = 5, ignoreInit = FALSE)
    
    observe({
      req(app_data$processed_data)
      
      if (is.null(selected_sample())) {
        grid_data <- sample_grid_data()
        if (nrow(grid_data) > 0) {
          first_sample_id <- grid_data[[1]][1]
          selected_sample(first_sample_id)
          
          sample <- app_data$processed_data$samples[[first_sample_id]]
          last_reviewed_value(if(is.null(sample$reviewed)) FALSE else sample$reviewed)
          last_excluded_value(if(is.null(sample$excluded)) FALSE else sample$excluded)
          
          cat(sprintf("[INIT] Auto-selected first sample: %s\n", first_sample_id))
        }
      }
    })
    
    # =========================================================================
    # PLOT HEADER AND VIEW TOGGLE
    # =========================================================================
    
    output$plot_header <- renderUI({
      req(selected_sample())
      
      sample <- app_data$processed_data$samples[[selected_sample()]]
      
      tagList(
        icon("chart-line"), 
        sprintf(" Thermogram: %s", selected_sample()),
        if (!sample$has_signal) {
          span(
            icon("exclamation-triangle"),
            " No Signal Detected",
            class = "badge bg-warning ms-2"
          )
        }
      )
    })
    
    output$view_toggle_buttons <- renderUI({
      current_view <- plot_view()
      
      div(
        class = "btn-group btn-group-sm",
        role = "group",
        actionButton(
          ns("view_raw"),
          "Raw Thermogram",
          class = if (current_view == "raw") "btn btn-primary" else "btn btn-outline-primary"
        ),
        actionButton(
          ns("view_baseline"),
          "Baseline Subtracted",
          class = if (current_view == "baseline_subtracted") "btn btn-primary" else "btn btn-outline-primary"
        )
      )
    })
    
    # =========================================================================
    # ADJUSTMENT NOTIFICATION
    # =========================================================================
    
    output$adjustment_notification <- renderUI({
      mode <- adjustment_mode()
      
      if (!is.null(mode)) {
        div(
          class = "alert alert-info d-flex justify-content-between align-items-center mb-3",
          div(
            icon("hand-pointer"),
            sprintf(" Click on the plot to set the %s endpoint", mode)
          ),
          actionButton(
            ns("cancel_adjustment"),
            "Cancel",
            icon = icon("times"),
            class = "btn btn-sm btn-outline-secondary"
          )
        )
      }
    })
    
    observeEvent(input$cancel_adjustment, {
      cat("[CANCEL] Canceling adjustment mode\n")
      adjustment_mode(NULL)
    })
    
    # =========================================================================
    # PLOT RENDERING
    # =========================================================================
    
    output$plot_area <- renderUI({
      plotly::plotlyOutput(ns("thermogram_plot"), height = "450px")
    })
    
    output$thermogram_plot <- plotly::renderPlotly({
      req(selected_sample())
      req(app_data$processed_data)
      
      sample <- app_data$processed_data$samples[[selected_sample()]]
      
      if (!sample$success) return(NULL)
      
      current_view <- plot_view()
      
      if (current_view == "baseline_subtracted") {
        y_data <- as.numeric(sample$baseline_subtracted)
        x_data <- as.numeric(sample$temperature)
        y_label <- "dCp (Baseline Subtracted)"
      } else {
        if (!is.null(sample$temperature_original)) {
          x_data <- as.numeric(sample$temperature_original)
        } else {
          x_data <- as.numeric(sample$temperature)
        }
        y_data <- as.numeric(sample$dcp_original)
        y_label <- "dCp (Raw)"
      }
      
      min_len <- min(length(x_data), length(y_data))
      x_data <- x_data[1:min_len]
      y_data <- y_data[1:min_len]
      
      valid_idx <- !is.na(x_data) & !is.na(y_data)
      x_data <- x_data[valid_idx]
      y_data <- y_data[valid_idx]
      
      if (length(x_data) < 2) {
        return(plotly::plot_ly() %>%
                 plotly::layout(
                   title = "Insufficient data to plot",
                   xaxis = list(title = "Temperature (°C)"),
                   yaxis = list(title = y_label)
                 ))
      }
      
      p <- plotly::plot_ly(source = "thermogram_plot")
      p <- plotly::event_register(p, 'plotly_click')  # Register event EARLY
      
      p <- p %>%
        plotly::add_trace(
          x = x_data,
          y = y_data,
          type = "scatter",
          mode = "lines",
          line = list(color = "#1f77b4", width = 2),
          name = "Thermogram",
          hovertemplate = paste(
            "<b>Temperature:</b> %{x:.1f}°C<br>",
            "<b>", y_label, ":</b> %{y:.3f}<br>",
            "<extra></extra>"
          )
        )
      
      # FIXED: Add endpoint lines for BOTH views
      y_min <- min(y_data, na.rm = TRUE)
      y_max <- max(y_data, na.rm = TRUE)
      
      lower_endpoint <- as.numeric(sample$lower_endpoint)
      upper_endpoint <- as.numeric(sample$upper_endpoint)
      
      if (current_view == "baseline_subtracted") {
        p <- p %>%
          plotly::add_trace(
            x = c(lower_endpoint, upper_endpoint, upper_endpoint, lower_endpoint),
            y = c(y_min, y_min, y_max, y_max),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = "rgba(200, 200, 200, 0.1)",
            name = "Transition Region",
            showlegend = FALSE,
            hoverinfo = "skip"
          )
      }
      
      # Add endpoint lines (visible in both views)
      p <- p %>%
        plotly::add_trace(
          x = rep(lower_endpoint, 2),
          y = c(y_min, y_max),
          type = "scatter",
          mode = "lines",
          line = list(color = "#2ca02c", width = 2, dash = "dash"),
          name = "Lower Endpoint",
          showlegend = TRUE,
          hoverinfo = "skip"
        ) %>%
        plotly::add_trace(
          x = rep(upper_endpoint, 2),
          y = c(y_min, y_max),
          type = "scatter",
          mode = "lines",
          line = list(color = "#9467bd", width = 2, dash = "dash"),
          name = "Upper Endpoint",
          showlegend = TRUE,
          hoverinfo = "skip"
        )
      
      plot_bg <- "#ffffff"
      if (!is.null(adjustment_mode())) {
        plot_bg <- "#fffef0"
      }
      
      p <- p %>%
        plotly::layout(
          title = NULL,
          xaxis = list(title = "Temperature (°C)", gridcolor = "#dee2e6", showgrid = TRUE),
          yaxis = list(title = y_label, gridcolor = "#dee2e6", showgrid = TRUE),
          hovermode = "closest",
          plot_bgcolor = plot_bg,
          paper_bgcolor = "#ffffff",
          legend = list(
            x = 0.02, y = 0.98,
            bgcolor = "rgba(255, 255, 255, 0.8)",
            bordercolor = "#dee2e6",
            borderwidth = 1
          ),
          dragmode = "pan"
        ) %>%
        plotly::config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("lasso2d", "select2d"),
          displaylogo = FALSE
        )
      
      p
    })
    
    # =========================================================================
    # PLOT CLICK HANDLER WITH CRASH PROTECTION
    # =========================================================================
    
    observeEvent(plotly::event_data("plotly_click", source = "thermogram_plot"), {
      click <- plotly::event_data("plotly_click", source = "thermogram_plot")
      
      if (is.null(click)) return()
      
      # FIXED: Protect against NULL or empty adjustment_mode
      mode <- adjustment_mode()
      if (is.null(mode) || length(mode) == 0) {
        cat("[CLICK] No adjustment mode set - ignoring click\n")
        return()
      }
      
      if (is.null(selected_sample())) return()
      if (is.na(click$x) || length(click$x) == 0) return()
      
      clicked_temp <- as.numeric(click$x)
      if (is.na(clicked_temp)) return()
      
      click_key <- paste(mode, clicked_temp, Sys.time())
      
      if (!is.null(last_click_key()) && last_click_key() == click_key) {
        cat("[CLICK] Duplicate click ignored\n")
        return()
      }
      
      # FIXED: Set processing flag
      processing_adjustment(TRUE)
      
      last_click_key(click_key)
      
      sample <- app_data$processed_data$samples[[selected_sample()]]
      
      cat(sprintf("\n[CLICK] Processing %s endpoint at %.2f°C\n", mode, clicked_temp))
      
      # FIXED: More robust endpoint validation
      lower_ep <- sample$lower_endpoint
      upper_ep <- sample$upper_endpoint
      
      # Check for NULL or invalid endpoints
      if (is.null(lower_ep) || length(lower_ep) == 0 || is.na(lower_ep)) {
        cat("[CLICK] ERROR: Invalid lower_endpoint\n")
        showNotification("Error: Invalid lower endpoint value", type = "error", duration = 3)
        adjustment_mode(NULL)
        processing_adjustment(FALSE)
        return()
      }
      
      if (is.null(upper_ep) || length(upper_ep) == 0 || is.na(upper_ep)) {
        cat("[CLICK] ERROR: Invalid upper_endpoint\n")
        showNotification("Error: Invalid upper endpoint value", type = "error", duration = 3)
        adjustment_mode(NULL)
        processing_adjustment(FALSE)
        return()
      }
      
      lower_ep <- as.numeric(lower_ep)
      upper_ep <- as.numeric(upper_ep)
      
      cat(sprintf("[CLICK] Current endpoints - lower: %.2f, upper: %.2f\n", lower_ep, upper_ep))
      
      if (mode == "lower" && clicked_temp >= upper_ep) {
        showNotification(
          "Lower endpoint must be less than upper endpoint",
          type = "error",
          duration = 3
        )
        processing_adjustment(FALSE)
        return()
      }
      
      if (mode == "upper" && clicked_temp <= lower_ep) {
        showNotification(
          "Upper endpoint must be greater than lower endpoint",
          type = "error",
          duration = 3
        )
        processing_adjustment(FALSE)
        return()
      }
      
      # Store auto endpoints before first manual adjustment
      if (is.null(sample$lower_endpoint_auto)) {
        app_data$processed_data$samples[[selected_sample()]]$lower_endpoint_auto <- sample$lower_endpoint
        app_data$processed_data$samples[[selected_sample()]]$upper_endpoint_auto <- sample$upper_endpoint
        cat("[STORE] Saved original auto endpoints for future discard\n")
      }
      
      previous_state <- capture_sample_state(selected_sample())
      
      # FIXED: Use validated endpoint values
      new_lower <- if (mode == "lower") clicked_temp else lower_ep
      new_upper <- if (mode == "upper") clicked_temp else upper_ep
      
      cat(sprintf("[CLICK] New endpoints will be - lower: %.2f, upper: %.2f\n", new_lower, new_upper))
      
      result <- reprocess_with_manual_endpoints(
        temperature = sample$temperature_original,
        dcp = sample$dcp_original,
        lower_endpoint = new_lower,
        upper_endpoint = new_upper
      )
      
      if (result$success) {
        push_undo_state(
          sample_id = selected_sample(),
          action_type = paste0("adjust_", mode),
          previous_state = previous_state
        )
        
        isolate({
          app_data$processed_data$samples[[selected_sample()]]$lower_endpoint <- result$lower_endpoint
          app_data$processed_data$samples[[selected_sample()]]$upper_endpoint <- result$upper_endpoint
          app_data$processed_data$samples[[selected_sample()]]$baseline_subtracted <- result$baseline_subtracted
          app_data$processed_data$samples[[selected_sample()]]$manual_adjustment <- TRUE
          
          if (mode == "lower") {
            app_data$processed_data$samples[[selected_sample()]]$lower_manual <- TRUE
          } else {
            app_data$processed_data$samples[[selected_sample()]]$upper_manual <- TRUE
          }
        })
        
        programmatic_selection(TRUE)
        grid_data <- sample_grid_data()
        current_idx <- which(grid_data[[1]] == selected_sample())
        
        proxy <- DT::dataTableProxy("sample_grid")
        DT::replaceData(proxy = proxy, data = grid_data, 
                        resetPaging = FALSE, rownames = FALSE, clearSelection = FALSE)
        
        if (length(current_idx) > 0) {
          shinyjs::delay(50, {
            DT::selectRows(proxy, current_idx[1])
          })
        }
        
        shinyjs::delay(100, { programmatic_selection(FALSE) })
        
        adjustment_mode(NULL)
        
        # FIXED: Clear processing flag after delay to allow UI to settle
        shinyjs::delay(200, { processing_adjustment(FALSE) })
        
        showNotification(
          sprintf("%s endpoint updated to %.1f°C", 
                  tools::toTitleCase(mode), result[[paste0(mode, "_endpoint")]]),
          type = "message",
          duration = 2
        )
      } else {
        processing_adjustment(FALSE)
        showNotification(
          paste("Failed to update endpoint:", result$message),
          type = "error",
          duration = 5
        )
      }
    })
    
    # =========================================================================
    # REVIEW CONTROLS WITH VERTICAL DIVIDER
    # =========================================================================
    
    output$review_controls <- renderUI({
      req(selected_sample())
      req(app_data$processed_data)
      
      sample <- app_data$processed_data$samples[[selected_sample()]]
      
      reviewed_value <- if(is.null(sample$reviewed)) FALSE else sample$reviewed
      excluded_value <- if(is.null(sample$excluded)) FALSE else sample$excluded
      
      updating_checkboxes(TRUE)
      last_reviewed_value(reviewed_value)
      last_excluded_value(excluded_value)
      shinyjs::delay(150, { updating_checkboxes(FALSE) })
      
      lower_manual <- if(is.null(sample$lower_manual)) FALSE else sample$lower_manual
      upper_manual <- if(is.null(sample$upper_manual)) FALSE else sample$upper_manual
      
      lower_badge <- if (lower_manual) {
        span(icon("hand-pointer"), " Manual", class = "badge bg-warning text-dark ms-1", style = "font-size: 0.75rem;")
      } else {
        span(icon("robot"), " Auto", class = "badge bg-primary ms-1", style = "font-size: 0.75rem;")
      }
      
      upper_badge <- if (upper_manual) {
        span(icon("hand-pointer"), " Manual", class = "badge bg-warning text-dark ms-1", style = "font-size: 0.75rem;")
      } else {
        span(icon("robot"), " Auto", class = "badge bg-primary ms-1", style = "font-size: 0.75rem;")
      }
      
      # FIXED: Added vertical divider and improved layout
      tagList(
        # Header row
        div(
          class = "row mb-2",
          div(
            class = "col-6",
            strong("ID: ", style = "font-size: 1rem;"),
            span(selected_sample(), style = "font-size: 1rem;")
          ),
          div(
            class = "col-6 d-flex justify-content-end gap-2",
            div(
              class = "form-check form-check-inline mb-0",
              checkboxInput(
                ns("mark_reviewed"),
                label = span("Reviewed", style = "font-size: 0.85rem;"),
                value = reviewed_value
              )
            ),
            div(
              class = "form-check form-check-inline mb-0",
              checkboxInput(
                ns("mark_excluded"),
                label = span("Exclude", style = "font-size: 0.85rem;"),
                value = excluded_value
              )
            )
          )
        ),
        
        hr(style = "margin: 0.5rem 0;"),
        
        # Endpoints with vertical divider
        div(
          class = "row mb-1",
          div(
            class = "col-6",
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                span("Lower Endpoint: ", style = "font-weight: 500; font-size: 0.85rem;"),
                span(sprintf("%.1f°C", sample$lower_endpoint), style = "font-size: 0.85rem;"),
                lower_badge
              ),
              actionButton(
                ns("adjust_lower"),
                span("Manually Adjust ", icon("pencil")),
                class = "btn btn-sm btn-outline-primary",
                style = "padding: 0.15rem 0.5rem; font-size: 0.75rem;",
                disabled = !is.null(adjustment_mode())
              )
            )
          ),
          # FIXED: Vertical divider
          div(
            class = "col-6",
            style = "border-left: 3px solid #dee2e6; padding-left: 0.75rem;",
            div(
              class = "d-flex justify-content-between align-items-center",
              div(
                span("Upper Endpoint: ", style = "font-weight: 500; font-size: 0.85rem;"),
                span(sprintf("%.1f°C", sample$upper_endpoint), style = "font-size: 0.85rem;"),
                upper_badge
              ),
              actionButton(
                ns("adjust_upper"),
                span("Manually Adjust ", icon("pencil")),
                class = "btn btn-sm btn-outline-primary",
                style = "padding: 0.15rem 0.5rem; font-size: 0.75rem;",
                disabled = !is.null(adjustment_mode())
              )
            )
          )
        ),
        
        if (lower_manual || upper_manual) {
          div(
            class = "mb-2 mt-2",
            actionButton(
              ns("discard_changes"),
              "Discard Manual Changes",
              icon = icon("undo"),
              class = "btn btn-sm btn-outline-warning w-100",
              style = "font-size: 0.85rem;"
            )
          )
        },
        
        hr(style = "margin: 0.5rem 0;"),
        
        div(
          class = "d-flex justify-content-between",
          div(
            class = "btn-group btn-group-sm",
            actionButton(ns("prev_sample"), icon("arrow-left"), class = "btn btn-secondary"),
            actionButton(ns("next_sample"), icon("arrow-right"), class = "btn btn-secondary")
          ),
          div(
            class = "btn-group btn-group-sm",
            actionButton(
              ns("undo_btn"),
              icon("undo"),
              class = if (can_undo()) "btn btn-outline-secondary" else "btn btn-outline-secondary disabled"
            ),
            actionButton(
              ns("redo_btn"),
              icon("redo"),
              class = if (can_redo()) "btn btn-outline-secondary" else "btn btn-outline-secondary disabled"
            )
          )
        )
      )
    })
    
    # =========================================================================
    # CHECKBOX HANDLERS
    # =========================================================================
    
    observeEvent(input$mark_reviewed, {
      req(selected_sample())
      req(!is.null(input$mark_reviewed))
      
      if (isolate(updating_checkboxes())) {
        cat("[CHECKBOX] Blocked - UI is updating\n")
        return()
      }
      
      current_sample_id <- isolate(selected_sample())
      
      if (isolate(programmatic_selection())) {
        cat(sprintf("[CHECKBOX] Ignoring mark_reviewed - programmatic for %s\n", current_sample_id))
        return()
      }
      
      new_value <- input$mark_reviewed
      
      last_value <- isolate(last_reviewed_value())
      if (!is.null(last_value) && last_value == new_value) {
        cat(sprintf("[CHECKBOX] Ignoring mark_reviewed - unchanged (sample=%s, value=%s)\n",
                    current_sample_id, new_value))
        return()
      }
      
      cat(sprintf("[CHECKBOX] Processing mark_reviewed: sample=%s, old=%s, new=%s\n",
                  current_sample_id, last_value, new_value))
      
      previous_state <- capture_sample_state(current_sample_id)
      
      push_undo_state(
        sample_id = current_sample_id,
        action_type = "review_status",
        previous_state = previous_state
      )
      
      updating_from_checkbox(TRUE)
      programmatic_selection(TRUE)
      
      isolate({
        app_data$processed_data$samples[[current_sample_id]]$reviewed <- new_value
        last_reviewed_value(new_value)
        cat(sprintf("[DATA] Updated sample %s reviewed=%s\n", current_sample_id, new_value))
      })
      
      updated_grid_data <- isolate(sample_grid_data())
      current_idx <- which(updated_grid_data[[1]] == current_sample_id)
      
      if (length(current_idx) > 0) {
        proxy <- DT::dataTableProxy("sample_grid")
        DT::replaceData(proxy = proxy, data = updated_grid_data, 
                        resetPaging = FALSE, rownames = FALSE, clearSelection = FALSE)
        
        shinyjs::delay(50, {
          DT::selectRows(proxy, current_idx[1])
        })
      }
      
      shinyjs::delay(150, {
        programmatic_selection(FALSE)
        updating_from_checkbox(FALSE)
      })
      
      showNotification(
        sprintf("Sample %s marked as %s", 
                current_sample_id, 
                if(new_value) "reviewed" else "not reviewed"),
        type = "message",
        duration = 2
      )
      
    }, priority = 10, ignoreInit = TRUE)
    
    observeEvent(input$mark_excluded, {
      req(selected_sample())
      req(!is.null(input$mark_excluded))
      
      if (isolate(updating_checkboxes())) {
        cat("[CHECKBOX] Blocked - UI is updating\n")
        return()
      }
      
      current_sample_id <- isolate(selected_sample())
      
      if (isolate(programmatic_selection())) {
        cat(sprintf("[CHECKBOX] Ignoring mark_excluded - programmatic for %s\n", current_sample_id))
        return()
      }
      
      new_value <- input$mark_excluded
      
      last_value <- isolate(last_excluded_value())
      if (!is.null(last_value) && last_value == new_value) {
        cat(sprintf("[CHECKBOX] Ignoring mark_excluded - unchanged (sample=%s, value=%s)\n",
                    current_sample_id, new_value))
        return()
      }
      
      cat(sprintf("[CHECKBOX] Processing mark_excluded: sample=%s, old=%s, new=%s\n",
                  current_sample_id, last_value, new_value))
      
      previous_state <- capture_sample_state(current_sample_id)
      
      push_undo_state(
        sample_id = current_sample_id,
        action_type = "review_status",
        previous_state = previous_state
      )
      
      updating_from_checkbox(TRUE)
      programmatic_selection(TRUE)
      
      isolate({
        app_data$processed_data$samples[[current_sample_id]]$excluded <- new_value
        last_excluded_value(new_value)
        cat(sprintf("[DATA] Updated sample %s excluded=%s\n", current_sample_id, new_value))
      })
      
      updated_grid_data <- isolate(sample_grid_data())
      current_idx <- which(updated_grid_data[[1]] == current_sample_id)
      
      if (length(current_idx) > 0) {
        proxy <- DT::dataTableProxy("sample_grid")
        DT::replaceData(proxy = proxy, data = updated_grid_data,
                        resetPaging = FALSE, rownames = FALSE, clearSelection = FALSE)
        
        shinyjs::delay(50, {
          DT::selectRows(proxy, current_idx[1])
        })
      }
      
      shinyjs::delay(150, {
        programmatic_selection(FALSE)
        updating_from_checkbox(FALSE)
      })
      
      showNotification(
        sprintf("Sample %s %s from analysis", 
                current_sample_id, 
                if(new_value) "excluded" else "included"),
        type = "message",
        duration = 2
      )
      
    }, priority = 10, ignoreInit = TRUE)
    
    # =========================================================================
    # ADJUSTMENT BUTTON HANDLERS WITH PROTECTION
    # =========================================================================
    
    observeEvent(input$adjust_lower, {
      # FIXED: Block during updates
      if (isolate(updating_checkboxes())) {
        cat("[BUTTON] Ignoring adjust_lower - UI updating\n")
        return()
      }
      if (isolate(programmatic_selection())) {
        cat("[BUTTON] Ignoring adjust_lower - programmatic\n")
        return()
      }
      if (isolate(updating_from_checkbox())) {
        cat("[BUTTON] Ignoring adjust_lower - checkbox update\n")
        return()
      }
      if (isolate(processing_adjustment())) {
        cat("[BUTTON] Ignoring adjust_lower - already processing\n")
        return()
      }
      
      cat("\n[BUTTON] Adjust Lower clicked\n")
      adjustment_mode("lower")
      last_click_key(NULL)
    }, ignoreInit = TRUE, priority = 10)
    
    observeEvent(input$adjust_upper, {
      # FIXED: Block during updates
      if (isolate(updating_checkboxes())) {
        cat("[BUTTON] Ignoring adjust_upper - UI updating\n")
        return()
      }
      if (isolate(programmatic_selection())) {
        cat("[BUTTON] Ignoring adjust_upper - programmatic\n")
        return()
      }
      if (isolate(updating_from_checkbox())) {
        cat("[BUTTON] Ignoring adjust_upper - checkbox update\n")
        return()
      }
      if (isolate(processing_adjustment())) {
        cat("[BUTTON] Ignoring adjust_upper - already processing\n")
        return()
      }
      
      cat("\n[BUTTON] Adjust Upper clicked\n")
      adjustment_mode("upper")
      last_click_key(NULL)
    }, ignoreInit = TRUE, priority = 10)
    
    # =========================================================================
    # DISCARD CHANGES HANDLER
    # =========================================================================
    
    observeEvent(input$discard_changes, {
      req(selected_sample())
      showModal(
        modalDialog(
          title = tagList(icon("exclamation-triangle"), " Discard Manual Changes?"),
          "This will revert to automatically detected endpoints. Are you sure?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_discard"), "Discard Changes", class = "btn-danger")
          ),
          easyClose = FALSE
        )
      )
    })
    
    observeEvent(input$confirm_discard, {
      sample_id <- selected_sample()
      sample <- app_data$processed_data$samples[[sample_id]]
      
      cat(sprintf("[DISCARD] Attempting to discard for sample %s\n", sample_id))
      
      if (!is.null(sample$lower_endpoint_auto) && !is.null(sample$upper_endpoint_auto)) {
        cat(sprintf("[DISCARD] Found auto endpoints: lower=%.1f, upper=%.1f\n", 
                    sample$lower_endpoint_auto, sample$upper_endpoint_auto))
        
        previous_state <- capture_sample_state(sample_id)
        
        push_undo_state(
          sample_id = sample_id,
          action_type = "discard_changes",
          previous_state = previous_state
        )
        
        result <- reprocess_with_manual_endpoints(
          temperature = sample$temperature_original,
          dcp = sample$dcp_original,
          lower_endpoint = sample$lower_endpoint_auto,
          upper_endpoint = sample$upper_endpoint_auto
        )
        
        if (result$success) {
          updating_from_checkbox(TRUE)
          programmatic_selection(TRUE)
          
          isolate({
            app_data$processed_data$samples[[sample_id]]$lower_endpoint <- result$lower_endpoint
            app_data$processed_data$samples[[sample_id]]$upper_endpoint <- result$upper_endpoint
            app_data$processed_data$samples[[sample_id]]$baseline_subtracted <- result$baseline_subtracted
            app_data$processed_data$samples[[sample_id]]$manual_adjustment <- FALSE
            app_data$processed_data$samples[[sample_id]]$lower_manual <- FALSE
            app_data$processed_data$samples[[sample_id]]$upper_manual <- FALSE
          })
          
          grid_data <- sample_grid_data()
          current_idx <- which(grid_data[[1]] == sample_id)
          
          proxy <- DT::dataTableProxy("sample_grid")
          DT::replaceData(proxy = proxy, data = grid_data, 
                          resetPaging = FALSE, rownames = FALSE, clearSelection = FALSE)
          
          if (length(current_idx) > 0) {
            shinyjs::delay(50, {
              DT::selectRows(proxy, current_idx[1])
            })
          }
          
          shinyjs::delay(150, { 
            programmatic_selection(FALSE)
            updating_from_checkbox(FALSE)
          })
          
          cat("[DISCARD] Successfully reverted to auto endpoints\n")
          
          showNotification(
            "Manual changes discarded - reverted to auto-detected endpoints",
            type = "message",
            duration = 3
          )
        } else {
          cat(sprintf("[DISCARD] Reprocessing failed: %s\n", result$message))
          showNotification(
            paste("Failed to revert endpoints:", result$message),
            type = "error",
            duration = 5
          )
        }
      } else {
        cat("[DISCARD] No auto endpoints found - sample was not manually adjusted\n")
        showNotification(
          "No manual changes to discard",
          type = "warning",
          duration = 3
        )
      }
      
      removeModal()
    })
    
    # =========================================================================
    # UNDO/REDO HANDLERS
    # =========================================================================
    
    observeEvent(input$undo_btn, {
      req(can_undo())
      
      last_entry <- app_data$undo_stack[[length(app_data$undo_stack)]]
      app_data$undo_stack <- app_data$undo_stack[-length(app_data$undo_stack)]
      
      current_state <- capture_sample_state(last_entry$sample_id)
      
      updating_from_checkbox(TRUE)
      programmatic_selection(TRUE)
      isolate({ restore_sample_state(last_entry$sample_id, last_entry$previous_state) })
      
      redo_entry <- list(
        sample_id = last_entry$sample_id,
        action_type = last_entry$action_type,
        previous_state = current_state,
        timestamp = Sys.time()
      )
      app_data$redo_stack <- c(app_data$redo_stack, list(redo_entry))
      
      if (isolate(selected_sample()) != last_entry$sample_id) {
        selected_sample(last_entry$sample_id)
      }
      
      sample <- app_data$processed_data$samples[[last_entry$sample_id]]
      last_reviewed_value(if(is.null(sample$reviewed)) FALSE else sample$reviewed)
      last_excluded_value(if(is.null(sample$excluded)) FALSE else sample$excluded)
      
      grid_data <- sample_grid_data()
      current_idx <- which(grid_data[[1]] == last_entry$sample_id)
      
      proxy <- DT::dataTableProxy("sample_grid")
      DT::replaceData(proxy = proxy, data = grid_data, 
                      resetPaging = FALSE, rownames = FALSE, clearSelection = FALSE)
      
      if (length(current_idx) > 0) {
        shinyjs::delay(50, {
          DT::selectRows(proxy, current_idx[1])
        })
      }
      
      shinyjs::delay(150, { 
        programmatic_selection(FALSE)
        updating_from_checkbox(FALSE)
      })
      
      showNotification(
        sprintf("Undid %s for sample %s", gsub("_", " ", last_entry$action_type), last_entry$sample_id),
        type = "message", duration = 2
      )
    })
    
    observeEvent(input$redo_btn, {
      req(can_redo())
      
      last_entry <- app_data$redo_stack[[length(app_data$redo_stack)]]
      app_data$redo_stack <- app_data$redo_stack[-length(app_data$redo_stack)]
      
      current_state <- capture_sample_state(last_entry$sample_id)
      
      updating_from_checkbox(TRUE)
      programmatic_selection(TRUE)
      isolate({ restore_sample_state(last_entry$sample_id, last_entry$previous_state) })
      
      undo_entry <- list(
        sample_id = last_entry$sample_id,
        action_type = last_entry$action_type,
        previous_state = current_state,
        timestamp = Sys.time()
      )
      app_data$undo_stack <- c(app_data$undo_stack, list(undo_entry))
      
      if (isolate(selected_sample()) != last_entry$sample_id) {
        selected_sample(last_entry$sample_id)
      }
      
      sample <- app_data$processed_data$samples[[last_entry$sample_id]]
      last_reviewed_value(if(is.null(sample$reviewed)) FALSE else sample$reviewed)
      last_excluded_value(if(is.null(sample$excluded)) FALSE else sample$excluded)
      
      grid_data <- sample_grid_data()
      current_idx <- which(grid_data[[1]] == last_entry$sample_id)
      
      proxy <- DT::dataTableProxy("sample_grid")
      DT::replaceData(proxy = proxy, data = grid_data, 
                      resetPaging = FALSE, rownames = FALSE, clearSelection = FALSE)
      
      if (length(current_idx) > 0) {
        shinyjs::delay(50, {
          DT::selectRows(proxy, current_idx[1])
        })
      }
      
      shinyjs::delay(150, { 
        programmatic_selection(FALSE)
        updating_from_checkbox(FALSE)
      })
      
      showNotification(
        sprintf("Redid %s for sample %s", gsub("_", " ", last_entry$action_type), last_entry$sample_id),
        type = "message", duration = 2
      )
    })
    
    # =========================================================================
    # NAVIGATION HANDLERS
    # =========================================================================
    
    observeEvent(input$prev_sample, {
      req(selected_sample())
      req(app_data$processed_data)
      
      grid_data <- sample_grid_data()
      current_idx <- which(grid_data[[1]] == selected_sample())
      
      if (length(current_idx) > 0 && current_idx[1] > 1) {
        new_idx <- current_idx[1] - 1
        programmatic_selection(TRUE)
        selected_sample(grid_data[[1]][new_idx])
        
        sample <- app_data$processed_data$samples[[grid_data[[1]][new_idx]]]
        last_reviewed_value(if(is.null(sample$reviewed)) FALSE else sample$reviewed)
        last_excluded_value(if(is.null(sample$excluded)) FALSE else sample$excluded)
        
        proxy <- DT::dataTableProxy("sample_grid")
        DT::selectRows(proxy, new_idx)
        
        shinyjs::delay(100, { programmatic_selection(FALSE) })
      }
    })
    
    observeEvent(input$next_sample, {
      req(selected_sample())
      req(app_data$processed_data)
      
      grid_data <- sample_grid_data()
      current_idx <- which(grid_data[[1]] == selected_sample())
      
      if (length(current_idx) > 0 && current_idx[1] < nrow(grid_data)) {
        new_idx <- current_idx[1] + 1
        programmatic_selection(TRUE)
        selected_sample(grid_data[[1]][new_idx])
        
        sample <- app_data$processed_data$samples[[grid_data[[1]][new_idx]]]
        last_reviewed_value(if(is.null(sample$reviewed)) FALSE else sample$reviewed)
        last_excluded_value(if(is.null(sample$excluded)) FALSE else sample$excluded)
        
        proxy <- DT::dataTableProxy("sample_grid")
        DT::selectRows(proxy, new_idx)
        
        shinyjs::delay(100, { programmatic_selection(FALSE) })
      }
    })
    
    # =========================================================================
    # SAVE PROCESSED DATA
    # =========================================================================
    
    observeEvent(input$save_btn, {
      req(app_data$processed_data)
      
      # Generate default filename with timestamp
      default_filename <- format(Sys.time(), "thermogram_processed_%Y%m%d_%H%M%S")
      
      showModal(
        modalDialog(
          title = tagList(icon("save"), " Save Processed Data"),
          size = "m",
          
          textInput(
            ns("save_filename"),
            "Filename:",
            value = default_filename,
            placeholder = "Enter filename without extension"
          ),
          
          selectInput(
            ns("save_format"),
            "Format:",
            choices = c(
              "RDS (R Data - Full data, can reload)" = "rds",
              "CSV (Wide format - Export only)" = "csv",
              "Excel (Wide format + metadata - Export only)" = "xlsx"
            ),
            selected = "rds"
          ),
          
          div(
            class = "alert alert-info mt-3",
            icon("info-circle"), 
            strong(" Format Information:"),
            tags$ul(
              tags$li(strong("RDS:"), " Saves complete data including all sample curves, can be reloaded into app"),
              tags$li(strong("CSV/Excel:"), " Saves wide-format interpolated data (Temperature as columns, samples as rows), cannot be reloaded")
            )
          ),
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_save"), "Save", class = "btn-primary", icon = icon("save"))
          ),
          easyClose = FALSE
        )
      )
    })
    
    observeEvent(input$confirm_save, {
      filename <- trimws(input$save_filename)
      format <- input$save_format
      
      # Validate filename
      if (filename == "" || is.null(filename)) {
        showNotification(
          "Please enter a filename",
          type = "error",
          duration = 3
        )
        return()
      }
      
      # Remove any existing extension if user added one
      filename <- gsub("\\.(rds|csv|xlsx)$", "", filename, ignore.case = TRUE)
      
      cat(sprintf("[SAVE] Attempting to save as %s format: %s\n", format, filename))
      
      # Call save function from processing_utils.R
      result <- save_processed_data(
        data = app_data$processed_data,
        filename = filename,
        format = format
      )
      
      if (result$success) {
        showNotification(
          result$message,
          type = "message",
          duration = 5
        )
        cat(sprintf("[SAVE] Success: %s\n", result$filepath))
      } else {
        showNotification(
          result$message,
          type = "error",
          duration = 5
        )
        cat(sprintf("[SAVE] Failed: %s\n", result$message))
      }
      
      removeModal()
    })
    
  })
}