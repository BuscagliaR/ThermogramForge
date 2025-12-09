# ==============================================================================
# User Guide Module - ThermogramForge
# ==============================================================================
#
# PURPOSE:
#   Comprehensive help and documentation for ThermogramForge application.
#   Provides step-by-step guides, data format specifications, feature
#   explanations, and troubleshooting information.
#
# SECTIONS:
#   1. Quick Start Guide - 5-minute overview
#   2. Preparing Your Data - File formats and requirements
#   3. Data Overview Tab - Upload, process, save, load
#   4. Review Endpoints Tab - Manual adjustments and review workflow
#   5. Report Builder Tab - Metric selection and export
#   6. Saving & Loading - File format explanations
#   7. Troubleshooting - Common issues and solutions
#   8. About - Credits and references
#
# AUTHOR: Chris Reger
# LAST UPDATED: December 2025
# ==============================================================================


# ==============================================================================
# UI FUNCTION
# ==============================================================================

mod_user_guide_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container-fluid p-3",
      
      # ========================================================================
      # HEADER
      # ========================================================================
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          div(
            class = "card bg-primary text-white",
            div(
              class = "card-body py-4",
              div(
                class = "d-flex align-items-center",
                div(
                  class = "me-4",
                  icon("book-open", class = "fa-3x")
                ),
                div(
                  h2(class = "mb-1", "ThermogramForge User Guide"),
                  p(class = "mb-0 opacity-75", 
                    "Complete documentation for thermal liquid biopsy analysis")
                )
              )
            )
          )
        )
      ),
      
      # ========================================================================
      # MAIN LAYOUT: Sidebar + Content
      # ========================================================================
      div(
        class = "row",
        
        # ----------------------------------------------------------------------
        # LEFT SIDEBAR: Navigation
        # ----------------------------------------------------------------------
        div(
          class = "col-lg-3 col-md-4 mb-3",
          div(
            class = "card sticky-top",
            style = "top: 80px; z-index: 100;",
            div(
              class = "card-header bg-light",
              strong(icon("list"), " Contents")
            ),
            div(
              class = "card-body p-0",
              div(
                class = "list-group list-group-flush",
                tags$a(
                  href = "#quick-start", 
                  class = "list-group-item list-group-item-action",
                  icon("rocket", class = "me-2 text-primary"),
                  "Quick Start Guide"
                ),
                tags$a(
                  href = "#data-preparation", 
                  class = "list-group-item list-group-item-action",
                  icon("file-csv", class = "me-2 text-success"),
                  "Preparing Your Data"
                ),
                tags$a(
                  href = "#data-overview", 
                  class = "list-group-item list-group-item-action",
                  icon("table", class = "me-2 text-info"),
                  "Data Overview Tab"
                ),
                tags$a(
                  href = "#review-endpoints", 
                  class = "list-group-item list-group-item-action",
                  icon("chart-line", class = "me-2 text-warning"),
                  "Review Endpoints Tab"
                ),
                tags$a(
                  href = "#report-builder", 
                  class = "list-group-item list-group-item-action",
                  icon("file-export", class = "me-2 text-danger"),
                  "Report Builder Tab"
                ),
                tags$a(
                  href = "#saving-loading", 
                  class = "list-group-item list-group-item-action",
                  icon("save", class = "me-2 text-secondary"),
                  "Saving & Loading"
                ),
                tags$a(
                  href = "#troubleshooting", 
                  class = "list-group-item list-group-item-action",
                  icon("wrench", class = "me-2 text-muted"),
                  "Troubleshooting"
                ),
                tags$a(
                  href = "#about", 
                  class = "list-group-item list-group-item-action",
                  icon("info-circle", class = "me-2 text-primary"),
                  "About"
                )
              )
            )
          )
        ),
        
        # ----------------------------------------------------------------------
        # RIGHT CONTENT: Guide Sections
        # ----------------------------------------------------------------------
        div(
          class = "col-lg-9 col-md-8",
          
          # ====================================================================
          # SECTION 1: QUICK START GUIDE
          # ====================================================================
          div(
            id = "quick-start",
            class = "card mb-4",
            div(
              class = "card-header bg-primary text-white",
              h4(class = "mb-0", icon("rocket"), " Quick Start Guide")
            ),
            div(
              class = "card-body",
              
              p(class = "lead", 
                "Get up and running with ThermogramForge in 5 minutes."),
              
              # Workflow Overview
              h5(class = "mt-4 mb-3", icon("route"), " The ThermogramForge Workflow"),
              
              div(
                class = "row g-3 mb-4",
                
                # Step 1
                div(
                  class = "col-md-3",
                  div(
                    class = "card h-100 border-primary",
                    div(
                      class = "card-body text-center",
                      div(
                        class = "rounded-circle bg-primary text-white d-inline-flex align-items-center justify-content-center mb-2",
                        style = "width: 40px; height: 40px;",
                        strong("1")
                      ),
                      h6("Upload"),
                      p(class = "small text-muted mb-0", 
                        "Upload your CSV or Excel thermogram data")
                    )
                  )
                ),
                
                # Step 2
                div(
                  class = "col-md-3",
                  div(
                    class = "card h-100 border-success",
                    div(
                      class = "card-body text-center",
                      div(
                        class = "rounded-circle bg-success text-white d-inline-flex align-items-center justify-content-center mb-2",
                        style = "width: 40px; height: 40px;",
                        strong("2")
                      ),
                      h6("Process"),
                      p(class = "small text-muted mb-0", 
                        "Automatic baseline detection and signal analysis")
                    )
                  )
                ),
                
                # Step 3
                div(
                  class = "col-md-3",
                  div(
                    class = "card h-100 border-warning",
                    div(
                      class = "card-body text-center",
                      div(
                        class = "rounded-circle bg-warning text-dark d-inline-flex align-items-center justify-content-center mb-2",
                        style = "width: 40px; height: 40px;",
                        strong("3")
                      ),
                      h6("Review"),
                      p(class = "small text-muted mb-0", 
                        "Review and manually adjust endpoints if needed")
                    )
                  )
                ),
                
                # Step 4
                div(
                  class = "col-md-3",
                  div(
                    class = "card h-100 border-danger",
                    div(
                      class = "card-body text-center",
                      div(
                        class = "rounded-circle bg-danger text-white d-inline-flex align-items-center justify-content-center mb-2",
                        style = "width: 40px; height: 40px;",
                        strong("4")
                      ),
                      h6("Export"),
                      p(class = "small text-muted mb-0", 
                        "Generate metric reports for analysis")
                    )
                  )
                )
              ),
              
              # Quick Steps
              h5(class = "mt-4 mb-3", icon("list-check"), " Quick Steps"),
              
              tags$ol(
                class = "mb-4",
                tags$li(
                  class = "mb-2",
                  strong("Go to Data Overview tab"), " and click ",
                  tags$span(class = "badge bg-success", icon("upload"), " Upload New Raw Thermogram Data")
                ),
                tags$li(
                  class = "mb-2",
                  strong("Select your data file"), " (CSV or Excel) and review the preview"
                ),
                tags$li(
                  class = "mb-2",
                  strong("Click Process"), " on your uploaded dataset to run baseline detection"
                ),
                tags$li(
                  class = "mb-2",
                  strong("Go to Review Endpoints tab"), " to inspect results and make adjustments"
                ),
                tags$li(
                  class = "mb-2",
                  strong("Go to Report Builder tab"), " to select metrics and generate reports"
                ),
                tags$li(
                  class = "mb-2",
                  strong("Save your work"), " using Save to Disk on Data Overview"
                )
              ),
              
              # Tip Box
              div(
                class = "alert alert-info",
                icon("lightbulb"), " ",
                strong("Tip: "),
                "Save your processed data in RDS format to preserve all adjustments. ",
                "You can reload it later to continue your work."
              )
            )
          ),
          
          # ====================================================================
          # SECTION 2: PREPARING YOUR DATA
          # ====================================================================
          div(
            id = "data-preparation",
            class = "card mb-4",
            div(
              class = "card-header bg-success text-white",
              h4(class = "mb-0", icon("file-csv"), " Preparing Your Data")
            ),
            div(
              class = "card-body",
              
              p("ThermogramForge accepts thermogram data in CSV or Excel format. ",
                "Your data must follow one of three supported structures."),
              
              # File Requirements
              h5(class = "mt-4 mb-3", icon("file"), " File Requirements"),
              
              div(
                class = "row mb-4",
                div(
                  class = "col-md-6",
                  tags$ul(
                    tags$li(strong("Formats: "), "CSV (.csv), Excel (.xlsx, .xls)"),
                    tags$li(strong("Maximum file size: "), "150 MB"),
                    tags$li(strong("Maximum samples: "), "1,000 per file")
                  )
                ),
                div(
                  class = "col-md-6",
                  tags$ul(
                    tags$li(strong("Temperature units: "), "Degrees Celsius (", tags$sup("o"), "C)"),
                    tags$li(strong("Typical range: "), "45-90", tags$sup("o"), "C for plasma DSC"),
                    tags$li(strong("Encoding: "), "UTF-8 recommended")
                  )
                )
              ),
              
              # Data Formats
              h5(class = "mt-4 mb-3", icon("table-columns"), " Supported Data Formats"),
              
              # Format 1: Single Sample
              div(
                class = "card mb-3 border-primary",
                div(
                  class = "card-header bg-primary bg-opacity-10",
                  strong("Format 1: Single Sample")
                ),
                div(
                  class = "card-body",
                  p("For files containing one thermogram:"),
                  tags$table(
                    class = "table table-sm table-bordered mb-3",
                    style = "max-width: 300px;",
                    tags$thead(
                      tags$tr(
                        tags$th("Temperature"),
                        tags$th("dCp")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("45.0"), tags$td("0.0012")),
                      tags$tr(tags$td("45.5"), tags$td("0.0015")),
                      tags$tr(tags$td("46.0"), tags$td("0.0018")),
                      tags$tr(tags$td("..."), tags$td("..."))
                    )
                  ),
                  p(class = "small text-muted mb-0",
                    "Column names can be variations like 'Temp', 'T', 'temperature' or 'Cp', 'dcp', 'cp_excess'")
                )
              ),
              
              # Format 2: Multi-Sample Long
              div(
                class = "card mb-3 border-success",
                div(
                  class = "card-header bg-success bg-opacity-10",
                  strong("Format 2: Multi-Sample Long Format")
                ),
                div(
                  class = "card-body",
                  p("Multiple samples stacked vertically with a sample identifier:"),
                  tags$table(
                    class = "table table-sm table-bordered mb-3",
                    style = "max-width: 400px;",
                    tags$thead(
                      tags$tr(
                        tags$th("Sample_ID"),
                        tags$th("Temperature"),
                        tags$th("dCp")
                      )
                    ),
                    tags$tbody(
                      tags$tr(tags$td("Sample_001"), tags$td("45.0"), tags$td("0.0012")),
                      tags$tr(tags$td("Sample_001"), tags$td("45.5"), tags$td("0.0015")),
                      tags$tr(tags$td("Sample_002"), tags$td("45.0"), tags$td("0.0008")),
                      tags$tr(tags$td("Sample_002"), tags$td("45.5"), tags$td("0.0011")),
                      tags$tr(tags$td("..."), tags$td("..."), tags$td("..."))
                    )
                  ),
                  p(class = "small text-muted mb-0",
                    "Sample ID column can be named 'Sample_ID', 'SampleID', 'Sample', 'ID', etc.")
                )
              ),
              
              # Format 3: Multi-Sample Wide
              div(
                class = "card mb-3 border-warning",
                div(
                  class = "card-header bg-warning bg-opacity-10",
                  strong("Format 3: Multi-Sample Wide Format")
                ),
                div(
                  class = "card-body",
                  p("Paired temperature and dCp columns for each sample:"),
                  tags$table(
                    class = "table table-sm table-bordered mb-3",
                    tags$thead(
                      tags$tr(
                        tags$th("T1a"),
                        tags$th("1a"),
                        tags$th("T1b"),
                        tags$th("1b"),
                        tags$th("T2a"),
                        tags$th("2a"),
                        tags$th("...")
                      )
                    ),
                    tags$tbody(
                      tags$tr(
                        tags$td("45.0"), tags$td("0.0012"),
                        tags$td("45.0"), tags$td("0.0008"),
                        tags$td("45.0"), tags$td("0.0015"),
                        tags$td("...")
                      ),
                      tags$tr(
                        tags$td("45.5"), tags$td("0.0015"),
                        tags$td("45.5"), tags$td("0.0011"),
                        tags$td("45.5"), tags$td("0.0018"),
                        tags$td("...")
                      )
                    )
                  ),
                  p(class = "small text-muted mb-0",
                    "Temperature column starts with 'T' followed by sample ID (e.g., T1a). ",
                    "dCp column is just the sample ID (e.g., 1a).")
                )
              ),
              
              # Warning Box
              div(
                class = "alert alert-warning",
                icon("exclamation-triangle"), " ",
                strong("Important: "),
                "Ensure your data has no missing temperature values. ",
                "Missing dCp values are handled but may affect analysis quality."
              )
            )
          ),
          
          # ====================================================================
          # SECTION 3: DATA OVERVIEW TAB
          # ====================================================================
          div(
            id = "data-overview",
            class = "card mb-4",
            div(
              class = "card-header bg-info text-white",
              h4(class = "mb-0", icon("table"), " Data Overview Tab")
            ),
            div(
              class = "card-body",
              
              p("The Data Overview tab is your central hub for managing datasets. ",
                "Upload, process, save, and load your thermogram data here."),
              
              # Uploading Data
              h5(class = "mt-4 mb-3", icon("upload"), " Uploading Data"),
              
              tags$ol(
                tags$li(
                  class = "mb-2",
                  "Click the ", 
                  tags$span(class = "badge bg-success", icon("upload"), " Upload New Raw Thermogram Data"),
                  " button"
                ),
                tags$li(
                  class = "mb-2",
                  "Select your CSV or Excel file"
                ),
                tags$li(
                  class = "mb-2",
                  strong("For Excel files with multiple sheets: "),
                  "A dropdown will appear to select the specific sheet"
                ),
                tags$li(
                  class = "mb-2",
                  "Review the data preview to ensure it looks correct"
                ),
                tags$li(
                  class = "mb-2",
                  strong("Optional: "), "Adjust temperature range filter if needed ",
                  "(e.g., 45-90", tags$sup("o"), "C)"
                ),
                tags$li(
                  class = "mb-2",
                  "Click ", tags$span(class = "badge bg-primary", "Confirm Upload")
                )
              ),
              
              # Dataset Statuses
              h5(class = "mt-4 mb-3", icon("circle-info"), " Understanding Dataset Statuses"),
              
              div(
                class = "row mb-4",
                div(
                  class = "col-md-4",
                  div(
                    class = "card h-100 border-warning",
                    div(
                      class = "card-body",
                      h6(
                        tags$span(class = "badge bg-warning text-dark me-2", "Unprocessed"),
                        "Raw Data"
                      ),
                      p(class = "small mb-2", 
                        "Data has been uploaded but not yet processed."),
                      p(class = "small text-muted mb-0",
                        strong("Available actions: "), "Process, Delete")
                    )
                  )
                ),
                div(
                  class = "col-md-4",
                  div(
                    class = "card h-100 border-success",
                    div(
                      class = "card-body",
                      h6(
                        tags$span(class = "badge bg-success me-2", "Processed"),
                        "Analysis Ready"
                      ),
                      p(class = "small mb-2", 
                        "Baseline detection complete. Ready for review and reports."),
                      p(class = "small text-muted mb-0",
                        strong("Available actions: "), 
                        "Review Endpoints, Reports, Save, Download, Delete")
                    )
                  )
                ),
                div(
                  class = "col-md-4",
                  div(
                    class = "card h-100 border-primary",
                    div(
                      class = "card-body",
                      h6(
                        tags$span(class = "badge bg-primary me-2", "Loaded"),
                        "From Saved File"
                      ),
                      p(class = "small mb-2", 
                        "Previously saved dataset loaded from disk."),
                      p(class = "small text-muted mb-0",
                        strong("Available actions: "), 
                        "Review Endpoints, Reports, Download, Delete")
                    )
                  )
                )
              ),
              
              # Processing
              h5(class = "mt-4 mb-3", icon("microchip"), " Processing Data"),
              
              p("When you click ", strong("Process"), " on an unprocessed dataset:"),
              
              tags$ol(
                tags$li(
                  strong("Baseline Detection: "),
                  "The algorithm identifies stable pre- and post-transition regions"
                ),
                tags$li(
                  strong("Endpoint Selection: "),
                  "Lower and upper baseline endpoints are automatically determined"
                ),
                tags$li(
                  strong("Baseline Subtraction: "),
                  "A spline-fitted baseline is subtracted from raw data"
                ),
                tags$li(
                  strong("Signal Detection: "),
                  "ARIMA-based analysis determines if a thermal signature is present"
                )
              ),
              
              div(
                class = "alert alert-info",
                icon("lightbulb"), " ",
                strong("Tip: "),
                "Processing may take a few seconds per sample. "
              ),
              
              # Managing Datasets
              h5(class = "mt-4 mb-3", icon("folder-open"), " Managing Datasets"),
              
              tags$table(
                class = "table table-hover",
                tags$thead(
                  tags$tr(
                    tags$th("Action"),
                    tags$th("Description")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td(tags$span(class = "badge bg-primary", icon("chart-line"), " Review Endpoints")),
                    tags$td("Navigate to Review Endpoints tab with this dataset loaded")
                  ),
                  tags$tr(
                    tags$td(tags$span(class = "badge bg-danger", icon("file-export"), " Reports")),
                    tags$td("Navigate to Report Builder tab with this dataset selected")
                  ),
                  tags$tr(
                    tags$td(tags$span(class = "badge bg-success", icon("save"), " Save to Disk")),
                    tags$td("Save processed data to the server's file system (RDS/CSV/Excel)")
                  ),
                  tags$tr(
                    tags$td(tags$span(class = "badge bg-outline-success", icon("download"), " Download")),
                    tags$td("Download data directly to your computer")
                  ),
                  tags$tr(
                    tags$td(tags$span(class = "badge bg-outline-danger", icon("trash"), " Delete")),
                    tags$td("Remove dataset from the current session")
                  )
                )
              ),
              
              # Saved Datasets Section
              h5(class = "mt-4 mb-3", icon("database"), " Saved Datasets Section"),
              
              p("The bottom of the Data Overview tab shows datasets saved to disk. ",
                "From here you can:"),
              
              tags$ul(
                tags$li(strong("Load: "), "Load a saved dataset back into your session"),
                tags$li(strong("Download: "), "Download the saved file to your computer"),
                tags$li(strong("Delete: "), "Permanently remove the saved file from disk")
              ),
              
              div(
                class = "alert alert-warning",
                icon("exclamation-triangle"), " ",
                strong("Note: "),
                "Only RDS files can be fully reloaded with all adjustments intact. ",
                "CSV and Excel files contain only the baseline-subtracted data."
              )
            )
          ),
          
          # ====================================================================
          # SECTION 4: REVIEW ENDPOINTS TAB
          # ====================================================================
          div(
            id = "review-endpoints",
            class = "card mb-4",
            div(
              class = "card-header bg-warning text-dark",
              h4(class = "mb-0", icon("chart-line"), " Review Endpoints Tab")
            ),
            div(
              class = "card-body",
              
              p("The Review Endpoints tab allows you to inspect each sample's baseline ",
                "detection results and make manual adjustments if needed."),
              
              # Sample Grid
              h5(class = "mt-4 mb-3", icon("table-list"), " Sample Grid"),
              
              p("The sample grid shows all samples in your dataset with key information:"),
              
              tags$table(
                class = "table table-sm table-bordered",
                tags$thead(
                  tags$tr(
                    tags$th("Column"),
                    tags$th("Description")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td(strong("Sample")),
                    tags$td("Sample identifier from your data file")
                  ),
                  tags$tr(
                    tags$td(strong("Status")),
                    tags$td(
                      tags$span(class = "badge bg-success me-1", "Signal"),
                      " = thermal signature detected; ",
                      tags$span(class = "badge bg-secondary me-1", "No Signal"),
                      " = no clear signature"
                    )
                  ),
                  tags$tr(
                    tags$td(strong("Lower / Upper")),
                    tags$td("Baseline endpoint temperatures in ", tags$sup("o"), "C")
                  ),
                  tags$tr(
                    tags$td(strong("Reviewed")),
                    tags$td(icon("check-circle", class = "text-success"), 
                            " indicates you have reviewed this sample")
                  ),
                  tags$tr(
                    tags$td(strong("Exclude")),
                    tags$td(icon("ban", class = "text-danger"), 
                            " indicates sample is excluded from reports")
                  )
                )
              ),
              
              p(class = "small text-muted",
                "Click any row to select that sample and view its thermogram."),
              
              # Plot Views
              h5(class = "mt-4 mb-3", icon("chart-area"), " Plot Views"),
              
              div(
                class = "row mb-4",
                div(
                  class = "col-md-6",
                  div(
                    class = "card h-100",
                    div(
                      class = "card-header",
                      strong("Raw Thermogram View")
                    ),
                    div(
                      class = "card-body",
                      p(class = "small",
                        "Shows the original thermogram data with the fitted baseline curve. ",
                        "Useful for understanding how the baseline was determined."),
                      tags$ul(
                        class = "small mb-0",
                        tags$li("Gray dashed lines mark endpoint positions"),
                        tags$li("Red line shows the fitted baseline"),
                        tags$li("Click here to set endpoints on raw data scale")
                      )
                    )
                  )
                ),
                div(
                  class = "col-md-6",
                  div(
                    class = "card h-100",
                    div(
                      class = "card-header",
                      strong("Baseline Subtracted View")
                    ),
                    div(
                      class = "card-body",
                      p(class = "small",
                        "Shows the thermogram after baseline subtraction. ",
                        "This is the data used for metric calculations."),
                      tags$ul(
                        class = "small mb-0",
                        tags$li("Should show thermal transition peaks clearly"),
                        tags$li("Flat regions at endpoints indicate good baseline fit"),
                        tags$li("Default view for quality assessment")
                      )
                    )
                  )
                )
              ),
              
              # Manual Adjustment
              h5(class = "mt-4 mb-3", icon("mouse-pointer"), " Manual Endpoint Adjustment"),
              
              p("If the automatic baseline detection needs correction:"),
              
              tags$ol(
                tags$li(
                  class = "mb-2",
                  "Click ", 
                  tags$span(class = "badge bg-info", "Set Lower Endpoint"),
                  " or ",
                  tags$span(class = "badge bg-info", "Set Upper Endpoint")
                ),
                tags$li(
                  class = "mb-2",
                  "A notification will confirm you're in adjustment mode"
                ),
                tags$li(
                  class = "mb-2",
                  strong("Click directly on the plot"),
                  " at the desired temperature position"
                ),
                tags$li(
                  class = "mb-2",
                  "The baseline will automatically recalculate with the new endpoint"
                )
              ),
              
              div(
                class = "alert alert-info",
                icon("lightbulb"), " ",
                strong("Tip: "),
                "Look for flat, stable regions in the thermogram for optimal endpoint placement. ",
                "The pre-transition region is typically 45-55", tags$sup("o"), "C and ",
                "post-transition is typically 82-90", tags$sup("o"), "C for plasma samples."
              ),
              
              # Review Controls
              h5(class = "mt-4 mb-3", icon("clipboard-check"), " Review Controls"),
              
              tags$table(
                class = "table table-hover",
                tags$thead(
                  tags$tr(
                    tags$th("Control"),
                    tags$th("Purpose")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td(icon("check-square", class = "text-success"), strong(" Reviewed")),
                    tags$td("Mark a sample as reviewed. Helps track your progress through the dataset.")
                  ),
                  tags$tr(
                    tags$td(icon("ban", class = "text-danger"), strong(" Exclude")),
                    tags$td("Exclude a sample from report generation. Use for poor quality or failed samples.")
                  ),
                  tags$tr(
                    tags$td(icon("arrow-left"), " / ", icon("arrow-right"), strong(" Prev/Next")),
                    tags$td("Navigate to previous or next sample in the grid.")
                  ),
                  tags$tr(
                    tags$td(icon("undo"), " / ", icon("redo"), strong(" Undo/Redo")),
                    tags$td("Revert or restore changes to endpoints, reviewed status, or exclusions.")
                  )
                )
              ),
              
              # Undo/Redo
              h5(class = "mt-4 mb-3", icon("history"), " Undo/Redo System"),
              
              p("ThermogramForge maintains a complete history of your changes:"),
              
              tags$ul(
                tags$li("Every endpoint adjustment is tracked"),
                tags$li("Checkbox changes (Reviewed, Exclude) are tracked"),
                tags$li("Use ", strong("Undo"), " to revert the last change"),
                tags$li("Use ", strong("Redo"), " to restore an undone change"),
                tags$li("History is preserved until you leave the tab or switch datasets")
              ),
              
              div(
                class = "alert alert-success",
                icon("check-circle"), " ",
                strong("Best Practice: "),
                "Review all samples with 'No Signal' status carefully. ",
                "Consider excluding samples with poor quality or no discernible thermal transition."
              )
            )
          ),
          
          # ====================================================================
          # SECTION 5: REPORT BUILDER TAB
          # ====================================================================
          div(
            id = "report-builder",
            class = "card mb-4",
            div(
              class = "card-header bg-danger text-white",
              h4(class = "mb-0", icon("file-export"), " Report Builder Tab")
            ),
            div(
              class = "card-body",
              
              p("The Report Builder generates comprehensive metric reports using the ",
                tags$a(href = "https://github.com/BuscagliaR/tlbparam", target = "_blank", "tlbparam"),
                " package for thermogram analysis."),
              
              # Dataset Selection
              h5(class = "mt-4 mb-3", icon("database"), " Dataset Selection"),
              
              p("Use the dropdown at the top to select which processed dataset to analyze. ",
                "The display shows the filename and number of samples."),
              
              div(
                class = "alert alert-info",
                icon("info-circle"), " ",
                "Only samples ", strong("not marked as Excluded"), 
                " will be included in the report."
              ),
              
              # Metric Categories
              h5(class = "mt-4 mb-3", icon("chart-bar"), " Available Metrics"),
              
              p("ThermogramForge calculates 24 metrics across 6 categories:"),
              
              # Metric Accordion
              div(
                class = "accordion mb-4",
                id = "metricsAccordion",
                
                # Peak Heights
                div(
                  class = "accordion-item",
                  h2(
                    class = "accordion-header",
                    tags$button(
                      class = "accordion-button",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#peakHeights",
                      strong("Peak Heights"), 
                      tags$span(class = "badge bg-primary ms-2", "4 metrics")
                    )
                  ),
                  div(
                    id = "peakHeights",
                    class = "accordion-collapse collapse show",
                    `data-bs-parent` = "#metricsAccordion",
                    div(
                      class = "accordion-body",
                      tags$ul(
                        class = "mb-0",
                        tags$li(strong("Peak 1: "), "Height of peak in region 60-66", tags$sup("o"), "C"),
                        tags$li(strong("Peak 2: "), "Height of peak in region 67-73", tags$sup("o"), "C"),
                        tags$li(strong("Peak 3: "), "Height of peak in region 73-81", tags$sup("o"), "C"),
                        tags$li(strong("Peak F (Fibrinogen): "), "Height in fibrinogen region 47-60", tags$sup("o"), "C")
                      )
                    )
                  )
                ),
                
                # Peak Temperatures
                div(
                  class = "accordion-item",
                  h2(
                    class = "accordion-header",
                    tags$button(
                      class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#peakTemps",
                      strong("Peak Temperatures"), 
                      tags$span(class = "badge bg-primary ms-2", "4 metrics")
                    )
                  ),
                  div(
                    id = "peakTemps",
                    class = "accordion-collapse collapse",
                    `data-bs-parent` = "#metricsAccordion",
                    div(
                      class = "accordion-body",
                      tags$ul(
                        class = "mb-0",
                        tags$li(strong("T Peak 1-3: "), "Temperature at each peak maximum (", tags$sup("o"), "C)"),
                        tags$li(strong("T Peak F: "), "Temperature of fibrinogen peak (", tags$sup("o"), "C)")
                      )
                    )
                  )
                ),
                
                # Peak Ratios
                div(
                  class = "accordion-item",
                  h2(
                    class = "accordion-header",
                    tags$button(
                      class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#peakRatios",
                      strong("Peak Ratios"), 
                      tags$span(class = "badge bg-primary ms-2", "3 metrics")
                    )
                  ),
                  div(
                    id = "peakRatios",
                    class = "accordion-collapse collapse",
                    `data-bs-parent` = "#metricsAccordion",
                    div(
                      class = "accordion-body",
                      tags$ul(
                        class = "mb-0",
                        tags$li(strong("Peak 1/Peak 2: "), "Ratio of Peak 1 to Peak 2 heights"),
                        tags$li(strong("Peak 1/Peak 3: "), "Ratio of Peak 1 to Peak 3 heights"),
                        tags$li(strong("Peak 2/Peak 3: "), "Ratio of Peak 2 to Peak 3 heights")
                      )
                    )
                  )
                ),
                
                # Valley Metrics
                div(
                  class = "accordion-item",
                  h2(
                    class = "accordion-header",
                    tags$button(
                      class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#valleyMetrics",
                      strong("Valley Metrics"), 
                      tags$span(class = "badge bg-primary ms-2", "5 metrics")
                    )
                  ),
                  div(
                    id = "valleyMetrics",
                    class = "accordion-collapse collapse",
                    `data-bs-parent` = "#metricsAccordion",
                    div(
                      class = "accordion-body",
                      tags$ul(
                        class = "mb-0",
                        tags$li(strong("V1.2: "), "Valley (minimum) between Peak 1 and Peak 2"),
                        tags$li(strong("TV1.2: "), "Temperature of the valley (", tags$sup("o"), "C)"),
                        tags$li(strong("V1.2/Peak ratios: "), "Valley height relative to each peak")
                      )
                    )
                  )
                ),
                
                # Global Metrics
                div(
                  class = "accordion-item",
                  h2(
                    class = "accordion-header",
                    tags$button(
                      class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#globalMetrics",
                      strong("Global Metrics (Primary)"), 
                      tags$span(class = "badge bg-success ms-2", "5 metrics")
                    )
                  ),
                  div(
                    id = "globalMetrics",
                    class = "accordion-collapse collapse",
                    `data-bs-parent` = "#metricsAccordion",
                    div(
                      class = "accordion-body",
                      tags$ul(
                        class = "mb-0",
                        tags$li(strong("Max: "), "Maximum observed excess heat capacity"),
                        tags$li(strong("TMax: "), "Temperature at maximum height (", tags$sup("o"), "C)"),
                        tags$li(strong("TFM: "), "Temperature of first moment (center of mass)"),
                        tags$li(strong("Width: "), "Full width at half maximum (FWHM)"),
                        tags$li(strong("Area: "), "Total area under the thermogram signature")
                      )
                    )
                  )
                ),
                
                # Additional Metrics
                div(
                  class = "accordion-item",
                  h2(
                    class = "accordion-header",
                    tags$button(
                      class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#additionalMetrics",
                      strong("Additional Metrics"), 
                      tags$span(class = "badge bg-secondary ms-2", "3 metrics")
                    )
                  ),
                  div(
                    id = "additionalMetrics",
                    class = "accordion-collapse collapse",
                    `data-bs-parent` = "#metricsAccordion",
                    div(
                      class = "accordion-body",
                      tags$ul(
                        class = "mb-0",
                        tags$li(strong("Min: "), "Minimum observed excess heat capacity"),
                        tags$li(strong("TMin: "), "Temperature at minimum (", tags$sup("o"), "C)"),
                        tags$li(strong("Median: "), "Median excess heat capacity value")
                      )
                    )
                  )
                )
              ),
              
              # Generating Reports
              h5(class = "mt-4 mb-3", icon("cogs"), " Generating Reports"),
              
              tags$ol(
                tags$li(
                  class = "mb-2",
                  "Select desired metrics using checkboxes (or use All/None buttons)"
                ),
                tags$li(
                  class = "mb-2",
                  "Click ", 
                  tags$span(class = "badge bg-primary", icon("calculator"), " Generate Report")
                ),
                tags$li(
                  class = "mb-2",
                  "Preview the results in the table"
                ),
                tags$li(
                  class = "mb-2",
                  "Export using Download buttons or Save to reports/ folder"
                )
              ),
              
              # Export Options
              h5(class = "mt-4 mb-3", icon("download"), " Export Options"),
              
              div(
                class = "row",
                div(
                  class = "col-md-6",
                  div(
                    class = "card h-100",
                    div(
                      class = "card-header bg-light",
                      strong(icon("arrow-down"), " Direct Download")
                    ),
                    div(
                      class = "card-body",
                      p(class = "small", 
                        "Downloads directly to your computer via browser:"),
                      tags$ul(
                        class = "small mb-0",
                        tags$li(strong("Download CSV: "), "Simple tabular format"),
                        tags$li(strong("Download Excel: "), "Includes metadata sheet")
                      )
                    )
                  )
                ),
                div(
                  class = "col-md-6",
                  div(
                    class = "card h-100",
                    div(
                      class = "card-header bg-light",
                      strong(icon("save"), " Save to Reports Folder")
                    ),
                    div(
                      class = "card-body",
                      p(class = "small", 
                        "Saves to server's reports/ directory:"),
                      tags$ul(
                        class = "small mb-0",
                        tags$li("Files appear in Data Overview's saved section"),
                        tags$li("Good for batch processing workflows"),
                        tags$li("Enter custom report name before saving")
                      )
                    )
                  )
                )
              )
            )
          ),
          
          # ====================================================================
          # SECTION 6: SAVING & LOADING
          # ====================================================================
          div(
            id = "saving-loading",
            class = "card mb-4",
            div(
              class = "card-header bg-secondary text-white",
              h4(class = "mb-0", icon("save"), " Saving & Loading")
            ),
            div(
              class = "card-body",
              
              p("Understanding the different save formats helps you choose the right one for your needs."),
              
              # Format Comparison
              h5(class = "mt-4 mb-3", icon("file"), " File Format Comparison"),
              
              tags$table(
                class = "table table-bordered",
                tags$thead(
                  class = "table-light",
                  tags$tr(
                    tags$th("Format"),
                    tags$th("Contents"),
                    tags$th("Can Reload?"),
                    tags$th("Best For")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td(
                      tags$span(class = "badge bg-success", "RDS")
                    ),
                    tags$td(
                      "Complete data: raw thermograms, baseline-subtracted data, ",
                      "endpoints, adjustments, review status, exclusions"
                    ),
                    tags$td(
                      icon("check-circle", class = "text-success"),
                      " Yes - Full reload"
                    ),
                    tags$td("Saving work in progress, collaboration, archival")
                  ),
                  tags$tr(
                    tags$td(
                      tags$span(class = "badge bg-primary", "CSV")
                    ),
                    tags$td(
                      "Baseline-subtracted data only in wide format ",
                      "(temperature columns + sample columns)"
                    ),
                    tags$td(
                      icon("times-circle", class = "text-warning"),
                      " Report data only"
                    ),
                    tags$td("External analysis in R, Python, Excel, etc.")
                  ),
                  tags$tr(
                    tags$td(
                      tags$span(class = "badge bg-info", "Excel")
                    ),
                    tags$td(
                      "Baseline-subtracted data + metadata sheet with ",
                      "processing parameters"
                    ),
                    tags$td(
                      icon("times-circle", class = "text-warning"),
                      " Report data only"
                    ),
                    tags$td("Sharing with collaborators, documentation")
                  )
                )
              ),
              
              # File Locations
              h5(class = "mt-4 mb-3", icon("folder"), " File Locations"),
              
              tags$ul(
                tags$li(
                  strong("processed_data/"), " - Saved processed datasets (RDS, CSV, Excel)"
                ),
                tags$li(
                  strong("reports/"), " - Generated metric reports from Report Builder"
                )
              ),
              
              div(
                class = "alert alert-success",
                icon("lightbulb"), " ",
                strong("Recommendation: "),
                "Always save an RDS copy of your work before exporting to CSV or Excel. ",
                "This preserves all your manual adjustments and allows you to make changes later."
              )
            )
          ),
          
          # ====================================================================
          # SECTION 7: TROUBLESHOOTING
          # ====================================================================
          div(
            id = "troubleshooting",
            class = "card mb-4",
            div(
              class = "card-header bg-dark text-white",
              h4(class = "mb-0", icon("wrench"), " Troubleshooting")
            ),
            div(
              class = "card-body",
              
              h5(class = "mt-3 mb-3", icon("question-circle"), " Common Issues"),
              
              # FAQ Accordion
              div(
                class = "accordion",
                id = "faqAccordion",
                
                # Issue 1
                div(
                  class = "accordion-item",
                  h2(
                    class = "accordion-header",
                    tags$button(
                      class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#faq1",
                      "My file won't upload"
                    )
                  ),
                  div(
                    id = "faq1",
                    class = "accordion-collapse collapse",
                    `data-bs-parent` = "#faqAccordion",
                    div(
                      class = "accordion-body",
                      tags$ul(
                        tags$li("Check file size is under 150 MB"),
                        tags$li("Ensure file extension is .csv, .xlsx, or .xls"),
                        tags$li("Verify the file isn't corrupted by opening in Excel"),
                        tags$li("For Excel files, ensure the sheet contains data starting from row 1")
                      )
                    )
                  )
                ),
                
                # Issue 2
                div(
                  class = "accordion-item",
                  h2(
                    class = "accordion-header",
                    tags$button(
                      class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#faq2",
                      "Format not recognized"
                    )
                  ),
                  div(
                    id = "faq2",
                    class = "accordion-collapse collapse",
                    `data-bs-parent` = "#faqAccordion",
                    div(
                      class = "accordion-body",
                      tags$ul(
                        tags$li("Ensure you have Temperature and dCp columns (or equivalent names)"),
                        tags$li("For wide format, column names must follow T[id] and [id] pattern"),
                        tags$li("Remove any extra header rows above the column names"),
                        tags$li("Check for hidden characters in column names")
                      )
                    )
                  )
                ),
                
                # Issue 3
                div(
                  class = "accordion-item",
                  h2(
                    class = "accordion-header",
                    tags$button(
                      class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#faq3",
                      "Poor baseline detection results"
                    )
                  ),
                  div(
                    id = "faq3",
                    class = "accordion-collapse collapse",
                    `data-bs-parent` = "#faqAccordion",
                    div(
                      class = "accordion-body",
                      tags$ul(
                        tags$li("Check temperature range - ensure it covers full transition (45-90", 
                                tags$sup("o"), "C typical)"),
                        tags$li("Look for data quality issues (noise, artifacts, missing values)"),
                        tags$li("Use manual endpoint adjustment for problematic samples"),
                        tags$li("Consider excluding very noisy samples from analysis")
                      )
                    )
                  )
                ),
                
                # Issue 4
                div(
                  class = "accordion-item",
                  h2(
                    class = "accordion-header",
                    tags$button(
                      class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#faq4",
                      "Manual endpoint click not working"
                    )
                  ),
                  div(
                    id = "faq4",
                    class = "accordion-collapse collapse",
                    `data-bs-parent` = "#faqAccordion",
                    div(
                      class = "accordion-body",
                      tags$ul(
                        tags$li("Make sure you clicked 'Set Lower Endpoint' or 'Set Upper Endpoint' first"),
                        tags$li("Wait for the notification confirming adjustment mode"),
                        tags$li("Click directly on the plot area (not the axes or legend)"),
                        tags$li("If stuck, click 'Cancel' and try again")
                      )
                    )
                  )
                ),
                
                # Issue 5
                div(
                  class = "accordion-item",
                  h2(
                    class = "accordion-header",
                    tags$button(
                      class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#faq5",
                      "Report shows no samples"
                    )
                  ),
                  div(
                    id = "faq5",
                    class = "accordion-collapse collapse",
                    `data-bs-parent` = "#faqAccordion",
                    div(
                      class = "accordion-body",
                      tags$ul(
                        tags$li("Check if all samples are marked as Excluded"),
                        tags$li("Ensure a dataset is selected in the dropdown"),
                        tags$li("Verify the dataset has been processed (not just uploaded)"),
                        tags$li("Check that at least one metric is selected")
                      )
                    )
                  )
                ),
                
                # Issue 6
                div(
                  class = "accordion-item",
                  h2(
                    class = "accordion-header",
                    tags$button(
                      class = "accordion-button collapsed",
                      type = "button",
                      `data-bs-toggle` = "collapse",
                      `data-bs-target` = "#faq6",
                      "Can't reload my saved CSV/Excel file"
                    )
                  ),
                  div(
                    id = "faq6",
                    class = "accordion-collapse collapse",
                    `data-bs-parent` = "#faqAccordion",
                    div(
                      class = "accordion-body",
                      p("CSV and Excel exports contain only the baseline-subtracted data, not the full processing state."),
                      tags$ul(
                        tags$li("These formats are for external analysis, not for reloading into ThermogramForge"),
                        tags$li("Always save an RDS file if you want to continue working later"),
                        tags$li("RDS files preserve all endpoints, adjustments, and exclusions")
                      )
                    )
                  )
                )
              )
            )
          ),
          
          # ====================================================================
          # SECTION 8: ABOUT
          # ====================================================================
          div(
            id = "about",
            class = "card mb-4",
            div(
              class = "card-header bg-primary text-white",
              h4(class = "mb-0", icon("info-circle"), " About ThermogramForge")
            ),
            div(
              class = "card-body",
              
              # Description
              h5(class = "mt-3 mb-3", "Application Overview"),
              
              p("ThermogramForge is an R Shiny application for analyzing differential scanning ",
                "calorimetry (DSC) data from thermal liquid biopsy (TLB) experiments. ",
                "It provides automated baseline detection, interactive review capabilities, ",
                "and comprehensive metric calculation."),
              
              # Key Packages
              h5(class = "mt-4 mb-3", icon("cube"), " Core Dependencies"),
              
              div(
                class = "row",
                div(
                  class = "col-md-6",
                  tags$ul(
                    tags$li(
                      tags$a(
                        href = "https://github.com/BuscagliaR/ThermogramBaseline", 
                        target = "_blank",
                        strong("ThermogramBaseline")
                      ),
                      " - Automated baseline detection algorithm"
                    ),
                    tags$li(
                      tags$a(
                        href = "https://github.com/BuscagliaR/tlbparam", 
                        target = "_blank",
                        strong("tlbparam")
                      ),
                      " - Thermogram metric calculations"
                    )
                  )
                ),
                div(
                  class = "col-md-6",
                  tags$ul(
                    tags$li(strong("shiny"), " / ", strong("bslib"), " - Web application framework"),
                    tags$li(strong("plotly"), " - Interactive visualizations"),
                    tags$li(strong("DT"), " - Interactive data tables")
                  )
                )
              ),
              
              # Credits
              h5(class = "mt-4 mb-3", icon("users"), " Credits"),
              
              p("Developed by Chris Reger at Northern Arizona University."),
              
              p("The baseline detection algorithm and validation are described in:"),
              
              div(
                class = "alert alert-light border",
                p(class = "mb-0 small",
                  em("Reger et al. (2025). Automated baseline-correction and signal detection ",
                     "algorithms with web-based implementation for Thermal Liquid Biopsy data analysis."),
                  " Manuscript in preparation."
                )
              ),
              
              # Version
              h5(class = "mt-4 mb-3", icon("code-branch"), " Version"),
              
              p("ThermogramForge v1.0.0 (December 2025)")
            )
          ),
          
          # Back to Top Button
          div(
            class = "text-center mb-4",
            tags$a(
              href = "#quick-start",
              class = "btn btn-outline-primary",
              icon("arrow-up"), " Back to Top"
            )
          )
          
        )
      )
    ),
    
    # Smooth scrolling CSS
    tags$style(HTML("
      html {
        scroll-behavior: smooth;
      }
      
      .list-group-item-action:hover {
        background-color: #f8f9fa;
      }
      
      .accordion-button:not(.collapsed) {
        background-color: #e7f1ff;
        color: #0c63e4;
      }
      
      .card-header {
        font-weight: 500;
      }
      
      /* Offset for fixed navbar when jumping to anchors */
      [id] {
        scroll-margin-top: 80px;
      }
    "))
  )
}


# ==============================================================================
# SERVER FUNCTION
# ==============================================================================

mod_user_guide_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    
    # This module is primarily static content
    # No server logic required currently
    
    # Log when user visits the guide
    cat("[USER_GUIDE] User Guide tab accessed\n")
    
  })
}