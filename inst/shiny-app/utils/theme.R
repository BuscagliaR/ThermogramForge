# Theme utilities for ThermogramForge
# Maintains visual consistency with Python application

#' Create ThermogramForge bslib theme
#'
#' @return A bslib theme object
thermogram_theme <- function() {
  bslib::bs_theme(
    version = 5,
    
    # Primary color scheme matching Python app
    primary = "#0d6efd",
    secondary = "#6c757d", 
    success = "#198754",
    warning = "#ffc107",
    danger = "#dc3545",
    info = "#0dcaf0",
    
    # Background colors
    bg = "#ffffff",
    fg = "#212529",
    
    # Typography
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter"),
    code_font = bslib::font_google("Roboto Mono"),
    
    # Font sizes
    "font-size-base" = "0.95rem",
    
    # Border radius
    "border-radius" = "0.375rem",
    
    # Spacing
    spacer = "1rem"
  )
}

#' Get custom CSS for ThermogramForge
#'
#' @return Character string of CSS
custom_css <- function() {
  "
  /* Global styles */
  body {
    background-color: #f8f9fa;
  }
  
  /* Navbar styling */
  .navbar {
    background-color: #ffffff !important;
    border-bottom: 1px solid #dee2e6;
    box-shadow: 0 0.125rem 0.25rem rgba(0,0,0,0.075);
  }
  
  .navbar-brand {
    font-weight: 600;
    font-size: 1.25rem;
    color: #0d6efd !important;
  }
  
  /* Card styling */
  .card {
    border: 1px solid #dee2e6;
    border-radius: 0.375rem;
    box-shadow: 0 0.125rem 0.25rem rgba(0,0,0,0.075);
    margin-bottom: 1rem;
  }
  
  .card-header {
    background-color: #e9ecef;
    border-bottom: 1px solid #dee2e6;
    font-weight: 600;
    padding: 0.75rem 1rem;
  }
  
  .card-body {
    padding: 1rem;
  }
  
  /* Button styling */
  .btn {
    border-radius: 0.375rem;
    font-weight: 500;
    padding: 0.5rem 1rem;
  }
  
  .btn-primary {
    background-color: #0d6efd;
    border-color: #0d6efd;
  }
  
  .btn-primary:hover {
    background-color: #0b5ed7;
    border-color: #0a58ca;
  }
  
  /* DataTable styling */
  .dataTables_wrapper {
    padding: 0;
  }
  
  table.dataTable tbody tr.selected {
    background-color: #cfe2ff !important;
  }
  
  table.dataTable tbody tr:hover {
    background-color: #e3f2fd !important;
  }
  
  /* Plot container */
  .plotly {
    border: 1px solid #dee2e6;
    border-radius: 0.375rem;
    background-color: white;
  }
  
  .js-plotly-plot .plotly .main-svg {
    border-radius: 0.375rem;
  }
  
  /* Status badges */
  .badge {
    padding: 0.35em 0.65em;
    font-size: 0.875em;
    font-weight: 600;
    border-radius: 0.375rem;
  }
  
  /* Spacing utilities */
  .mt-3 { margin-top: 1rem !important; }
  .mb-3 { margin-bottom: 1rem !important; }
  .ms-2 { margin-left: 0.5rem !important; }
  .p-3 { padding: 1rem !important; }
  .d-flex { display: flex !important; }
  .gap-2 > * + * { margin-left: 0.5rem; }
  .justify-content-between { justify-content: space-between !important; }
  .align-items-center { align-items: center !important; }
  .text-center { text-align: center; }
  
  /* Badge styling */
  .badge {
    display: inline-block;
    padding: 0.35em 0.65em;
    font-size: 0.875em;
    font-weight: 600;
    line-height: 1;
    color: #fff;
    text-align: center;
    white-space: nowrap;
    vertical-align: baseline;
    border-radius: 0.375rem;
  }
  
  .bg-warning {
    background-color: #ffc107 !important;
    color: #000 !important;
  }
  
  /* DataTable row styling - very specific selectors to override defaults */
  table.dataTable.hover tbody tr.selected,
  table.dataTable.display tbody tr.selected,
  table.dataTable tbody tr.selected,
  table.dataTable tbody tr.selected > td,
  table.dataTable.row-border tbody tr.selected > td,
  table.dataTable.cell-border tbody tr.selected > td {
    background-color: rgba(13, 110, 253, 0.08) !important;
    box-shadow: inset 0 0 0 9999px rgba(13, 110, 253, 0.08) !important;
  }
  
  table.dataTable tbody tr.selected:hover,
  table.dataTable tbody tr.selected:hover > td {
    background-color: rgba(13, 110, 253, 0.12) !important;
    box-shadow: inset 0 0 0 9999px rgba(13, 110, 253, 0.12) !important;
  }
  
  table.dataTable.row-border tbody tr:hover,
  table.dataTable.row-border tbody tr:hover > td {
    background-color: rgba(13, 110, 253, 0.05) !important;
  }
  
  table.dataTable.compact tbody td {
    padding: 0.5rem;
  }
  
  /* Custom alert styling */
  .alert {
    border-radius: 0.375rem;
    border: 1px solid transparent;
  }
  
  /* Loading spinner */
  .spinner-border {
    display: inline-block;
    width: 2rem;
    height: 2rem;
    vertical-align: text-bottom;
    border: 0.25em solid currentColor;
    border-right-color: transparent;
    border-radius: 50%;
    animation: spinner-border 0.75s linear infinite;
  }
  
  @keyframes spinner-border {
    to { transform: rotate(360deg); }
  }
  
  .visually-hidden {
    position: absolute;
    width: 1px;
    height: 1px;
    padding: 0;
    margin: -1px;
    overflow: hidden;
    clip: rect(0,0,0,0);
    white-space: nowrap;
    border: 0;
  }
  
  .shiny-busy-panel {
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    z-index: 9999;
  }
  "
}