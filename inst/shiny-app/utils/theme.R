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
  
  /* Sticky Navbar - Pin to top without covering content */
  .bslib-page-navbar {
    position: sticky !important;
    top: 0;
    z-index: 1020;
  }
  
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
  }
  
  .btn-sm {
    padding: 0.25rem 0.5rem;
    font-size: 0.875rem;
  }
  
  /* Table styling */
  .table {
    margin-bottom: 0;
  }
  
  .table-striped > tbody > tr:nth-of-type(odd) > * {
    background-color: rgba(0,0,0,0.02);
  }
  
  /* Alert styling */
  .alert {
    border-radius: 0.375rem;
  }
  
  /* Badge styling */
  .badge {
    font-weight: 500;
    padding: 0.35em 0.65em;
  }
  
  /* Modal styling */
  .modal-content {
    border-radius: 0.375rem;
    border: none;
    box-shadow: 0 0.5rem 1rem rgba(0,0,0,0.15);
  }
  
  .modal-header {
    border-bottom: 1px solid #dee2e6;
    background-color: #f8f9fa;
  }
  
  .modal-footer {
    border-top: 1px solid #dee2e6;
    background-color: #f8f9fa;
  }
  
  /* Form styling */
  .form-control, .form-select {
    border-radius: 0.375rem;
    border: 1px solid #ced4da;
  }
  
  .form-control:focus, .form-select:focus {
    border-color: #0d6efd;
    box-shadow: 0 0 0 0.25rem rgba(13,110,253,0.25);
  }
  
  /* DataTable styling */
  .dataTables_wrapper {
    font-size: 0.95rem;
  }
  
  .dataTables_filter input {
    border-radius: 0.375rem;
    border: 1px solid #ced4da;
    padding: 0.375rem 0.75rem;
    margin-left: 0.5rem;
  }
  
  /* Plotly plot styling */
  .plotly {
    border-radius: 0.375rem;
  }
  
  /* Progress bar styling */
  .progress {
    border-radius: 0.375rem;
    height: 1.5rem;
  }
  
  /* Notification styling */
  .shiny-notification {
    border-radius: 0.375rem;
    border: none;
    box-shadow: 0 0.5rem 1rem rgba(0,0,0,0.15);
  }
  
  /* Custom spacing helpers */
  .me-2 {
    margin-right: 0.5rem !important;
  }
  
  .me-3 {
    margin-right: 1rem !important;
  }
  
  .ms-2 {
    margin-left: 0.5rem !important;
  }
  
  .mt-2 {
    margin-top: 0.5rem !important;
  }
  
  .mb-2 {
    margin-bottom: 0.5rem !important;
  }
  
  .mt-3 {
    margin-top: 1rem !important;
  }
  
  .mb-3 {
    margin-bottom: 1rem !important;
  }
  
  /* Grid gap utility for button stacks */
  .d-grid.gap-2 {
    display: grid !important;
    gap: 0.5rem !important;
  }
  
  /* Responsive adjustments */
  @media (max-width: 768px) {
    .navbar-brand {
      font-size: 1rem;
    }
    
    .card {
      margin-bottom: 0.75rem;
    }
  }
  "
}