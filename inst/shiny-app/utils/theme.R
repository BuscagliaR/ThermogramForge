# ==============================================================================
# Theme Utilities for ThermogramForge
# ==============================================================================
#
# FILE: theme.R
# VERSION: 1.0.0
# AUTHOR: Chris Reger
# LAST UPDATED: December 9, 2024
#
# ==============================================================================
# PURPOSE
# ==============================================================================
#
# Provides consistent visual styling across the ThermogramForge application.
# This file contains:
#
#   - Bootstrap 5 theme configuration via bslib
#   - Custom CSS overrides for application-specific styling
#   - Color palette and typography definitions
#
# Maintains visual consistency with the original Python/Dash implementation
# while leveraging modern Bootstrap 5 components.
#
# ==============================================================================
# FUNCTIONS
# ==============================================================================
#
#   1. thermogram_theme()  - Creates bslib Bootstrap 5 theme object
#   2. custom_css()        - Returns custom CSS string for additional styling
#
# ==============================================================================
# DESIGN NOTES
# ==============================================================================
#
# COLOR PALETTE:
#   - Primary:   #0d6efd (Bootstrap Blue)  - Main actions, links, branding
#   - Secondary: #6c757d (Gray)            - Secondary text, muted elements
#   - Success:   #198754 (Green)           - Completed/valid states
#   - Warning:   #ffc107 (Yellow)          - Caution/attention states
#   - Danger:    #dc3545 (Red)             - Errors, destructive actions
#   - Info:      #0dcaf0 (Cyan)            - Informational messages
#
# TYPOGRAPHY:
#   - Body:     Inter (Google Font) - Clean, readable sans-serif
#   - Headings: Inter (Google Font) - Consistent with body
#   - Code:     Roboto Mono        - Monospace for technical content
#
# SPACING:
#   - Base unit: 1rem (16px)
#   - Border radius: 0.375rem (6px) - Subtle rounded corners
#   - Card margins: 1rem bottom
#
# RESPONSIVE BREAKPOINTS:
#   - Mobile adjustments at 768px (md breakpoint)
#
# ==============================================================================
# DEPENDENCIES
# ==============================================================================
#
# Required packages:
#   - bslib: Bootstrap 5 theme creation and customization
#
# Note: Google Fonts (Inter, Roboto Mono) are loaded automatically by bslib
# when the app has internet connectivity.
#
# ==============================================================================


# ==============================================================================
# BOOTSTRAP THEME CONFIGURATION
# ==============================================================================

#' Create ThermogramForge Bootstrap 5 Theme
#'
#' @description
#' Creates a customized bslib theme object for ThermogramForge using Bootstrap 5.
#' Defines the color palette, typography, and base styling variables.
#'
#' @return A bslib bs_theme object to be passed to page_navbar() or similar.
#'
#' @details
#' The theme uses:
#'
#' **Colors:**
#' Standard Bootstrap 5 semantic colors with slight customizations for
#' consistency with the original Python implementation.
#'
#' **Typography:**
#' - Base font: Inter (Google Font) - Modern, highly readable sans-serif
#' - Code font: Roboto Mono - Clean monospace for any code/technical text
#' - Base size: 0.95rem (slightly smaller than default for data-dense UI)
#'
#' **Styling:**
#' - Border radius: 0.375rem for subtle rounded corners
#' - Spacer: 1rem base unit for consistent spacing
#'
#' @examples
#' \dontrun{
#' # In app.R UI definition:
#' ui <- page_navbar(
#'   title = "ThermogramForge",
#'   theme = thermogram_theme(),
#'   # ... nav panels
#' )
#' }
#'
#' @seealso [custom_css()] for additional CSS overrides
#'
#' @export
thermogram_theme <- function() {
  bslib::bs_theme(
    version = 5,
    
    # -------------------------------------------------------------------------
    # Semantic Color Palette
    # -------------------------------------------------------------------------
    # Standard Bootstrap 5 colors, matching the original Python app design
    
    primary   = "#0d6efd",   # Main brand/action color (Bootstrap Blue)
    secondary = "#6c757d",   # Muted text, disabled states (Gray)
    success   = "#198754",   # Completed states, valid data (Green)
    warning   = "#ffc107",   # Attention/caution states (Yellow)
    danger    = "#dc3545",   # Errors, destructive actions (Red)
    info      = "#0dcaf0",   # Informational messages (Cyan)
    
    # -------------------------------------------------------------------------
    # Background and Foreground
    # -------------------------------------------------------------------------
    
    bg = "#ffffff",          # White background for main content
    fg = "#212529",          # Dark gray text for readability
    
    # -------------------------------------------------------------------------
    # Typography
    # -------------------------------------------------------------------------
    # Using Google Fonts for modern, professional appearance
    
    base_font    = bslib::font_google("Inter"),       # Body text
    heading_font = bslib::font_google("Inter"),       # Headings (consistent)
    code_font    = bslib::font_google("Roboto Mono"), # Code/monospace
    
    # -------------------------------------------------------------------------
    # Sizing and Spacing
    # -------------------------------------------------------------------------
    
    "font-size-base" = "0.95rem",    # Slightly smaller for data-dense UI
    "border-radius"  = "0.375rem",   # Subtle rounded corners (6px)
    spacer           = "1rem"        # Base spacing unit (16px)
  )
}


# ==============================================================================
# CUSTOM CSS OVERRIDES
# ==============================================================================

#' Get Custom CSS for ThermogramForge
#'
#' @description
#' Returns a CSS string with custom styles that extend or override the
#' Bootstrap 5 theme. These styles handle application-specific UI elements
#' that require more granular control than theme variables provide.
#'
#' @return Character string containing valid CSS rules.
#'
#' @details
#' The custom CSS covers the following areas:
#'
#' **Layout:**
#' - Sticky navbar positioning
#' - Page background color
#'
#' **Components:**
#' - Card styling (headers, shadows, borders)
#' - Modal dialogs (rounded corners, shadows)
#' - Buttons (sizing, font weight)
#' - Form controls (focus states)
#' - DataTables integration
#' - Plotly chart styling
#' - Shiny notifications
#'
#' **Utilities:**
#' - Margin/spacing helpers (me-2, mt-3, etc.)
#' - Grid gap utilities
#'
#' **Responsive:**
#' - Mobile adjustments at 768px breakpoint
#'
#' @examples
#' \dontrun{
#' # In app.R header:
#' header = tagList(
#'   tags$head(
#'     tags$style(HTML(custom_css()))
#'   )
#' )
#' }
#'
#' @seealso [thermogram_theme()] for the base Bootstrap theme
#'
#' @export
custom_css <- function() {
  "
  /* =========================================================================
     GLOBAL STYLES
     ========================================================================= */
  
  /* Light gray page background for contrast with white cards */
  body {
    background-color: #f8f9fa;
  }
  
  /* =========================================================================
     NAVBAR STYLING
     ========================================================================= */
  
  /* Sticky positioning - stays at top during scroll */
  .bslib-page-navbar {
    position: sticky !important;
    top: 0;
    z-index: 1020;
  }
  
  /* Navbar appearance - white with subtle shadow */
  .navbar {
    background-color: #ffffff !important;
    border-bottom: 1px solid #dee2e6;
    box-shadow: 0 0.125rem 0.25rem rgba(0,0,0,0.075);
  }
  
  /* Brand/title styling */
  .navbar-brand {
    font-weight: 600;
    font-size: 1.25rem;
    color: #0d6efd !important;
  }
  
  /* =========================================================================
     CARD COMPONENTS
     ========================================================================= */
  
  /* Base card styling */
  .card {
    border: 1px solid #dee2e6;
    border-radius: 0.375rem;
    box-shadow: 0 0.125rem 0.25rem rgba(0,0,0,0.075);
    margin-bottom: 1rem;
  }
  
  /* Card header - light gray background */
  .card-header {
    background-color: #e9ecef;
    border-bottom: 1px solid #dee2e6;
    font-weight: 600;
    padding: 0.75rem 1rem;
  }
  
  /* Card body padding */
  .card-body {
    padding: 1rem;
  }
  
  /* =========================================================================
     BUTTON STYLING
     ========================================================================= */
  
  /* Base button styling */
  .btn {
    border-radius: 0.375rem;
    font-weight: 500;
  }
  
  /* Small button variant */
  .btn-sm {
    padding: 0.25rem 0.5rem;
    font-size: 0.875rem;
  }
  
  /* =========================================================================
     TABLE STYLING
     ========================================================================= */
  
  /* Remove default bottom margin */
  .table {
    margin-bottom: 0;
  }
  
  /* Subtle striped rows */
  .table-striped > tbody > tr:nth-of-type(odd) > * {
    background-color: rgba(0,0,0,0.02);
  }
  
  /* =========================================================================
     ALERT AND BADGE STYLING
     ========================================================================= */
  
  /* Consistent border radius on alerts */
  .alert {
    border-radius: 0.375rem;
  }
  
  /* Badge appearance */
  .badge {
    font-weight: 500;
    padding: 0.35em 0.65em;
  }
  
  /* =========================================================================
     MODAL DIALOG STYLING
     ========================================================================= */
  
  /* Modal content container */
  .modal-content {
    border-radius: 0.375rem;
    border: none;
    box-shadow: 0 0.5rem 1rem rgba(0,0,0,0.15);
  }
  
  /* Modal header - light background */
  .modal-header {
    border-bottom: 1px solid #dee2e6;
    background-color: #f8f9fa;
  }
  
  /* Modal footer - light background */
  .modal-footer {
    border-top: 1px solid #dee2e6;
    background-color: #f8f9fa;
  }
  
  /* =========================================================================
     FORM CONTROLS
     ========================================================================= */
  
  /* Input and select styling */
  .form-control, .form-select {
    border-radius: 0.375rem;
    border: 1px solid #ced4da;
  }
  
  /* Focus state - blue glow */
  .form-control:focus, .form-select:focus {
    border-color: #0d6efd;
    box-shadow: 0 0 0 0.25rem rgba(13,110,253,0.25);
  }
  
  /* =========================================================================
     DATATABLE INTEGRATION
     ========================================================================= */
  
  /* Base font size for DataTables */
  .dataTables_wrapper {
    font-size: 0.95rem;
  }
  
  /* Search input styling */
  .dataTables_filter input {
    border-radius: 0.375rem;
    border: 1px solid #ced4da;
    padding: 0.375rem 0.75rem;
    margin-left: 0.5rem;
  }
  
  /* =========================================================================
     PLOTLY CHARTS
     ========================================================================= */
  
  /* Rounded corners on plot containers */
  .plotly {
    border-radius: 0.375rem;
  }
  
  /* =========================================================================
     PROGRESS BARS
     ========================================================================= */
  
  .progress {
    border-radius: 0.375rem;
    height: 1.5rem;
  }
  
  /* =========================================================================
     SHINY NOTIFICATIONS
     ========================================================================= */
  
  .shiny-notification {
    border-radius: 0.375rem;
    border: none;
    box-shadow: 0 0.5rem 1rem rgba(0,0,0,0.15);
  }
  
  /* =========================================================================
     SPACING UTILITY CLASSES
     ========================================================================= */
  /* 
   * These ensure Bootstrap 5 spacing classes work correctly.
   * me = margin-end (right in LTR), ms = margin-start (left in LTR)
   * mt = margin-top, mb = margin-bottom
   */
  
  .me-2 { margin-right: 0.5rem !important; }
  .me-3 { margin-right: 1rem !important; }
  .ms-2 { margin-left: 0.5rem !important; }
  .mt-2 { margin-top: 0.5rem !important; }
  .mb-2 { margin-bottom: 0.5rem !important; }
  .mt-3 { margin-top: 1rem !important; }
  .mb-3 { margin-bottom: 1rem !important; }
  
  /* =========================================================================
     GRID UTILITIES
     ========================================================================= */
  
  /* Grid layout with gap for button stacks */
  .d-grid.gap-2 {
    display: grid !important;
    gap: 0.5rem !important;
  }
  
  /* =========================================================================
     RESPONSIVE ADJUSTMENTS
     ========================================================================= */
  
  /* Mobile breakpoint (768px and below) */
  @media (max-width: 768px) {
    /* Smaller navbar brand on mobile */
    .navbar-brand {
      font-size: 1rem;
    }
    
    /* Tighter card margins on mobile */
    .card {
      margin-bottom: 0.75rem;
    }
  }
  "
}