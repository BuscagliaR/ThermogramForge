# ==============================================================================
# Metrics Calculation Utilities for ThermogramForge
# ==============================================================================
#
# FILE: metrics_utils.R
# VERSION: 1.0.0
# AUTHOR: Chris Reger
# LAST UPDATED: December 9, 2024
#
# ==============================================================================
# PURPOSE
# ==============================================================================
#
# Integration with the tlbparam package for thermogram metric calculations.
# Handles the conversion of processed data to tlbparam format and calculates
# user-selected metrics.
#
# ==============================================================================
# AVAILABLE METRICS
# ==============================================================================
#
# The tlbparam package provides 24 metrics across 6 categories:
#
# PEAK METRICS:
#   - Peak 1-4: Peak heights at specific temperature ranges
#   - Peak Ratios: Relative peak heights
#
# AREA METRICS:
#   - AUC: Total area under curve
#   - Partial AUCs: Area in specific temperature ranges
#
# TEMPERATURE METRICS:
#   - Tm: Melting temperature (peak maximum)
#   - TFM: First moment temperature
#   - TCM: Center of mass temperature
#   - Width: Peak width at half maximum
#
# SHAPE METRICS:
#   - Asymmetry: Peak shape asymmetry
#   - Kurtosis: Peak sharpness
#
# DERIVATIVE METRICS:
#   - First/Second derivatives at key points
#
# CURVE FIT METRICS:
#   - Gaussian fit parameters
#
# ==============================================================================
# FUNCTIONS
# ==============================================================================
#
# Exported:
#   - calculate_tlbparam_metrics()  Calculate metrics using tlbparam package
#
# ==============================================================================
# DEPENDENCIES
# ==============================================================================
#
# Required packages:
#   - tlbparam: Thermogram metric calculations (from BuscagliaR)
#       Functions used: clean_thermograms()
#
# Required project files:
#   - format_utils.R: convert_to_tlbparam_format() for data preparation
#
# ==============================================================================


# ==============================================================================
# TLBPARAM METRIC CALCULATION
# ==============================================================================
#
# Calculates thermogram metrics using the tlbparam package.
# Maps UI-friendly metric names to tlbparam internal names.
#
# ==============================================================================

#'         \item Additional fields like `lower_endpoint`, `upper_endpoint`, etc.
#'       }
#'     \item `summary`: Summary statistics (optional)
#'   }
#'
#' @param selected_metrics
#'   Character vector of metric names to calculate (UI names, lowercase).
#'   Example: `c("peak_1", "tfm", "area")`.
#'   Invalid metric names are silently ignored with a warning message.
#'
#' @return
#'   Data frame with structure:
#'   \itemize{
#'     \item Row 1+: One row per sample
#'     \item Column 1: `SampleCode` (sample identifiers)
#'     \item Columns 2+: Requested metrics (in order specified)
#'   }
#'   Returns `NULL` if:
#'   - Input validation fails
#'   - Data conversion fails
#'   - `tlbparam::clean_thermograms()` fails
#'   - No valid metrics remain after mapping
#'
#' @seealso
#'   \code{\link{convert_to_tlbparam_format}()} for data format conversion
#'   \code{tlbparam::clean_thermograms()} from the tlbparam package
#'
#' @examples
#' \dontrun{
#'   # Generate metrics for multiple samples
#'   metrics <- calculate_tlbparam_metrics(
#'     processed_data = my_processed_data,
#'     selected_metrics = c("peak_1", "peak_2", "tfm", "area")
#'   )
#'   # Result: data frame with columns: SampleCode, peak_1, peak_2, tfm, area
#'   # Each row is one sample, with calculated metric values
#'   head(metrics)
#'
#'   # Common use case: Report generation
#'   report_metrics <- calculate_tlbparam_metrics(
#'     processed_data = app_data$processed_data,
#'     selected_metrics = c("tpeak_1", "tpeak_2", "area", "tfm", "width")
#'   )
#'   # Export to CSV for downstream analysis
#'   write.csv(report_metrics, "thermogram_report.csv", row.names = FALSE)
#' }
#'
#' @keywords internal
#' @export
calculate_tlbparam_metrics <- function(processed_data, selected_metrics) {
  
  cat("\n[METRICS] ========================================\n")
  cat("[METRICS] Starting tlbparam metric calculation\n")
  
  if (!is.list(processed_data) || !("samples" %in% names(processed_data))) {
    cat("[METRICS] ❌ ERROR: Invalid processed_data\n")
    return(NULL)
  }
  
  if (length(processed_data$samples) == 0) {
    cat("[METRICS] ❌ ERROR: No samples\n")
    return(NULL)
  }
  
  cat(sprintf("[METRICS] Input validation: [OK] (%d samples, %d metrics)\n",
              length(processed_data$samples), length(selected_metrics)))
  
  # MAP UI metric names to tlbparam names
  metric_mapping <- list(
    "peak_1" = "Peak 1", "peak_2" = "Peak 2", "peak_3" = "Peak 3", "peak_f" = "Peak F",
    "tpeak_1" = "TPeak 1", "tpeak_2" = "TPeak 2", "tpeak_3" = "TPeak 3", "tpeak_f" = "TPeak F",
    "area" = "Area", "tfm" = "TFM", "width" = "Width",
    "max_dcp" = "Max", "tmax" = "TMax", "min_dcp" = "Min", "tmin" = "TMin", "median_dcp" = "Median",
    "v12" = "V1.2", "tv12" = "TV1.2",
    "peak1_peak2_ratio" = "Peak 1 / Peak 2", "peak1_peak3_ratio" = "Peak 1 / Peak 3",
    "peak2_peak3_ratio" = "Peak 2 / Peak 3",
    "v12_peak1_ratio" = "V1.2 / Peak 1", "v12_peak2_ratio" = "V1.2 / Peak 2", "v12_peak3_ratio" = "V1.2 / Peak 3"
  )
  
  # Convert UI names to tlbparam names
  tlbparam_metrics <- character()
  for (metric in tolower(selected_metrics)) {
    if (metric %in% names(metric_mapping)) {
      tlbparam_metrics <- c(tlbparam_metrics, metric_mapping[[metric]])
      cat(sprintf("[METRICS] [OK] Mapped '%s' → '%s'\n", metric, metric_mapping[[metric]]))
    }
  }
  
  if (length(tlbparam_metrics) == 0) {
    cat("[METRICS] ❌ ERROR: No valid metrics\n")
    return(NULL)
  }
  
  # Convert to tlbparam format
  cat("[METRICS] Converting to tlbparam wide format...\n")
  tlbparam_data <- tryCatch(convert_to_tlbparam_format(processed_data), error = function(e) NULL)
  
  if (is.null(tlbparam_data)) {
    cat("[METRICS] ❌ ERROR: Conversion failed\n")
    return(NULL)
  }
  
  cat(sprintf("[METRICS] [OK] Converted: %d samples × %d columns\n",
              nrow(tlbparam_data), ncol(tlbparam_data)))
  
  # Call tlbparam::clean_thermograms with mapped metric names
  cat("[METRICS] Calling tlbparam::clean_thermograms()...\n")
  
  cleaned_data <- tryCatch({
    tlbparam::clean_thermograms(
      df = tlbparam_data,
      type = "Plasma",
      column = "SampleCode",
      low_temp = "T45",
      high_temp = "T90",
      temp_range = seq(45, 90, by = 0.1),
      summary = tlbparam_metrics,
      lag = FALSE
    )
  }, error = function(e) {
    cat(sprintf("[METRICS] ❌ clean_thermograms error: %s\n", e$message))
    return(NULL)
  })
  
  if (is.null(cleaned_data)) {
    cat("[METRICS] ❌ ERROR: clean_thermograms failed\n")
    return(NULL)
  }
  
  # Extract ONLY the requested metric columns (not temperature columns)
  cat("[METRICS] Extracting requested metrics...\n")
  
  cols_to_extract <- c("SampleCode")
  for (tlbparam_name in tlbparam_metrics) {
    if (tlbparam_name %in% names(cleaned_data)) {
      cols_to_extract <- c(cols_to_extract, tlbparam_name)
      cat(sprintf("[METRICS] [OK] Including '%s'\n", tlbparam_name))
    }
  }
  
  result <- cleaned_data[, cols_to_extract, drop = FALSE]
  
  # Rename back to UI names
  new_names <- c("SampleCode")
  for (i in 2:length(cols_to_extract)) {
    tlbparam_name <- cols_to_extract[i]
    ui_name <- names(metric_mapping)[which(unlist(metric_mapping) == tlbparam_name)]
    new_names <- c(new_names, ui_name)
  }
  names(result) <- new_names
  
  cat(sprintf("[METRICS] [OK] SUCCESS: %d samples × %d columns\n",
              nrow(result), ncol(result)))
  cat("[METRICS] ========================================\n\n")
  
  return(result)
}