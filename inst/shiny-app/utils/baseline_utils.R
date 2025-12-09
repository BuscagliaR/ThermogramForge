# ==============================================================================
# Baseline Detection Utilities for ThermogramForge
# ==============================================================================
#
# FILE: baseline_utils.R
# VERSION: 1.0.0
# AUTHOR: Chris Reger
# LAST UPDATED: December 9, 2024
#
# ==============================================================================
# PURPOSE
# ==============================================================================
#
# Core processing functions for baseline detection and signal quality
# assessment. This file contains the main processing pipeline that:
#
#   - Applies the ThermogramBaseline algorithm to detect baseline endpoints
#   - Performs baseline subtraction and interpolation
#   - Assesses signal quality for each sample
#
# ==============================================================================
# ARCHITECTURE
# ==============================================================================
#
# Processing Flow:
#
#   process_thermogram_data()     Main entry point
#       |
#       +-- extract_sample_data()     Extract individual sample from dataset
#       |
#       +-- apply_baseline_detection() Run ThermogramBaseline algorithm
#       |       |
#       |       +-- ThermogramBaseline::var_rollmean()
#       |       +-- ThermogramBaseline::baseline_subtract()
#       |       +-- ThermogramBaseline::interp_dsc()
#       |
#       +-- apply_signal_detection()  Check for thermal signature
#               |
#               +-- ThermogramBaseline::signal_detect()
#
# ==============================================================================
# FUNCTIONS
# ==============================================================================
#
# Main Processing:
#   - process_thermogram_data()    Run full baseline detection pipeline
#
# Internal Helpers (not exported):
#   - extract_sample_data()        Extract single sample from multi-sample data
#   - apply_baseline_detection()   Run ThermogramBaseline on one sample
#   - apply_signal_detection()     Check if sample has detectable signal
#
# ==============================================================================
# DEPENDENCIES
# ==============================================================================
#
# Required packages:
#   - ThermogramBaseline: Baseline detection algorithm (from BuscagliaR)
#       Functions used: var_rollmean(), baseline_subtract(), interp_dsc(),
#                       signal_detect()
#   - forecast: Time series utilities (dependency of ThermogramBaseline)
#
# Required project files:
#   - format_utils.R: Data format information structures
#
# ==============================================================================


# ==============================================================================
# BASELINE DETECTION AND PROCESSING
# ==============================================================================
#
# Core processing functions that apply the ThermogramBaseline algorithm to
# detect baseline endpoints and subtract baseline from thermogram data.
#
# ==============================================================================


#' Process Thermogram Data with Baseline Detection
#'
#' Applies automatic baseline detection and signal quality assessment to
#' thermogram data using the ThermogramBaseline package.
#'
#' @param data Data frame containing thermogram data
#' @param format_info List with format metadata including:
#'   \itemize{
#'     \item format_type: "single_sample", "multi_sample_long", or "multi_sample_wide"
#'     \item sample_ids: Vector of sample identifiers
#'   }
#' @param temp_params List with processing parameters:
#'   \itemize{
#'     \item temp_min: Minimum temperature (C) for filtering
#'     \item temp_max: Maximum temperature (C) for filtering
#'     \item window_size: Window size for rolling variance (default 90)
#'     \item exclusion_lower: Lower exclusion temperature (default 60)
#'     \item exclusion_upper: Upper exclusion temperature (default 80)
#'     \item grid_resolution: Temperature grid step size (default 0.1)
#'     \item point_selection: "innermost", "outmost", or "middle" (default "innermost")
#'   }
#'
#' @return List containing:
#'   \itemize{
#'     \item samples: Named list of processed samples
#'     \item n_samples: Total number of samples processed
#'     \item n_signal: Number of samples with signal
#'     \item n_no_signal: Number of samples without signal
#'     \item processing_params: Copy of input parameters
#'     \item processing_time: Timestamp of processing
#'   }
#'   Returns NULL if processing fails completely.
#'
#' @export
process_thermogram_data <- function(data, format_info, temp_params) {
  
  # Validate inputs
  if (is.null(data) || nrow(data) == 0) {
    stop("Input data is NULL or empty")
  }
  
  if (is.null(format_info$sample_ids) || length(format_info$sample_ids) == 0) {
    stop("No sample IDs provided in format_info")
  }
  
  # Check ThermogramBaseline package
  if (!requireNamespace("ThermogramBaseline", quietly = TRUE)) {
    stop("ThermogramBaseline package is not installed")
  }
  
  # Set default parameters if not provided
  window_size <- temp_params$window_size %||% 90
  exclusion_lower <- temp_params$exclusion_lower %||% 60
  exclusion_upper <- temp_params$exclusion_upper %||% 80
  grid_resolution <- temp_params$grid_resolution %||% 0.1
  point_selection <- temp_params$point_selection %||% "innermost"
  
  cat(sprintf("\n[PROCESS_UTIL] Starting baseline detection\n"))
  cat(sprintf("[PROCESS_UTIL] Parameters:\n"))
  cat(sprintf("  - Temperature range: [%.1f, %.1f]C\n", temp_params$temp_min, temp_params$temp_max))
  cat(sprintf("  - Window size: %d\n", window_size))
  cat(sprintf("  - Exclusion zone: [%.1f, %.1f]C\n", exclusion_lower, exclusion_upper))
  cat(sprintf("  - Grid resolution: %.2fC\n", grid_resolution))
  cat(sprintf("  - Point selection: %s\n", point_selection))
  cat(sprintf("[PROCESS_UTIL] Format: %s, Samples: %d\n",
              format_info$format_type,
              length(format_info$sample_ids)))
  
  # Create temperature grid for output
  grid_temp <- seq(
    from = max(45, temp_params$temp_min),  # Start at 45 or temp_min, whichever is higher
    to = min(90, temp_params$temp_max),     # End at 90 or temp_max, whichever is lower
    by = grid_resolution
  )
  
  # Initialize results storage
  samples_list <- list()
  
  # Process each sample
  for (sample_id in format_info$sample_ids) {
    
    cat(sprintf("[PROCESS_UTIL] Processing sample: %s\n", sample_id))
    
    # Extract sample data based on format type
    sample_data <- extract_sample_data(data, sample_id, format_info$format_type)
    
    if (is.null(sample_data)) {
      cat(sprintf("[PROCESS_UTIL] WARNING: Could not extract data for sample %s\n", sample_id))
      next
    }
    
    # Validate sample has enough data points
    if (nrow(sample_data) < 10) {
      cat(sprintf("[PROCESS_UTIL] WARNING: Sample %s has < 10 data points, skipping\n", sample_id))
      next
    }
    
    # Filter by temperature range
    sample_data <- sample_data[
      sample_data$Temperature >= temp_params$temp_min & 
        sample_data$Temperature <= temp_params$temp_max,
    ]
    
    if (nrow(sample_data) < 10) {
      cat(sprintf("[PROCESS_UTIL] WARNING: Sample %s has < 10 points after filtering, skipping\n", sample_id))
      next
    }
    
    # Apply baseline detection
    baseline_result <- apply_baseline_detection(
      sample_data = sample_data,
      window_size = window_size,
      exclusion_lower = exclusion_lower,
      exclusion_upper = exclusion_upper,
      grid_temp = grid_temp,
      point_selection = point_selection,
      sample_id = sample_id
    )
    
    if (is.null(baseline_result)) {
      next  # Error logged in apply_baseline_detection
    }
    
    # Apply signal detection - pass the full processed data frame
    signal_result <- apply_signal_detection(
      processed_data = baseline_result$processed_df,
      sample_id = sample_id
    )
    
    
    # Store results
    samples_list[[sample_id]] <- list(
      sample_id = sample_id,
      # Original data (for raw plot in review module)
      temperature_original = baseline_result$temperature_original,
      dcp_original = baseline_result$dcp_original,
      # Processed data (interpolated, baseline-subtracted)
      temperature = baseline_result$temperature,
      dcp = baseline_result$dcp,
      baseline = baseline_result$baseline,
      baseline_subtracted = baseline_result$baseline_subtracted,
      # Endpoints
      lower_endpoint = baseline_result$lower_endpoint,
      upper_endpoint = baseline_result$upper_endpoint,
      # Signal detection result - EXPLICITLY store it
      has_signal = if (!is.null(signal_result$has_signal)) signal_result$has_signal else TRUE,
      # Metadata
      manual_adjustment = FALSE,
      excluded = FALSE,
      reviewed = FALSE,
      success = TRUE
    )
    
    cat(sprintf("[PROCESS_UTIL] [OK] Sample %s processed successfully (endpoints: %.1f, %.1f)\n", 
                sample_id, baseline_result$lower_endpoint, baseline_result$upper_endpoint))
  }
  
  # Check if any samples were processed
  if (length(samples_list) == 0) {
    stop("No samples were successfully processed")
  }
  
  # Calculate summary statistics safely
  n_signal <- 0
  n_no_signal <- 0
  
  for (sample in samples_list) {
    if (!is.null(sample) && !is.null(sample$has_signal)) {
      if (isTRUE(sample$has_signal)) {
        n_signal <- n_signal + 1
      } else {
        n_no_signal <- n_no_signal + 1
      }
    }
  }
  
  cat(sprintf("[PROCESS_UTIL] Summary: %d samples, %d with signal, %d without signal\n",
              length(samples_list), n_signal, n_no_signal))
  
  # Return results structure
  list(
    samples = samples_list,
    n_samples = length(samples_list),
    n_signal = n_signal,
    n_no_signal = n_no_signal,
    processing_params = list(
      temp_min = temp_params$temp_min,
      temp_max = temp_params$temp_max,
      window_size = window_size,
      exclusion_lower = exclusion_lower,
      exclusion_upper = exclusion_upper,
      grid_resolution = grid_resolution,
      point_selection = point_selection
    ),
    processing_time = Sys.time()
  )
}

# ==============================================================================

# ==============================================================================
# HELPER FUNCTIONS (Internal, not exported)
# ==============================================================================
#
# Internal utility functions used by the main processing pipeline.
#
# ==============================================================================


#' Extract Sample Data Based on Format Type
#' @keywords internal
extract_sample_data <- function(data, sample_id, format_type) {
  
  tryCatch({
    
    if (format_type == "multi_sample_wide") {
      # Wide format: T1a, 1a columns
      temp_col <- paste0("T", sample_id)
      dcp_col <- sample_id
      
      if (!(temp_col %in% names(data) && dcp_col %in% names(data))) {
        return(NULL)
      }
      
      sample_data <- data.frame(
        Temperature = data[[temp_col]],
        dCp = data[[dcp_col]]
      )
      
      # Remove NA rows
      sample_data <- sample_data[complete.cases(sample_data), ]
      
    } else if (format_type == "multi_sample_long") {
      # Long format: Sample_ID, Temperature, dCp
      sample_data <- data[data$Sample_ID == sample_id, c("Temperature", "dCp")]
      
    } else {
      # Single sample format
      sample_data <- data[, c("Temperature", "dCp")]
    }
    
    return(sample_data)
    
  }, error = function(e) {
    cat(sprintf("[PROCESS_UTIL] ERROR extracting sample %s: %s\n", sample_id, e$message))
    return(NULL)
  })
}

#' Apply Baseline Detection Using ThermogramBaseline Package
#' @keywords internal
apply_baseline_detection <- function(sample_data, window_size, exclusion_lower, 
                                     exclusion_upper, grid_temp, point_selection, 
                                     sample_id) {
  
  tryCatch({
    
    # Call auto.baseline - returns data frame with Temperature and dCp columns
    result_df <- ThermogramBaseline::auto.baseline(
      x = sample_data,
      w = window_size,
      exclusion.lwr = exclusion_lower,
      exclusion.upr = exclusion_upper,
      grid.temp = grid_temp,
      plot.on = FALSE,
      point = point_selection,
      explicit = FALSE
    )
    
    # Get endpoints by calling endpoint.detection directly
    endpoints <- ThermogramBaseline::endpoint.detection(
      x = sample_data,
      w = window_size,
      exclusion.lwr = exclusion_lower,
      exclusion.upr = exclusion_upper,
      point.selection = point_selection,
      explicit = FALSE
    )
    
    cat(sprintf("[PROCESS_UTIL] [OK] Baseline detection complete (endpoints: %.1f, %.1fC)\n", 
                endpoints$lower, endpoints$upper))
    
    # Return both original and processed data
    return(list(
      # Original data (for raw plot in review module)
      temperature_original = sample_data$Temperature,
      dcp_original = sample_data$dCp,
      # Processed data (interpolated, baseline-subtracted)
      temperature = result_df$Temperature,
      dcp = result_df$dCp,
      baseline = NULL,  # Not provided by auto.baseline
      baseline_subtracted = result_df$dCp,  # Final baseline-subtracted data
      lower_endpoint = endpoints$lower,
      upper_endpoint = endpoints$upper,
      # Keep the full processed data frame for signal detection
      processed_df = result_df
    ))
    
  }, error = function(e) {
    cat(sprintf("[PROCESS_UTIL] ERROR in auto.baseline for %s: %s\n", sample_id, e$message))
    return(NULL)
  })
}

#' Apply Signal Detection Using ThermogramBaseline Package
#' @keywords internal
apply_signal_detection <- function(processed_data, sample_id) {
  
  tryCatch({
    
    # Ensure dplyr is available
    if (!"package:dplyr" %in% search()) {
      library(dplyr, warn.conflicts = FALSE)
    }
    
    # Pass the entire data frame to signal.detection
    result <- ThermogramBaseline::signal.detection(processed_data)
    
    # Signal detection returns a data frame with column 'result' 
    # containing "Signal" or "No Signal"
    if (is.null(result) || !"result" %in% names(result)) {
      cat(sprintf("[PROCESS_UTIL] WARNING: Signal detection returned unexpected format for %s\n", sample_id))
      return(list(has_signal = TRUE))
    }
    
    # Extract the result string and convert to boolean
    result_string <- as.character(result$result[1])
    has_sig <- (result_string == "Signal")
    
    cat(sprintf("[PROCESS_UTIL] [OK] Signal detection: %s\n", result_string))
    
    return(list(has_signal = has_sig))
    
  }, error = function(e) {
    cat(sprintf("[PROCESS_UTIL] WARNING: Signal detection failed for %s: %s\n", sample_id, e$message))
    cat("[PROCESS_UTIL] Defaulting to 'has_signal = TRUE'\n")
    return(list(has_signal = TRUE))
  })
}