# Processing Utilities for ThermogramForge
# Baseline detection and signal quality assessment

#' Detect baseline endpoints and perform subtraction
#' 
#' @description
#' Wrapper for ThermogramBaseline::auto.baseline()
#' 
#' @param temperature Numeric vector of temperature values
#' @param dcp Numeric vector of dCp values  
#' @param w Window size for endpoint detection (default: 90)
#' @param exclusion_lwr Lower exclusion boundary (default: 60)
#' @param exclusion_upr Upper exclusion boundary (default: 80)
#' @param point Endpoint selection method: "innermost", "outmost", "mid" (default: "innermost")
#' 
#' @return List with baseline detection results
detect_baseline <- function(temperature, dcp, w = 90, 
                            exclusion_lwr = 60, exclusion_upr = 80,
                            point = "innermost") {
  
  # Remove NA values and create input data frame
  valid_idx <- !is.na(temperature) & !is.na(dcp)
  
  if (sum(valid_idx) < 10) {
    return(list(
      success = FALSE,
      error = "Insufficient data points"
    ))
  }
  
  input_data <- data.frame(
    Temperature = temperature[valid_idx],
    dCp = dcp[valid_idx]
  )
  
  tryCatch({
    # Call ThermogramBaseline::auto.baseline()
    result <- ThermogramBaseline::auto.baseline(
      x = input_data,
      w = w,
      exclusion.lwr = exclusion_lwr,
      exclusion.upr = exclusion_upr,
      grid.temp = seq(45, 90, 0.1),
      plot.on = FALSE,
      point = point,
      explicit = FALSE
    )
    
    # Extract endpoint information
    # Note: We need to get endpoints from the function's internal calculations
    # For now, use the exclusion boundaries as proxies
    list(
      success = TRUE,
      lower_endpoint = exclusion_lwr,
      upper_endpoint = exclusion_upr,
      baseline_subtracted = result$dCp,
      temperature = result$Temperature,
      result = result
    )
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = as.character(e)
    )
  })
}

#' Detect signal in thermogram
#' 
#' @description
#' Wrapper for ThermogramBaseline::signal.detection()
#' 
#' @param temperature Numeric vector of temperature values
#' @param dcp Numeric vector of dCp values
#' 
#' @return List with signal detection results
detect_signal <- function(temperature, dcp) {
  
  # Remove NA values
  valid_idx <- !is.na(temperature) & !is.na(dcp)
  
  if (sum(valid_idx) < 10) {
    return(list(
      has_signal = FALSE,
      confidence = 0,
      reason = "Insufficient data"
    ))
  }
  
  input_data <- data.frame(
    Temperature = temperature[valid_idx],
    dCp = dcp[valid_idx]
  )
  
  tryCatch({
    # Call ThermogramBaseline::signal.detection()
    result <- ThermogramBaseline::signal.detection(input_data)
    
    # Parse result
    has_signal <- result == "Signal"
    
    list(
      has_signal = has_signal,
      confidence = if (has_signal) 1.0 else 0.0,
      reason = result,
      result = result
    )
    
  }, error = function(e) {
    list(
      has_signal = FALSE,
      confidence = 0,
      reason = as.character(e)
    )
  })
}

#' Process a single thermogram sample
#' 
#' @param temperature Numeric vector of temperature values
#' @param dcp Numeric vector of dCp values
#' @param sample_id Character, sample identifier
#' 
#' @return List with processing results
process_single_sample <- function(temperature, dcp, sample_id) {
  
  tryCatch({
    # Run baseline detection
    baseline_result <- detect_baseline(temperature, dcp)
    
    if (!baseline_result$success) {
      return(list(
        sample_id = sample_id,
        success = FALSE,
        error = baseline_result$error,
        has_signal = FALSE
      ))
    }
    
    # Run signal detection
    signal_result <- detect_signal(temperature, dcp)
    
    # Return combined results
    list(
      sample_id = sample_id,
      success = TRUE,
      lower_endpoint = baseline_result$lower_endpoint,
      upper_endpoint = baseline_result$upper_endpoint,
      baseline_subtracted = baseline_result$baseline_subtracted,
      temperature = baseline_result$temperature,
      dcp_original = dcp[!is.na(temperature) & !is.na(dcp)],
      has_signal = signal_result$has_signal,
      signal_confidence = signal_result$confidence,
      signal_reason = signal_result$reason,
      reviewed = FALSE,
      excluded = FALSE,
      manual_adjustment = FALSE
    )
    
  }, error = function(e) {
    list(
      sample_id = sample_id,
      success = FALSE,
      error = as.character(e),
      has_signal = FALSE
    )
  })
}

#' Process wide-format thermogram data
#' 
#' @param data Data frame in wide format
#' @param format_info Format information from detect_data_format()
#' @param progress_callback Function to call with progress updates
#' 
#' @return List of processing results for each sample
process_wide_format <- function(data, format_info, progress_callback = NULL) {
  
  n_samples <- length(format_info$sample_ids)
  results <- vector("list", n_samples)
  
  for (i in seq_along(format_info$sample_ids)) {
    sample_id <- format_info$sample_ids[i]
    temp_col <- format_info$temperature_cols[i]
    dcp_col <- format_info$dcp_cols[i]
    
    # Update progress
    if (!is.null(progress_callback)) {
      progress_callback(
        value = i / n_samples,
        message = sprintf("Processing sample %s (%d/%d)", sample_id, i, n_samples)
      )
    }
    
    # Extract data for this sample
    temperature <- data[[temp_col]]
    dcp <- data[[dcp_col]]
    
    # Process sample
    results[[i]] <- process_single_sample(temperature, dcp, sample_id)
    
    # Small delay to show progress (remove in production)
    Sys.sleep(0.01)
  }
  
  names(results) <- format_info$sample_ids
  results
}

#' Process standard-format thermogram data
#' 
#' @param data Data frame in standard format
#' @param format_info Format information from detect_data_format()
#' @param progress_callback Function to call with progress updates
#' 
#' @return List of processing results for each sample
process_standard_format <- function(data, format_info, progress_callback = NULL) {
  
  if (format_info$format_type == "single_sample") {
    # Single sample
    if (!is.null(progress_callback)) {
      progress_callback(value = 0.5, message = "Processing single sample")
    }
    
    result <- process_single_sample(
      data[[format_info$temperature_col]],
      data[[format_info$dcp_col]],
      "Sample_1"
    )
    
    if (!is.null(progress_callback)) {
      progress_callback(value = 1.0, message = "Processing complete")
    }
    
    return(list(Sample_1 = result))
    
  } else {
    # Multi-sample long format
    sample_ids <- unique(data[[format_info$sample_id_col]])
    n_samples <- length(sample_ids)
    results <- vector("list", n_samples)
    
    for (i in seq_along(sample_ids)) {
      sample_id <- sample_ids[i]
      
      # Update progress
      if (!is.null(progress_callback)) {
        progress_callback(
          value = i / n_samples,
          message = sprintf("Processing sample %s (%d/%d)", sample_id, i, n_samples)
        )
      }
      
      # Extract data for this sample
      sample_data <- data[data[[format_info$sample_id_col]] == sample_id, ]
      
      # Process sample
      results[[i]] <- process_single_sample(
        sample_data[[format_info$temperature_col]],
        sample_data[[format_info$dcp_col]],
        sample_id
      )
      
      # Small delay to show progress
      Sys.sleep(0.01)
    }
    
    names(results) <- sample_ids
    results
  }
}

#' Main processing function
#' 
#' @param data Data frame with thermogram data
#' @param format_info Format information from detect_data_format()
#' @param progress_callback Function to call with progress updates
#' 
#' @return List of processing results
process_thermogram_data <- function(data, format_info, progress_callback = NULL) {
  
  if (format_info$data_format == "wide") {
    results <- process_wide_format(data, format_info, progress_callback)
  } else {
    results <- process_standard_format(data, format_info, progress_callback)
  }
  
  # Add summary statistics
  n_total <- length(results)
  n_success <- sum(sapply(results, function(x) x$success))
  n_failed <- n_total - n_success
  n_signal <- sum(sapply(results, function(x) x$success && x$has_signal))
  n_no_signal <- n_success - n_signal
  
  list(
    samples = results,
    summary = list(
      n_total = n_total,
      n_success = n_success,
      n_failed = n_failed,
      n_signal = n_signal,
      n_no_signal = n_no_signal
    ),
    format_info = format_info,
    processing_time = Sys.time()
  )
}