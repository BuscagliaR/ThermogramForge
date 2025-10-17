# Processing Utilities for ThermogramForge
# Baseline detection, signal quality assessment, and data management

#' Detect baseline endpoints and perform subtraction
#' 
#' @description
#' Detects baseline endpoints using ThermogramBaseline::endpoint.detection(),
#' then performs baseline subtraction and interpolation
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
    # First, detect the endpoints to get actual values
    endpoints <- ThermogramBaseline::endpoint.detection(
      x = input_data,
      w = w,
      exclusion.lwr = exclusion_lwr,
      exclusion.upr = exclusion_upr,
      point.selection = point,
      explicit = FALSE  # Suppress console output
    )
    
    # Extract the detected endpoint temperatures
    lower_endpoint <- endpoints$lower
    upper_endpoint <- endpoints$upper
    
    # Now perform baseline subtraction with detected endpoints
    baseline_result <- ThermogramBaseline::baseline.subtraction.byhand(
      x = input_data,
      lwr.temp = lower_endpoint,
      upr.temp = upper_endpoint,
      plot.on = FALSE
    )
    
    # Interpolate to standard grid
    final_result <- ThermogramBaseline::final.sample.interpolate(
      x = baseline_result,
      grid.temp = seq(45, 90, 0.1),
      plot.on = FALSE
    )
    
    # Return results with actual detected endpoints
    list(
      success = TRUE,
      lower_endpoint = lower_endpoint,
      upper_endpoint = upper_endpoint,
      baseline_subtracted = final_result$dCp,
      temperature = final_result$Temperature,
      endpoints_obj = endpoints,
      baseline_obj = baseline_result
    )
    
  }, error = function(e) {
    list(
      success = FALSE,
      error = as.character(e)
    )
  })
}

#' Re-process sample with manually specified endpoints
#' 
#' @description
#' Performs baseline subtraction using user-specified endpoint temperatures.
#' Used for manual endpoint adjustment.
#' 
#' @param temperature Numeric vector of temperature values
#' @param dcp Numeric vector of dCp values
#' @param lower_endpoint Numeric, manually specified lower endpoint (°C)
#' @param upper_endpoint Numeric, manually specified upper endpoint (°C)
#' 
#' @return List with baseline subtraction results
reprocess_with_manual_endpoints <- function(temperature, dcp, 
                                            lower_endpoint, 
                                            upper_endpoint) {
  
  # Remove NA values and create input data frame
  valid_idx <- !is.na(temperature) & !is.na(dcp)
  
  if (sum(valid_idx) < 10) {
    return(list(
      success = FALSE,
      error = "Insufficient data points"
    ))
  }
  
  # Validate endpoints
  if (lower_endpoint >= upper_endpoint) {
    return(list(
      success = FALSE,
      error = "Lower endpoint must be less than upper endpoint"
    ))
  }
  
  input_data <- data.frame(
    Temperature = temperature[valid_idx],
    dCp = dcp[valid_idx]
  )
  
  # Check that endpoints are within data range
  temp_range <- range(input_data$Temperature)
  if (lower_endpoint < temp_range[1] || upper_endpoint > temp_range[2]) {
    return(list(
      success = FALSE,
      error = sprintf(
        "Endpoints (%.1f, %.1f) outside data range (%.1f, %.1f)",
        lower_endpoint, upper_endpoint, temp_range[1], temp_range[2]
      )
    ))
  }
  
  tryCatch({
    # Perform baseline subtraction with manual endpoints
    baseline_result <- ThermogramBaseline::baseline.subtraction.byhand(
      x = input_data,
      lwr.temp = lower_endpoint,
      upr.temp = upper_endpoint,
      plot.on = FALSE
    )
    
    # Interpolate to standard grid
    final_result <- ThermogramBaseline::final.sample.interpolate(
      x = baseline_result,
      grid.temp = seq(45, 90, 0.1),
      plot.on = FALSE
    )
    
    # Return results
    list(
      success = TRUE,
      lower_endpoint = lower_endpoint,
      upper_endpoint = upper_endpoint,
      baseline_subtracted = final_result$dCp,
      temperature = final_result$Temperature,
      baseline_obj = baseline_result,
      manual = TRUE
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
    
    # Store original raw data for reprocessing
    valid_idx <- !is.na(temperature) & !is.na(dcp)
    original_temp <- temperature[valid_idx]
    original_dcp <- dcp[valid_idx]
    
    # Return combined results
    list(
      sample_id = sample_id,
      success = TRUE,
      lower_endpoint = baseline_result$lower_endpoint,
      upper_endpoint = baseline_result$upper_endpoint,
      baseline_subtracted = baseline_result$baseline_subtracted,
      temperature = baseline_result$temperature,  # Interpolated grid for plotting
      dcp_original = original_dcp,  # Original dcp values
      temperature_original = original_temp,  # Original temperature values
      has_signal = signal_result$has_signal,
      signal_confidence = signal_result$confidence,
      signal_reason = signal_result$reason,
      reviewed = FALSE,
      excluded = FALSE,
      manual_adjustment = FALSE,
      lower_manual = FALSE,  # Track which endpoint was manually adjusted
      upper_manual = FALSE,
      # Store original auto-detected endpoints for undo
      auto_lower_endpoint = baseline_result$lower_endpoint,
      auto_upper_endpoint = baseline_result$upper_endpoint
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
    
    # Small delay to show progress
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

# =============================================================================
# PHASE 7: DATA MANAGEMENT FUNCTIONS
# =============================================================================

#' Convert processed data to wide format for export
#' 
#' @description
#' Converts processed thermogram data to wide format matching Python implementation:
#' - Temperature values as column headers
#' - Sample IDs as row index
#' - Baseline-subtracted dCp values as data
#' - Common interpolated temperature grid
#' 
#' @param data Processed data list from app_data$processed_data
#' 
#' @return Data frame in wide format with SampleID column
format_data_for_wide_export <- function(data) {
  
  if (is.null(data$samples) || length(data$samples) == 0) {
    return(data.frame())
  }
  
  # Get all successful samples
  successful_samples <- Filter(function(x) x$success, data$samples)
  
  if (length(successful_samples) == 0) {
    return(data.frame())
  }
  
  # Use the temperature grid from the first successful sample
  # All samples should have the same grid after interpolation
  temp_grid <- successful_samples[[1]]$temperature
  n_temps <- length(temp_grid)
  
  # Create matrix to hold all sample data
  sample_ids <- names(successful_samples)
  n_samples <- length(sample_ids)
  
  # Initialize matrix: rows = samples, cols = temperatures
  data_matrix <- matrix(NA, nrow = n_samples, ncol = n_temps)
  
  # Fill matrix with baseline-subtracted data
  for (i in seq_along(sample_ids)) {
    sample_id <- sample_ids[i]
    sample <- successful_samples[[sample_id]]
    
    # Verify temperature grid matches
    if (length(sample$temperature) == n_temps &&
        all(abs(sample$temperature - temp_grid) < 0.01)) {
      data_matrix[i, ] <- sample$baseline_subtracted
    } else {
      warning(sprintf("Sample %s has mismatched temperature grid, filling with NA", sample_id))
    }
  }
  
  # Convert to data frame with check.names = FALSE to preserve numeric column names
  df_wide <- as.data.frame(data_matrix, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Set column names as temperature values (formatted to 1 decimal)
  colnames(df_wide) <- sprintf("%.1f", temp_grid)
  
  # Add SampleID as first column
  df_wide <- data.frame(
    SampleID = sample_ids,
    df_wide,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # Apply edge column adjustment (matching Python implementation)
  if (ncol(df_wide) > 3) {  # Need at least SampleID + 2 temp columns
    # Column indices: col 2 is first temp, col 3 is second temp
    # col ncol-1 is second-to-last temp, col ncol is last temp
    second_temp_col <- df_wide[[3]]
    second_last_temp_col <- df_wide[[ncol(df_wide) - 1]]
    
    df_wide[[2]] <- 0.5 * second_temp_col
    df_wide[[ncol(df_wide)]] <- 0.5 * second_last_temp_col
  }
  
  df_wide
}

#' Save processed thermogram data
#' 
#' @description
#' Saves processed data in RDS, CSV, or Excel format
#' 
#' @param data Processed data list from app_data$processed_data
#' @param filename Base filename (without extension)
#' @param format One of "rds", "csv", or "xlsx"
#' @param output_dir Output directory (default: "data/processed")
#' 
#' @return List with success, filepath, and message
save_processed_data <- function(data, filename, format = "rds", 
                                output_dir = "data/processed") {
  
  # Validate inputs
  if (is.null(data) || !is.list(data)) {
    return(list(
      success = FALSE,
      message = "Invalid data: must be a list"
    ))
  }
  
  if (!format %in% c("rds", "csv", "xlsx")) {
    return(list(
      success = FALSE,
      message = "Invalid format: must be 'rds', 'csv', or 'xlsx'"
    ))
  }
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Build metadata
  metadata <- list(
    saved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    filename = filename,
    n_samples = length(data$samples),
    n_reviewed = sum(sapply(data$samples, function(s) isTRUE(s$reviewed))),
    n_excluded = sum(sapply(data$samples, function(s) isTRUE(s$excluded))),
    n_manual = sum(sapply(data$samples, function(s) isTRUE(s$manual_adjustment))),
    version = "0.1.0",
    format = format
  )
  
  tryCatch({
    if (format == "rds") {
      # RDS: Save full R object with metadata
      filepath <- file.path(output_dir, paste0(filename, ".rds"))
      save_obj <- list(
        metadata = metadata,
        data = data
      )
      saveRDS(save_obj, filepath)
      
      return(list(
        success = TRUE,
        filepath = filepath,
        message = sprintf("Saved as RDS: %s", basename(filepath))
      ))
      
    } else if (format == "csv") {
      # CSV: Wide format with interpolated data
      wide_data <- format_data_for_wide_export(data)
      
      if (nrow(wide_data) == 0) {
        return(list(
          success = FALSE,
          message = "No data to export"
        ))
      }
      
      filepath <- file.path(output_dir, paste0(filename, ".csv"))
      readr::write_csv(wide_data, filepath)
      
      # Save metadata separately
      meta_filepath <- file.path(output_dir, paste0(filename, "_metadata.txt"))
      writeLines(
        c(
          paste("Saved:", metadata$saved_at),
          paste("Samples:", metadata$n_samples),
          paste("Reviewed:", metadata$n_reviewed),
          paste("Excluded:", metadata$n_excluded),
          paste("Manual Adjustments:", metadata$n_manual),
          paste("Version:", metadata$version)
        ),
        meta_filepath
      )
      
      return(list(
        success = TRUE,
        filepath = filepath,
        message = sprintf("Saved as CSV: %s (with metadata file)", basename(filepath))
      ))
      
    } else if (format == "xlsx") {
      # Excel: Wide format + metadata sheet
      wide_data <- format_data_for_wide_export(data)
      
      if (nrow(wide_data) == 0) {
        return(list(
          success = FALSE,
          message = "No data to export"
        ))
      }
      
      filepath <- file.path(output_dir, paste0(filename, ".xlsx"))
      
      # Metadata sheet
      metadata_sheet <- data.frame(
        Property = c("Saved At", "Samples", "Reviewed", "Excluded", 
                     "Manual Adjustments", "Version"),
        Value = c(metadata$saved_at, metadata$n_samples, metadata$n_reviewed,
                  metadata$n_excluded, metadata$n_manual, metadata$version),
        stringsAsFactors = FALSE
      )
      
      # Write to Excel with multiple sheets
      writexl::write_xlsx(
        list(
          Data = wide_data,
          Metadata = metadata_sheet
        ),
        path = filepath
      )
      
      return(list(
        success = TRUE,
        filepath = filepath,
        message = sprintf("Saved as Excel: %s", basename(filepath))
      ))
    }
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = sprintf("Save failed: %s", e$message)
    ))
  })
}

#' Load processed thermogram data
#' 
#' @description
#' Loads processed data from RDS format (CSV/Excel not supported for loading)
#' 
#' @param filepath Full path to saved RDS file
#' 
#' @return List with success, data, metadata, and message
load_processed_data <- function(filepath) {
  
  if (!file.exists(filepath)) {
    return(list(
      success = FALSE,
      message = sprintf("File not found: %s", filepath)
    ))
  }
  
  ext <- tolower(tools::file_ext(filepath))
  
  if (ext != "rds") {
    return(list(
      success = FALSE,
      message = "Only RDS format can be loaded. CSV/Excel are export-only formats."
    ))
  }
  
  tryCatch({
    loaded <- readRDS(filepath)
    
    # Validate structure
    if (!is.list(loaded) || !all(c("metadata", "data") %in% names(loaded))) {
      return(list(
        success = FALSE,
        message = "Invalid RDS file structure: missing metadata or data"
      ))
    }
    
    return(list(
      success = TRUE,
      data = loaded$data,
      metadata = loaded$metadata,
      message = "Successfully loaded RDS file"
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = sprintf("Load failed: %s", e$message)
    ))
  })
}

#' List saved processed data files
#' 
#' @description
#' Scans directory for processed data files
#' 
#' @param output_dir Directory to scan (default: "data/processed")
#' 
#' @return Data frame with file information
list_saved_datasets <- function(output_dir = "data/processed") {
  
  if (!dir.exists(output_dir)) {
    return(data.frame(
      filename = character(0),
      format = character(0),
      size_mb = numeric(0),
      modified = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  files <- list.files(
    output_dir,
    pattern = "\\.(rds|csv|xlsx)$",
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    return(data.frame(
      filename = character(0),
      format = character(0),
      size_mb = numeric(0),
      modified = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  file_info <- file.info(files)
  
  data.frame(
    filename = basename(files),
    format = toupper(tools::file_ext(files)),
    size_mb = round(file_info$size / 1024^2, 2),
    modified = format(file_info$mtime, "%Y-%m-%d %H:%M:%S"),
    filepath = files,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# PHASE 7: LOAD/DELETE FUNCTIONALITY
# =============================================================================

#' Load processed thermogram data
#' 
#' @description
#' Loads processed data from RDS, CSV, or Excel format.
#' - RDS: Full reload with all sample curves (can use in Review Endpoints + Reports)
#' - CSV/Excel: Report-ready data only (can generate reports, but no Review Endpoints)
#' 
#' @param filepath Full path to saved file
#' 
#' @return List with success, data, message, metadata, and format
load_processed_data <- function(filepath) {
  
  # Validate file exists
  if (!file.exists(filepath)) {
    return(list(
      success = FALSE,
      message = "File not found",
      format = NULL,
      data_type = NULL
    ))
  }
  
  # Detect format from extension
  ext <- tolower(tools::file_ext(filepath))
  
  tryCatch({
    if (ext == "rds") {
      # ============================================================
      # RDS FORMAT - Full reload capability
      # ============================================================
      loaded <- readRDS(filepath)
      
      # Validate structure
      if (!is.list(loaded)) {
        return(list(
          success = FALSE,
          message = "Invalid RDS structure: not a list",
          format = "rds",
          data_type = NULL
        ))
      }
      
      if (!all(c("metadata", "data") %in% names(loaded))) {
        return(list(
          success = FALSE,
          message = "Invalid RDS structure: missing 'metadata' or 'data' fields",
          format = "rds",
          data_type = NULL
        ))
      }
      
      # Validate data has required components for full reload
      if (!all(c("samples", "summary") %in% names(loaded$data))) {
        return(list(
          success = FALSE,
          message = "Invalid data structure: missing 'samples' or 'summary'",
          format = "rds",
          data_type = NULL
        ))
      }
      
      return(list(
        success = TRUE,
        data = loaded$data,
        metadata = loaded$metadata,
        format = "rds",
        data_type = "full",  # Full data with all curves
        message = sprintf(
          "Loaded %d samples from RDS file (saved %s)",
          loaded$metadata$n_samples,
          loaded$metadata$saved_at
        )
      ))
      
    } else if (ext == "csv") {
      # ============================================================
      # CSV FORMAT - Report-ready data only
      # ============================================================
      
      # Read CSV
      csv_data <- readr::read_csv(filepath, show_col_types = FALSE)
      
      # Try to find companion metadata file
      meta_filepath <- sub("\\.csv$", "_metadata.txt", filepath)
      metadata <- list(
        format = "csv",
        loaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        note = "CSV file loaded for report generation only"
      )
      
      if (file.exists(meta_filepath)) {
        meta_lines <- readLines(meta_filepath)
        metadata$original_metadata <- paste(meta_lines, collapse = "\n")
      }
      
      # Create report-ready structure
      report_data <- list(
        wide_data = csv_data,
        n_samples = nrow(csv_data),
        format_info = list(
          format_type = "wide",
          source = "csv_import"
        )
      )
      
      return(list(
        success = TRUE,
        data = report_data,
        format = "csv",
        data_type = "report_only",  # Can only generate reports
        metadata = metadata,
        message = sprintf(
          "Loaded CSV file with %d samples for report generation",
          nrow(csv_data)
        )
      ))
      
    } else if (ext %in% c("xlsx", "xls")) {
      # ============================================================
      # EXCEL FORMAT - Report-ready data only
      # ============================================================
      
      # Read Excel - check for multiple sheets
      sheet_names <- readxl::excel_sheets(filepath)
      
      # Look for data sheet (usually first or named "Data")
      data_sheet <- if ("Data" %in% sheet_names) "Data" else sheet_names[1]
      excel_data <- readxl::read_excel(filepath, sheet = data_sheet)
      
      # Try to read metadata sheet if it exists
      metadata <- list(
        format = "xlsx",
        loaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        note = "Excel file loaded for report generation only"
      )
      
      if ("Metadata" %in% sheet_names) {
        meta_df <- readxl::read_excel(filepath, sheet = "Metadata")
        metadata$original_metadata <- paste(
          apply(meta_df, 1, function(row) paste(row, collapse = ": ")),
          collapse = "\n"
        )
      }
      
      # Create report-ready structure
      report_data <- list(
        wide_data = excel_data,
        n_samples = nrow(excel_data),
        format_info = list(
          format_type = "wide",
          source = "excel_import"
        )
      )
      
      return(list(
        success = TRUE,
        data = report_data,
        format = "xlsx",
        data_type = "report_only",  # Can only generate reports
        metadata = metadata,
        message = sprintf(
          "Loaded Excel file with %d samples for report generation",
          nrow(excel_data)
        )
      ))
      
    } else {
      return(list(
        success = FALSE,
        message = sprintf("Unsupported file format: %s", ext),
        format = ext,
        data_type = NULL
      ))
    }
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = sprintf("Error loading file: %s", e$message),
      format = ext,
      data_type = NULL
    ))
  })
}


#' List saved processed datasets
#' 
#' @description
#' Scans the data/processed directory and returns information about saved files
#' 
#' @param output_dir Directory to scan (default: "data/processed")
#' 
#' @return Data frame with file information
list_processed_datasets <- function(output_dir = "data/processed") {
  
  # Check if directory exists
  if (!dir.exists(output_dir)) {
    return(data.frame(
      filename = character(0),
      filepath = character(0),
      format = character(0),
      size_mb = numeric(0),
      modified = character(0),
      can_load = logical(0),
      load_type = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Find all processed data files
  all_files <- list.files(
    output_dir, 
    pattern = "\\.(rds|csv|xlsx)$", 
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(all_files) == 0) {
    return(data.frame(
      filename = character(0),
      filepath = character(0),
      format = character(0),
      size_mb = numeric(0),
      modified = character(0),
      can_load = logical(0),
      load_type = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Get file info
  file_info <- file.info(all_files)
  
  # Determine load type for each file
  load_types <- sapply(all_files, function(f) {
    ext <- tolower(tools::file_ext(f))
    if (ext == "rds") {
      "Full (Review + Reports)"
    } else {
      "Reports Only"
    }
  })
  
  # Build data frame
  df <- data.frame(
    filename = basename(all_files),
    filepath = all_files,
    format = toupper(tools::file_ext(all_files)),
    size_mb = round(file_info$size / 1024^2, 2),
    modified = format(file_info$mtime, "%Y-%m-%d %H:%M:%S"),
    can_load = TRUE,  # ALL formats can now be loaded
    load_type = unname(load_types),
    stringsAsFactors = FALSE
  )
  
  # Sort by modified date (most recent first)
  df <- df[order(df$modified, decreasing = TRUE), ]
  rownames(df) <- NULL
  
  return(df)
}


#' Delete processed dataset file
#' 
#' @description
#' Safely deletes a processed dataset file and its companion metadata if it exists
#' 
#' @param filepath Full path to file to delete
#' 
#' @return List with success flag and message
delete_processed_data <- function(filepath) {
  
  if (!file.exists(filepath)) {
    return(list(
      success = FALSE,
      message = "File not found"
    ))
  }
  
  tryCatch({
    # Delete main file
    file.remove(filepath)
    
    # Try to delete companion metadata file if it exists
    meta_filepath <- sub("\\.(rds|csv|xlsx)$", "_metadata.txt", filepath)
    if (file.exists(meta_filepath)) {
      file.remove(meta_filepath)
    }
    
    return(list(
      success = TRUE,
      message = sprintf("Successfully deleted %s", basename(filepath))
    ))
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = sprintf("Error deleting file: %s", e$message)
    ))
  })
}

# ============================================================================
# TLBPARAM METRIC CALCULATION FUNCTIONS
# ============================================================================

#' Convert processed samples to tlbparam format
#'
#' @description
#' Converts the processed_data$samples format to the wide format expected by tlbparam.
#' Column names: T45, T45.1, T45.2, ..., T89.9, T90
#' 
#' @param processed_data List containing samples with baseline-subtracted data
#' 
#' @return Data frame in tlbparam format (wide with temperature columns)
convert_to_tlbparam_format <- function(processed_data) {
  
  samples <- processed_data$samples
  
  if (length(samples) == 0) {
    stop("No samples found in processed_data")
  }
  
  # Define the standard temperature grid (45 to 90 by 0.1)
  temp_grid <- seq(45, 90, by = 0.1)
  
  # Create column names for tlbparam format
  # Format: T45, T45.1, T45.2, ..., T89.9, T90
  temp_col_names <- paste0("T", temp_grid)
  
  cat(sprintf("[TLBPARAM] Standard grid: %d temperatures from %.1f to %.1f\n", 
              length(temp_grid), min(temp_grid), max(temp_grid)))
  cat(sprintf("[TLBPARAM] Column names: %s ... %s\n", 
              temp_col_names[1], tail(temp_col_names, 1)))
  
  # Initialize list to store each sample's data
  sample_list <- list()
  
  for (sample_id in names(samples)) {
    sample <- samples[[sample_id]]
    
    # Skip failed samples
    if (!sample$success) {
      cat(sprintf("[TLBPARAM] Skipping failed sample: %s\n", sample_id))
      next
    }
    
    # Get baseline-subtracted data
    temperature <- as.numeric(sample$temperature)
    dcp_subtracted <- as.numeric(sample$baseline_subtracted)
    
    # Remove NAs
    valid_idx <- !is.na(temperature) & !is.na(dcp_subtracted)
    temperature <- temperature[valid_idx]
    dcp_subtracted <- dcp_subtracted[valid_idx]
    
    if (length(temperature) < 10) {
      cat(sprintf("[TLBPARAM] Insufficient data for sample: %s\n", sample_id))
      next
    }
    
    # Interpolate to standard grid if needed
    # Round temperatures to nearest 0.1 to match grid
    temperature_rounded <- round(temperature, 1)
    
    # Check if data is already on grid
    if (!all(temp_grid %in% temperature_rounded)) {
      cat(sprintf("[TLBPARAM] Interpolating sample %s to standard grid\n", sample_id))
      
      # Use approx for linear interpolation
      interp_result <- approx(
        x = temperature,
        y = dcp_subtracted,
        xout = temp_grid,
        rule = 2  # Use nearest value for extrapolation
      )
      
      dcp_on_grid <- interp_result$y
    } else {
      # Data already on grid, just reorder
      idx_match <- match(temp_grid, temperature_rounded)
      dcp_on_grid <- dcp_subtracted[idx_match]
    }
    
    # Create data frame for this sample with proper column names
    sample_df <- setNames(
      data.frame(matrix(dcp_on_grid, nrow = 1), stringsAsFactors = FALSE),
      temp_col_names
    )
    
    # Add SampleCode as first column
    sample_df <- data.frame(
      SampleCode = sample_id,
      sample_df,
      stringsAsFactors = FALSE
    )
    
    sample_list[[sample_id]] <- sample_df
    cat(sprintf("[TLBPARAM] Formatted sample: %s (%d temp columns)\n", 
                sample_id, length(temp_col_names)))
  }
  
  if (length(sample_list) == 0) {
    stop("No valid samples to process")
  }
  
  # Combine all samples into one data frame
  result_df <- do.call(rbind, sample_list)
  rownames(result_df) <- NULL
  
  cat(sprintf("[TLBPARAM] Final format: %d samples x %d total columns (1 ID + %d temps)\n", 
              nrow(result_df), ncol(result_df), length(temp_col_names)))
  
  # Verify column names
  cat(sprintf("[TLBPARAM] First temp column: %s, Last temp column: %s\n",
              names(result_df)[2], names(result_df)[ncol(result_df)]))
  
  return(result_df)
}

#' Calculate tlbparam metrics for processed samples
#'
#' @description
#' Main function to calculate thermogram metrics using tlbparam package
#' 
#' @param processed_data List containing processed sample data
#' @param selected_metrics Character vector of metric names to calculate
#' 
#' @return Data frame with Sample_ID and selected metric columns, or NULL if error
calculate_tlbparam_metrics <- function(processed_data, selected_metrics = NULL) {
  
  cat("\n[TLBPARAM] Starting metric calculation\n")
  cat(sprintf("[TLBPARAM] Number of samples: %d\n", length(processed_data$samples)))
  
  # Check if tlbparam is available
  if (!requireNamespace("tlbparam", quietly = TRUE)) {
    stop("tlbparam package is not installed. Install it from GitHub: remotes::install_github('BuscagliaR/tlbparam')")
  }
  
  # Convert to tlbparam format
  cat("[TLBPARAM] Converting data to tlbparam format...\n")
  
  tlb_data <- tryCatch({
    convert_to_tlbparam_format(processed_data)
  }, error = function(e) {
    cat(sprintf("[TLBPARAM] ERROR in format conversion: %s\n", e$message))
    return(NULL)
  })
  
  if (is.null(tlb_data)) {
    return(NULL)
  }
  
  cat(sprintf("[TLBPARAM] Converted %d samples successfully\n", nrow(tlb_data)))
  
  # Calculate all metrics using tlbparam
  cat("[TLBPARAM] Calling clean_thermograms()...\n")
  
  all_metrics <- tryCatch({
    tlbparam::clean_thermograms(
      df = tlb_data,
      type = "Plasma",
      column = "SampleCode",
      low_temp = "T45",
      high_temp = "T90",
      temp_range = seq(45, 90, by = 0.1)
    )
  }, error = function(e) {
    cat(sprintf("[TLBPARAM] ERROR in clean_thermograms: %s\n", e$message))
    return(NULL)
  })
  
  if (is.null(all_metrics)) {
    return(NULL)
  }
  
  cat(sprintf("[TLBPARAM] Calculated metrics for %d samples\n", nrow(all_metrics)))
  
  # Show available columns (excluding SampleCode)
  metric_cols <- setdiff(names(all_metrics), "SampleCode")
  cat(sprintf("[TLBPARAM] Available metric columns: %s\n", paste(metric_cols, collapse = ", ")))
  cat(sprintf("[TLBPARAM] Extracted %d metrics\n", length(metric_cols)))
  
  # If no specific metrics selected, return all
  if (is.null(selected_metrics) || length(selected_metrics) == 0) {
    cat("[TLBPARAM] Returning all metrics\n")
    
    # Rename SampleCode to Sample_ID for consistency
    names(all_metrics)[names(all_metrics) == "SampleCode"] <- "Sample_ID"
    
    return(all_metrics)
  }
  
  # Map UI metric names to tlbparam column names
  metric_map <- create_metric_name_map()
  
  # Select only requested metrics
  selected_cols <- c("SampleCode")
  found_metrics <- character(0)
  
  for (metric in selected_metrics) {
    tlb_col_name <- metric_map[[metric]]
    
    if (is.na(tlb_col_name)) {
      cat(sprintf("[TLBPARAM] Skipping unavailable metric: %s\n", metric))
      next
    }
    
    if (!is.null(tlb_col_name) && tlb_col_name %in% names(all_metrics)) {
      selected_cols <- c(selected_cols, tlb_col_name)
      found_metrics <- c(found_metrics, metric)
      cat(sprintf("[TLBPARAM] ✓ Found: %s -> '%s'\n", metric, tlb_col_name))
    } else {
      cat(sprintf("[TLBPARAM] Warning: Metric '%s' (tlbparam name: '%s') not found in results\n", 
                  metric, tlb_col_name))
    }
  }
  
  if (length(selected_cols) == 1) {
    cat("[TLBPARAM] No valid metrics found\n")
    return(NULL)
  }
  
  # Extract selected columns
  result <- all_metrics[, selected_cols, drop = FALSE]
  
  # Rename columns to match UI names
  # Create reverse mapping
  reverse_map <- setNames(names(metric_map), unlist(metric_map))
  
  for (col in names(result)) {
    if (col != "SampleCode" && col %in% names(reverse_map)) {
      new_name <- reverse_map[col]
      names(result)[names(result) == col] <- new_name
    }
  }
  
  # Rename SampleCode to Sample_ID for consistency
  names(result)[names(result) == "SampleCode"] <- "Sample_ID"
  
  cat(sprintf("[TLBPARAM] ✅ Returning %d metrics for %d samples\n", 
              ncol(result) - 1, nrow(result)))
  cat(sprintf("[TLBPARAM] Final columns: %s\n", 
              paste(names(result), collapse = ", ")))
  
  return(result)
}

#' Create mapping between UI metric names and tlbparam column names
#'
#' @description
#' Maps the metric names used in the UI to the actual column names
#' returned by tlbparam::clean_thermograms()
#' 
#' Based on actual output:
#' Width, Area, Max, TMax, TFM, Peak 1, Peak 2, Peak 3, 
#' TPeak 1, TPeak 2, TPeak 3, Peak 1 / Peak 2, Peak 1 / Peak 3, 
#' Peak 2 / Peak 3, Median, V1.2, TV1.2, V1.2 / Peak 1, 
#' V1.2 / Peak 2, V1.2 / Peak 3, Min, TMin, Peak F, TPeak F
#' 
#' @return Named list mapping UI names to tlbparam names
create_metric_name_map <- function() {
  list(
    # Peak Metrics
    "Tm" = "TMax",           # Temperature at maximum
    "Tpeak_1" = "TPeak 1",   # Peak 1 temperature
    "Tpeak_2" = "TPeak 2",   # Peak 2 temperature
    "Tpeak_3" = "TPeak 3",   # Peak 3 temperature
    "Tpeak_f" = "TPeak F",   # Fibrinogen peak temperature
    
    # Transition Metrics
    "Tm1" = "TPeak 1",       # Same as Tpeak_1
    "Tm2" = "TPeak 2",       # Same as Tpeak_2
    "Tm3" = "TPeak 3",       # Same as Tpeak_3
    "Tonset" = "TMin",       # Onset temperature (approximation)
    "Toffset" = "TMax",      # Offset temperature (approximation)
    "TV12" = "TV1.2",        # Valley between peak 1 and 2
    
    # Area Metrics
    "AUC" = "Area",          # Total area
    "AUC_normalized" = "Area",  # Same as AUC
    "Area" = "Area",         # Total area
    
    # Shape Metrics
    "FWHM" = "Width",        # Full width at half maximum
    "Width_50" = "Width",    # Same as FWHM
    "Width_80" = "Width",    # Approximation (tlbparam doesn't have 80%)
    "Asymmetry" = NA,        # Not available in tlbparam
    
    # Height Metrics
    "Max" = "Max",           # Maximum dCp value
    "Min" = "Min",           # Minimum dCp value
    "Median" = "Median",     # Median dCp value
    
    # Temperature Metrics
    "TMax" = "TMax",         # Temperature at maximum
    "TMin" = "TMin",         # Temperature at minimum
    "TFM" = "TFM",           # Temperature of first moment
    
    # Ratio Metrics
    "V1.2_Peak1_Ratio" = "V1.2 / Peak 1",   # Valley/Peak 1 ratio
    "V1.2_Peak2_Ratio" = "V1.2 / Peak 2",   # Valley/Peak 2 ratio
    "V1.2_Peak3_Ratio" = "V1.2 / Peak 3"    # Valley/Peak 3 ratio
  )
}

#' Get available metric names from tlbparam
#'
#' @description
#' Returns list of all metrics that can be calculated
#' 
#' @return Character vector of metric names
get_available_metrics <- function() {
  c(
    # Peak Metrics
    "Tm", "Tpeak_1", "Tpeak_2", "Tpeak_3", "Tpeak_f",
    # Transition Metrics
    "Tm1", "Tm2", "Tm3", "Tonset", "Toffset", "TV12",
    # Area Metrics
    "AUC", "AUC_normalized", "Area",
    # Shape Metrics
    "FWHM", "Width_50", "Width_80",
    # Height Metrics
    "Max", "Min", "Median",
    # Temperature Metrics
    "TMax", "TMin", "TFM",
    # Ratio Metrics (note: these need special handling)
    "V1.2_Peak1_Ratio", "V1.2_Peak2_Ratio", "V1.2_Peak3_Ratio"
  )
}

# ============================================================================
# CSV REPORT EXPORT FUNCTION
# Phase 8 - Session 3
# ============================================================================

#' Export calculated metrics to CSV
#'
#' @description
#' Saves calculated metrics data frame to a CSV file in the reports/ directory
#'
#' @param metrics_data Data frame with Sample_ID and metric columns
#' @param report_name User-provided report name (optional)
#' @param dataset_name Name of the source dataset
#'
#' @return List containing:
#'   - success: logical indicating if save succeeded
#'   - filepath: full path to saved file (if successful)
#'   - filename: just the filename (if successful)
#'   - message: status message
#'
#' @export
export_report_csv <- function(metrics_data, report_name = NULL, dataset_name = "Unknown") {
  
  cat("\n[CSV_EXPORT] Starting CSV export\n")
  
  # Validate inputs
  if (is.null(metrics_data) || nrow(metrics_data) == 0) {
    return(list(
      success = FALSE,
      message = "No data to export"
    ))
  }
  
  # Create reports directory if it doesn't exist
  reports_dir <- "reports"
  if (!dir.exists(reports_dir)) {
    dir.create(reports_dir, recursive = TRUE)
    cat(sprintf("[CSV_EXPORT] Created directory: %s\n", reports_dir))
  }
  
  # Generate filename
  if (is.null(report_name) || nchar(trimws(report_name)) == 0) {
    # Auto-generate name with timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- sprintf("Report_%s.csv", timestamp)
    cat("[CSV_EXPORT] Using auto-generated filename\n")
  } else {
    # Use provided name (sanitize it)
    clean_name <- gsub("[^A-Za-z0-9_-]", "_", report_name)
    filename <- sprintf("%s.csv", clean_name)
    cat(sprintf("[CSV_EXPORT] Using custom filename: %s\n", filename))
  }
  
  filepath <- file.path(reports_dir, filename)
  
  # Check if file already exists
  if (file.exists(filepath)) {
    # Add timestamp to make unique
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base_name <- tools::file_path_sans_ext(filename)
    filename <- sprintf("%s_%s.csv", base_name, timestamp)
    filepath <- file.path(reports_dir, filename)
    cat(sprintf("[CSV_EXPORT] File exists, using unique name: %s\n", filename))
  }
  
  # Write CSV
  tryCatch({
    readr::write_csv(metrics_data, filepath)
    
    cat(sprintf("[CSV_EXPORT] ✅ Successfully saved to: %s\n", filepath))
    cat(sprintf("[CSV_EXPORT] Exported %d samples, %d metrics\n", 
                nrow(metrics_data), 
                ncol(metrics_data) - 1))  # -1 for Sample_ID column
    
    return(list(
      success = TRUE,
      filepath = filepath,
      filename = filename,
      message = sprintf("Report saved: %s", filename)
    ))
    
  }, error = function(e) {
    cat(sprintf("[CSV_EXPORT] ❌ Error: %s\n", e$message))
    return(list(
      success = FALSE,
      message = sprintf("Error saving CSV: %s", e$message)
    ))
  })
}

# ============================================================================
# EXCEL REPORT EXPORT FUNCTION
# Phase 8 - Session 3
# ============================================================================

#' Export calculated metrics to Excel with metadata
#'
#' @description
#' Saves calculated metrics data frame to an Excel file with multiple sheets:
#' - Sheet 1: Metrics data
#' - Sheet 2: Metadata (dataset info, generation time, metric list)
#'
#' @param metrics_data Data frame with Sample_ID and metric columns
#' @param report_name User-provided report name (optional)
#' @param dataset_name Name of the source dataset
#'
#' @return List containing:
#'   - success: logical indicating if save succeeded
#'   - filepath: full path to saved file (if successful)
#'   - filename: just the filename (if successful)
#'   - message: status message
#'
#' @export
export_report_excel <- function(metrics_data, report_name = NULL, dataset_name = "Unknown") {
  
  cat("\n[EXCEL_EXPORT] Starting Excel export\n")
  
  # Validate inputs
  if (is.null(metrics_data) || nrow(metrics_data) == 0) {
    return(list(
      success = FALSE,
      message = "No data to export"
    ))
  }
  
  # Check if writexl is available
  if (!requireNamespace("writexl", quietly = TRUE)) {
    return(list(
      success = FALSE,
      message = "writexl package is required for Excel export"
    ))
  }
  
  # Create reports directory if it doesn't exist
  reports_dir <- "reports"
  if (!dir.exists(reports_dir)) {
    dir.create(reports_dir, recursive = TRUE)
    cat(sprintf("[EXCEL_EXPORT] Created directory: %s\n", reports_dir))
  }
  
  # Generate filename
  if (is.null(report_name) || nchar(trimws(report_name)) == 0) {
    # Auto-generate name with timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- sprintf("Report_%s.xlsx", timestamp)
    cat("[EXCEL_EXPORT] Using auto-generated filename\n")
  } else {
    # Use provided name (sanitize it)
    clean_name <- gsub("[^A-Za-z0-9_-]", "_", report_name)
    filename <- sprintf("%s.xlsx", clean_name)
    cat(sprintf("[EXCEL_EXPORT] Using custom filename: %s\n", filename))
  }
  
  filepath <- file.path(reports_dir, filename)
  
  # Check if file already exists
  if (file.exists(filepath)) {
    # Add timestamp to make unique
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base_name <- tools::file_path_sans_ext(filename)
    filename <- sprintf("%s_%s.xlsx", base_name, timestamp)
    filepath <- file.path(reports_dir, filename)
    cat(sprintf("[EXCEL_EXPORT] File exists, using unique name: %s\n", filename))
  }
  
  # Create metadata sheet
  metric_names <- setdiff(names(metrics_data), "Sample_ID")
  
  metadata_sheet <- data.frame(
    Property = c(
      "Report Generated",
      "Source Dataset",
      "Number of Samples",
      "Number of Metrics",
      "Metrics Included",
      "",
      "Metric List:"
    ),
    Value = c(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      dataset_name,
      as.character(nrow(metrics_data)),
      as.character(length(metric_names)),
      paste(metric_names, collapse = ", "),
      "",
      ""
    ),
    stringsAsFactors = FALSE
  )
  
  # Add individual metric names to metadata
  for (i in seq_along(metric_names)) {
    metadata_sheet <- rbind(
      metadata_sheet,
      data.frame(
        Property = sprintf("  %d.", i),
        Value = metric_names[i],
        stringsAsFactors = FALSE
      )
    )
  }
  
  cat(sprintf("[EXCEL_EXPORT] Created metadata with %d entries\n", nrow(metadata_sheet)))
  
  # Write Excel file with multiple sheets
  tryCatch({
    writexl::write_xlsx(
      list(
        Metrics = metrics_data,
        Metadata = metadata_sheet
      ),
      path = filepath
    )
    
    cat(sprintf("[EXCEL_EXPORT] ✅ Successfully saved to: %s\n", filepath))
    cat(sprintf("[EXCEL_EXPORT] Sheet 1 (Metrics): %d samples x %d metrics\n", 
                nrow(metrics_data), 
                length(metric_names)))
    cat(sprintf("[EXCEL_EXPORT] Sheet 2 (Metadata): %d rows\n", nrow(metadata_sheet)))
    
    return(list(
      success = TRUE,
      filepath = filepath,
      filename = filename,
      message = sprintf("Report saved: %s", filename)
    ))
    
  }, error = function(e) {
    cat(sprintf("[EXCEL_EXPORT] ❌ Error: %s\n", e$message))
    return(list(
      success = FALSE,
      message = sprintf("Error saving Excel: %s", e$message)
    ))
  })
}