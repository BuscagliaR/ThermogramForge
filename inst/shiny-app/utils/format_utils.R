# ==============================================================================
# Format Utilities for ThermogramForge
# ==============================================================================
#
# FILE: format_utils.R
# VERSION: 1.0.0
# AUTHOR: Chris Reger
# LAST UPDATED: December 9, 2024
#
# ==============================================================================
# PURPOSE
# ==============================================================================
#
# Data format detection, validation, and conversion utilities. Handles:
#
#   - Detecting thermogram data structure (single/multi-sample, wide/long)
#   - Validating data quality and completeness
#   - Converting between data formats
#   - Preparing data for tlbparam package
#
# ==============================================================================
# FUNCTIONS
# ==============================================================================
#
# Format Detection:
#   - detect_data_format()         Identify data structure from raw data
#
# Validation:
#   - validate_thermogram_data()   Check data quality and completeness
#   - validate_csv_format()        Validate CSV-specific format requirements
#   - return_validation_result()   (internal) Format validation results
#
# Conversion:
#   - convert_to_tlbparam_format() Convert to tlbparam-compatible wide format
#
# ==============================================================================
# DATA FORMATS SUPPORTED
# ==============================================================================
#
# 1. SINGLE SAMPLE: Temperature, dCp columns
# 2. MULTI-SAMPLE LONG: Sample_ID, Temperature, dCp columns  
# 3. MULTI-SAMPLE WIDE: T1a, 1a, T1b, 1b column pairs
#
# See file header documentation in processing_utils.R for detailed examples.
#
# ==============================================================================
# DEPENDENCIES
# ==============================================================================
#
# Required packages:
#   - dplyr: Data manipulation
#
# ==============================================================================


# ==============================================================================
# DATA FORMAT DETECTION
# ==============================================================================
#
# Analyzes raw data to determine its structure (single sample, multi-sample
# long format, or multi-sample wide format). Returns metadata about columns,
# sample counts, and data organization.
#
# ==============================================================================


#' Detect Thermogram Data Format
#'
#' @description
#' Analyzes data frame structure to determine which thermogram format is used.
#' Supports multiple formats commonly used in thermal analysis.
#'
#' @param data Data frame to analyze
#'
#' @return List with format information containing n_samples, format_type, etc.
#'
#' @details
#' See detect_data_format implementation for full details on supported formats.
#'
#' @export
detect_data_format <- function(data) {
  
  # Get column names
  cols <- colnames(data)
  cols_lower <- tolower(cols)
  
  # Check for temperature column
  has_temp_col <- any(grepl("^temp", cols_lower))
  
  # Check for dCp column
  has_dcp_col <- any(grepl("^dcp|^d_cp|^delta_cp", cols_lower))
  
  if (has_temp_col && has_dcp_col) {
    # Standard format detected
    
    temp_col <- cols[grepl("^temp", cols_lower)][1]
    dcp_col <- cols[grepl("^dcp|^d_cp|^delta_cp", cols_lower)][1]
    
    # Check for sample ID column
    has_sample_id <- any(grepl("sample.*id|sample.*name|^id$|^sample$", cols_lower))
    
    if (has_sample_id) {
      sample_col_idx <- which(grepl("sample.*id|sample.*name|^id$|^sample$", cols_lower))[1]
      sample_col <- cols[sample_col_idx]
      n_samples <- length(unique(data[[sample_col]]))
      format_type <- "multi_sample_long"
    } else {
      sample_col <- NULL
      n_samples <- 1
      format_type <- "single_sample"
    }
    
    return(list(
      format_type = format_type,
      n_samples = n_samples,
      n_samples_empty = 0,
      n_rows = nrow(data),
      temperature_col = temp_col,
      dcp_col = dcp_col,
      sample_id_col = sample_col,
      data_points = rep(nrow(data), n_samples),
      all_columns = cols,
      data_format = "standard"
    ))
  }
  
  # Check for wide format
  temp_cols <- cols[grepl("^T[0-9]", cols)]
  
  if (length(temp_cols) > 0) {
    sample_ids <- gsub("^T", "", temp_cols)
    dcp_cols <- sample_ids[sample_ids %in% cols]
    
    if (length(dcp_cols) > 0) {
      valid_samples <- logical(length(dcp_cols))
      data_points <- integer(length(dcp_cols))
      
      for (i in seq_along(dcp_cols)) {
        dcp_data <- data[[dcp_cols[i]]]
        non_na_count <- sum(!is.na(dcp_data))
        
        valid_samples[i] <- non_na_count > 0
        data_points[i] <- non_na_count
      }
      
      valid_idx <- which(valid_samples)
      
      if (length(valid_idx) == 0) {
        stop("No samples with valid data found in file")
      }
      
      n_samples_total <- length(dcp_cols)
      n_samples_valid <- length(valid_idx)
      n_samples_empty <- n_samples_total - n_samples_valid
      
      return(list(
        format_type = "multi_sample_wide",
        n_samples = n_samples_valid,
        n_samples_empty = n_samples_empty,
        n_rows = nrow(data),
        temperature_cols = temp_cols[valid_idx],
        dcp_cols = dcp_cols[valid_idx],
        sample_ids = sample_ids[valid_idx],
        empty_sample_ids = if (n_samples_empty > 0) sample_ids[!valid_samples] else NULL,
        data_points = data_points[valid_idx],
        all_columns = cols,
        data_format = "wide"
      ))
    }
  }
  
  stop(
    "Required columns not found. Data must be in one of these formats:\n\n",
    "1. Standard Format (Single Sample):\n",
    "   Columns: Temperature, dCp\n\n",
    "2. Standard Format (Multi-Sample Long):\n",
    "   Columns: Sample_ID, Temperature, dCp\n\n",
    "3. Wide Format (Multi-Sample):\n",
    "   Columns: T1a, 1a, T1b, 1b, ... (paired temperature and dCp columns)\n\n",
    "Found columns: ", paste(cols, collapse = ", ")
  )
}


# ==============================================================================

# ==============================================================================
# DATA VALIDATION
# ==============================================================================
#
# Quality checks for thermogram data. Validates temperature ranges, data
# completeness, sample counts, and identifies potential issues.
#
# ==============================================================================


#' Validate Thermogram Data Quality
#'
#' @description
#' Checks thermogram data for common issues and data quality problems.
#'
#' @param data Data frame containing thermogram data
#' @param format_info List with format metadata from detect_data_format()
#'
#' @return List with valid (logical), errors (character vector), warnings (character vector)
#'
#' @export
validate_thermogram_data <- function(data, format_info) {
  
  errors <- character()
  warnings <- character()
  
  # Check minimum data points
  if (format_info$n_rows < 10) {
    errors <- c(
      errors,
      sprintf(
        "Too few data points: %d (minimum 10 required)",
        format_info$n_rows
      )
    )
  }
  
  # Format-specific validation
  if (format_info$data_format == "standard") {
    temp_col <- format_info$temperature_col
    dcp_col <- format_info$dcp_col
    
    temp_na <- sum(is.na(data[[temp_col]]))
    if (temp_na > 0) {
      errors <- c(
        errors,
        sprintf("%d missing temperature values", temp_na)
      )
    }
    
    dcp_na <- sum(is.na(data[[dcp_col]]))
    if (dcp_na > 0) {
      warnings <- c(
        warnings,
        sprintf("%d missing dCp values", dcp_na)
      )
    }
    
    temp_range <- range(data[[temp_col]], na.rm = TRUE)
    if (temp_range[1] < 0 || temp_range[2] > 150) {
      warnings <- c(
        warnings,
        sprintf(
          "Unusual temperature range: %.1f to %.1fC",
          temp_range[1],
          temp_range[2]
        )
      )
    }
  }
  
  # Sample count limit
  if (format_info$n_samples > 1000) {
    errors <- c(
      errors,
      sprintf(
        "Too many samples: %d (maximum 1000)",
        format_info$n_samples
      )
    )
  }
  
  list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  )
}


# ==============================================================================

# ==============================================================================
# TLBPARAM FORMAT CONVERSION
# ==============================================================================
#
# Converts processed thermogram data to the wide format required by the
# tlbparam package for metric calculations.
#
# ==============================================================================


#' Convert Processed Data to tlbparam Wide Format
#'
#' @description
#' Converts ThermogramForge processed_data to tlbparam::clean_thermograms() format.
#'
#' @param processed_data List containing samples with baseline-subtracted data
#'
#' @return Data frame with SampleCode + temperature columns (T45 to T90)
#'
#' @export
convert_to_tlbparam_format <- function(processed_data) {
  
  cat("\n[FORMAT] Converting to tlbparam wide format\n")
  
  if (!is.list(processed_data) || !"samples" %in% names(processed_data)) {
    stop("processed_data must be a list with 'samples' element")
  }
  
  samples <- processed_data$samples
  
  if (length(samples) == 0) {
    stop("No samples found in processed_data$samples")
  }
  
  cat(sprintf("[FORMAT] Processing %d samples\n", length(samples)))
  
  # Define standard temperature grid
  temp_grid <- seq(45, 90, by = 0.1)
  n_temps <- length(temp_grid)
  
  cat(sprintf(
    "[FORMAT] Temperature grid: %d points from %.1f to %.1fC\n",
    n_temps,
    min(temp_grid),
    max(temp_grid)
  ))
  
  # Create column names (T45, T45.1, ..., T90)
  temp_col_names <- sapply(temp_grid, function(temp) {
    if (temp == round(temp)) {
      sprintf("T%d", as.integer(temp))
    } else {
      sprintf("T%.1f", temp)
    }
  })
  
  cat(sprintf(
    "[FORMAT] Column names: %s ... %s\n",
    temp_col_names[1],
    tail(temp_col_names, 1)
  ))
  
  # Process each sample
  sample_rows <- list()
  
  for (sample_id in names(samples)) {
    sample <- samples[[sample_id]]
    
    # Skip failed or excluded samples
    if ("success" %in% names(sample) && !isTRUE(sample$success)) {
      cat(sprintf("[FORMAT] Skipping failed sample: %s\n", sample_id))
      next
    }
    
    if ("excluded" %in% names(sample) && isTRUE(sample$excluded)) {
      cat(sprintf("[FORMAT] Skipping excluded sample: %s\n", sample_id))
      next
    }
    
    # Extract temperature and dCp data (handle both formats)
    temperature <- NULL
    dcp_subtracted <- NULL
    
    # Format 1: App production format
    if ("temperature" %in% names(sample) && "baseline_subtracted" %in% names(sample)) {
      temperature <- as.numeric(sample$temperature)
      dcp_subtracted <- as.numeric(sample$baseline_subtracted)
      
      # Format 2: Test format
    } else if ("data" %in% names(sample)) {
      sample_data <- sample$data
      
      if ("Temperature" %in% names(sample_data) && "dCp_subtracted" %in% names(sample_data)) {
        temperature <- as.numeric(sample_data$Temperature)
        dcp_subtracted <- as.numeric(sample_data$dCp_subtracted)
      }
    }
    
    # Validate extraction
    if (is.null(temperature) || is.null(dcp_subtracted)) {
      cat(sprintf(
        "[FORMAT] ❌ Sample %s: Cannot find temperature or dCp data\n",
        sample_id
      ))
      next
    }
    
    # Clean data
    valid_idx <- !is.na(temperature) & !is.na(dcp_subtracted)
    temperature <- temperature[valid_idx]
    dcp_subtracted <- dcp_subtracted[valid_idx]
    
    # Check sufficient data
    if (length(temperature) < 10) {
      cat(sprintf(
        "[FORMAT] Insufficient data for sample %s (%d points)\n",
        sample_id,
        length(temperature)
      ))
      next
    }
    
    # Interpolate to standard grid
    interp_result <- approx(
      x = temperature,
      y = dcp_subtracted,
      xout = temp_grid,
      method = "linear",
      rule = 2
    )
    
    dcp_on_grid <- interp_result$y
    
    # Handle NAs
    if (any(is.na(dcp_on_grid))) {
      n_na <- sum(is.na(dcp_on_grid))
      cat(sprintf(
        "[FORMAT] Warning: %d NA values after interpolation for %s\n",
        n_na,
        sample_id
      ))
      dcp_on_grid[is.na(dcp_on_grid)] <- 0
    }
    
    # Create row
    sample_row <- c(
      SampleCode = sample_id,
      setNames(dcp_on_grid, temp_col_names)
    )
    
    sample_rows[[sample_id]] <- sample_row
    
    cat(sprintf(
      "[FORMAT] [OK] Sample %s: interpolated to %d points\n",
      sample_id,
      length(dcp_on_grid)
    ))
  }
  
  # Validate results
  if (length(sample_rows) == 0) {
    stop("No valid samples could be converted to tlbparam format")
  }
  
  # Combine into data frame
  result_df <- do.call(rbind, lapply(sample_rows, function(row) {
    as.data.frame(t(row), stringsAsFactors = FALSE)
  }))
  
  # Ensure temperature columns are numeric
  for (col in temp_col_names) {
    result_df[[col]] <- as.numeric(result_df[[col]])
  }
  
  cat(sprintf(
    "\n[FORMAT] [OK] Conversion complete: %d samples x %d columns\n",
    nrow(result_df),
    ncol(result_df)
  ))
  cat(sprintf(
    "[FORMAT] Structure: SampleCode + %d temperature columns\n",
    ncol(result_df) - 1
  ))
  
  return(result_df)
}

# ==============================================================================

# ==============================================================================
# CSV FORMAT VALIDATION
# ==============================================================================
#
# Provides detailed validation for CSV files with user-friendly error messages.
#
# ==============================================================================


#' Validate CSV Format with Helpful Feedback
#'
#' @description
#' Provides comprehensive validation for CSV files with specific error messages
#' to help users fix format issues.
#'
#' @param raw_data Data frame from CSV file
#' @param format_info List from detect_data_format() with format metadata
#'
#' @return List with:
#'   \itemize{
#'     \item valid: Logical - TRUE if file is valid
#'     \item errors: Character vector of critical issues
#'     \item warnings: Character vector of non-critical issues
#'     \item suggestions: Character vector of actionable suggestions
#'     \item message: HTML-formatted message for user display
#'   }
#'
#' @export
validate_csv_format <- function(raw_data, format_info) {
  
  errors <- character()
  warnings <- character()
  suggestions <- character()
  
  cat("\n[VALIDATION] ========================================\n")
  cat("[VALIDATION] CSV Format Validation\n")
  cat("[VALIDATION] ========================================\n")
  
  # Check basic structure
  if (!is.data.frame(raw_data) || nrow(raw_data) == 0) {
    errors <- c(errors, "File is empty or not a valid CSV")
    suggestions <- c(suggestions,
                     "• Ensure CSV file contains data rows",
                     "• Check that Temperature and dCp columns are present")
    return_validation_result(errors, warnings, suggestions)
  }
  
  cat("[VALIDATION] Data shape: ", nrow(raw_data), "rows ×", ncol(raw_data), "columns\n")
  
  # Check by format type
  if (format_info$data_format == "standard") {
    
    temp_col <- format_info$temperature_col
    dcp_col <- format_info$dcp_col
    
    if (!(temp_col %in% names(raw_data))) {
      errors <- c(errors,
                  sprintf("Temperature column '%s' not found", temp_col))
      suggestions <- c(suggestions,
                       "• Rename your temperature column to 'Temperature' or 'Temp'")
    }
    
    if (!(dcp_col %in% names(raw_data))) {
      errors <- c(errors,
                  sprintf("dCp column '%s' not found", dcp_col))
      suggestions <- c(suggestions,
                       "• Rename your dCp column to 'dCp' or 'Heat Flow'")
    }
    
    # Check data types
    if (temp_col %in% names(raw_data)) {
      if (!is.numeric(raw_data[[temp_col]])) {
        errors <- c(errors,
                    sprintf("Temperature column is not numeric (found: %s)",
                            class(raw_data[[temp_col]])[1]))
        suggestions <- c(suggestions,
                         "• Ensure all temperature values are numbers")
      }
    }
    
    if (dcp_col %in% names(raw_data)) {
      if (!is.numeric(raw_data[[dcp_col]])) {
        warnings <- c(warnings,
                      sprintf("dCp column is not numeric (found: %s)",
                              class(raw_data[[dcp_col]])[1]))
        suggestions <- c(suggestions,
                         "• Ensure all dCp values are numbers (will attempt conversion)")
      }
    }
    
    cat("[VALIDATION] Format: Single sample\n")
    
  } else if (format_info$data_format == "multi_sample_long") {
    
    if (!("Sample_ID" %in% names(raw_data))) {
      errors <- c(errors,
                  "Sample_ID column not found in multi-sample format")
      suggestions <- c(suggestions,
                       "• Add a 'Sample_ID' column identifying each sample",
                       "• Example: 'Sample1', 'Sample2', 'T1a', '1a'")
    } else {
      n_unique <- length(unique(raw_data$Sample_ID))
      cat(sprintf("[VALIDATION] Found %d unique samples\n", n_unique))
      
      if (n_unique < 2) {
        warnings <- c(warnings,
                      sprintf("Only %d unique sample detected", n_unique))
      }
    }
    
    cat("[VALIDATION] Format: Multi-sample (long)\n")
    
  } else if (format_info$data_format == "multi_sample_wide") {
    
    cat(sprintf("[VALIDATION] Format: Multi-sample (wide): %d samples detected\n",
                format_info$n_samples))
    
    for (id in format_info$sample_ids) {
      temp_col <- sprintf("T%s", id)
      dcp_col <- id
      
      if (!(temp_col %in% names(raw_data))) {
        errors <- c(errors,
                    sprintf("Temperature column 'T%s' not found for sample '%s'",
                            id, id))
      }
      
      if (!(dcp_col %in% names(raw_data))) {
        errors <- c(errors,
                    sprintf("dCp column '%s' not found for sample '%s'",
                            id, id))
      }
    }
    
    suggestions <- c(suggestions,
                     "• Wide format requires paired columns: T[SampleID] and [SampleID]",
                     "• Example: T1a, 1a (temperature and dCp for sample 1a)")
  }
  
  # Check data quality
  if (nrow(raw_data) < 10) {
    warnings <- c(warnings,
                  sprintf("File has only %d data points (minimum 10 recommended)",
                          nrow(raw_data)))
    suggestions <- c(suggestions,
                     "• Thermograms with fewer than 10 points may not show clear peaks")
  }
  
  if (nrow(raw_data) > 10000) {
    warnings <- c(warnings,
                  sprintf("File has %d data points (very large)", nrow(raw_data)))
    suggestions <- c(suggestions,
                     "• Processing may be slow for very large files")
  }
  
  cat(sprintf("[VALIDATION] Errors: %d | Warnings: %d | Suggestions: %d\n",
              length(errors), length(warnings), length(suggestions)))
  
  return_validation_result(errors, warnings, suggestions)
}


#' Format Validation Results for Display
#'
#' @keywords internal
#' @noRd
return_validation_result <- function(errors, warnings, suggestions) {
  
  valid <- length(errors) == 0
  
  # Build HTML message for user
  message_parts <- character()
  
  if (!valid) {
    message_parts <- c(message_parts,
                       "<div class='alert alert-danger'>",
                       "<strong>❌ Validation Errors:</strong>",
                       "<ul>",
                       paste(sprintf("<li>%s</li>", errors), collapse = ""),
                       "</ul>",
                       "</div>")
  }
  
  if (length(warnings) > 0) {
    message_parts <- c(message_parts,
                       "<div class='alert alert-warning'>",
                       "<strong>⚠ Warnings:</strong>",
                       "<ul>",
                       paste(sprintf("<li>%s</li>", warnings), collapse = ""),
                       "</ul>",
                       "</div>")
  }
  
  if (length(suggestions) > 0 && (length(errors) > 0 || length(warnings) > 0)) {
    message_parts <- c(message_parts,
                       "<div class='alert alert-info'>",
                       "<strong>💡 Suggestions:</strong>",
                       "<ul>",
                       paste(sprintf("<li>%s</li>", suggestions), collapse = ""),
                       "</ul>",
                       "</div>")
  }
  
  list(
    valid = valid,
    errors = errors,
    warnings = warnings,
    suggestions = suggestions,
    message = HTML(paste(message_parts, collapse = ""))
  )
}