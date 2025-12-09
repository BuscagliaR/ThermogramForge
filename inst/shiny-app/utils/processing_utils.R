# ==============================================================================
# Processing Utilities for ThermogramForge
# ==============================================================================
#
# FILE: processing_utils.R
# VERSION: 2.0.0
# AUTHOR: Chris Reger
# LAST UPDATED: December 9, 2024
#
# ==============================================================================
# PURPOSE
# ==============================================================================
#
# Core data processing pipeline for ThermogramForge. This file provides all
# functions needed to:
#
#   - Read and parse thermogram data files (CSV and Excel)
#   - Detect and validate data formats
#   - Apply baseline detection using ThermogramBaseline package
#   - Perform signal detection for quality assessment
#   - Calculate thermogram metrics using tlbparam package
#   - Save and load processed datasets
#
# This is the primary utility file for the application's data processing
# workflow, handling everything from raw file upload to final metric
# calculation.
#
# ==============================================================================
# ARCHITECTURE
# ==============================================================================
#
# The processing pipeline follows this flow:
#
#   1. FILE READING
#      detect_file_type() -> read_thermogram_file()
#      Reads CSV or Excel files, detects format, applies temp filtering
#
#   2. FORMAT DETECTION & VALIDATION
#      detect_data_format() -> validate_thermogram_data()
#      Identifies data structure and checks for quality issues
#
#   3. BASELINE PROCESSING
#      process_thermogram_data() -> apply_baseline_detection()
#      Runs ThermogramBaseline algorithm to find baseline endpoints
#
#   4. SIGNAL DETECTION
#      apply_signal_detection()
#      Determines if sample has detectable thermal signature
#
#   5. METRIC CALCULATION
#      calculate_tlbparam_metrics() -> convert_to_tlbparam_format()
#      Computes thermogram metrics using tlbparam package
#
#   6. PERSISTENCE
#      save_processed_data() / load_processed_data()
#      Stores and retrieves processed datasets
#
# ==============================================================================
# SECTIONS
# ==============================================================================
#
# This file is organized into the following sections:
#
#   Line ~70:   FILE TYPE DETECTION
#               detect_file_type()
#
#   Line ~140:  FILE READING AND PARSING
#               read_thermogram_file()
#
#   Line ~270:  DATA FORMAT DETECTION
#               detect_data_format()
#
#   Line ~390:  DATA VALIDATION
#               validate_thermogram_data()
#
#   Line ~470:  TLBPARAM FORMAT CONVERSION
#               convert_to_tlbparam_format()
#
#   Line ~680:  BASELINE DETECTION AND PROCESSING
#               process_thermogram_data()
#
#   Line ~850:  HELPER FUNCTIONS (Internal)
#               extract_sample_data(), apply_baseline_detection(),
#               apply_signal_detection()
#
#   Line ~1000: FILE SAVE/LOAD OPERATIONS
#               save_processed_data(), load_processed_data(),
#               list_processed_datasets(), delete_processed_data()
#
#   Line ~1540: TLBPARAM METRIC CALCULATION
#               calculate_tlbparam_metrics()
#
#   Line ~1670: CSV FORMAT VALIDATION
#               validate_csv_format()
#
# ==============================================================================
# EXPORTED FUNCTIONS
# ==============================================================================
#
# File Operations:
#   - detect_file_type()           Determine if file is CSV or Excel
#   - read_thermogram_file()       Read and parse thermogram data with options
#
# Format Detection:
#   - detect_data_format()         Identify data structure (single/multi/wide)
#   - validate_thermogram_data()   Check data quality and completeness
#   - validate_csv_format()        Validate CSV-specific format requirements
#
# Processing:
#   - process_thermogram_data()    Run full baseline detection pipeline
#   - convert_to_tlbparam_format() Convert to tlbparam-compatible wide format
#   - calculate_tlbparam_metrics() Calculate metrics using tlbparam package
#
# Persistence:
#   - save_processed_data()        Save dataset to RDS/CSV/Excel
#   - load_processed_data()        Load previously saved dataset
#   - list_processed_datasets()    List all saved datasets in directory
#   - delete_processed_data()      Remove saved dataset files
#
# ==============================================================================
# DATA FORMATS SUPPORTED
# ==============================================================================
#
# ThermogramForge supports three thermogram data formats:
#
# 1. SINGLE SAMPLE (Standard Format)
#    Columns: Temperature, dCp
#    One thermogram per file with temperature and heat capacity columns.
#
#    Example:
#    | Temperature | dCp      |
#    |-------------|----------|
#    | 45.0        | 0.0012   |
#    | 45.5        | 0.0015   |
#
# 2. MULTI-SAMPLE LONG FORMAT
#    Columns: Sample_ID, Temperature, dCp
#    Multiple samples stacked vertically with identifier column.
#
#    Example:
#    | Sample_ID | Temperature | dCp      |
#    |-----------|-------------|----------|
#    | Sample_A  | 45.0        | 0.0012   |
#    | Sample_A  | 45.5        | 0.0015   |
#    | Sample_B  | 45.0        | 0.0018   |
#
# 3. MULTI-SAMPLE WIDE FORMAT
#    Columns: T1a, 1a, T1b, 1b, T2a, 2a, ...
#    Paired temperature and dCp columns for each sample.
#    Temperature columns prefixed with 'T', dCp columns are sample IDs.
#
#    Example:
#    | T1a   | 1a      | T1b   | 1b      |
#    |-------|---------|-------|---------|
#    | 45.0  | 0.0012  | 45.0  | 0.0018  |
#    | 45.5  | 0.0015  | 45.5  | 0.0021  |
#
# ==============================================================================
# DEPENDENCIES
# ==============================================================================
#
# Required packages:
#   - readr:              CSV file reading (read_csv, write_csv)
#   - readxl:             Excel file reading (read_excel, excel_sheets)
#   - writexl:            Excel file writing (write_xlsx)
#   - dplyr:              Data manipulation (filter, mutate, select)
#   - ThermogramBaseline: Baseline detection algorithm (from BuscagliaR)
#   - tlbparam:           Thermogram metric calculations (from BuscagliaR)
#   - forecast:           Time series utilities (used by ThermogramBaseline)
#
# Note: ThermogramBaseline and tlbparam are custom packages from the
# BuscagliaR GitHub repositories, not CRAN packages.
#
# ==============================================================================
# USAGE EXAMPLES
# ==============================================================================
#
# Basic file reading:
#
#   result <- read_thermogram_file("data.csv")
#   result <- read_thermogram_file("data.xlsx", sheet = "Sheet1")
#   result <- read_thermogram_file("data.csv", temp_min = 45, temp_max = 90)
#
# Full processing pipeline:
#
#   # Read file
#   file_result <- read_thermogram_file("thermograms.xlsx", sheet = "Data")
#
#   # Validate
#   validation <- validate_thermogram_data(file_result$data, file_result$format_info)
#
#   # Process with baseline detection
#   processed <- process_thermogram_data(
#     data = file_result$data,
#     format_info = file_result$format_info,
#     temp_params = list(temp_min = 45, temp_max = 90)
#   )
#
#   # Calculate metrics
#   metrics <- calculate_tlbparam_metrics(processed, selected_metrics = c("Tm", "AUC"))
#
#   # Save results
#   save_processed_data(processed, "my_dataset", format = "rds")
#
# ==============================================================================


# ==============================================================================
# FILE TYPE DETECTION
# ==============================================================================

#' Detect File Type from Extension
#'
#' @description
#' Determines whether uploaded file is CSV or Excel based on file extension.
#'
#' @param file_path Character. Path to the uploaded file
#'
#' @return Character. Either "csv" or "excel"
#'
#' @details
#' Supported extensions:
#' - CSV: .csv
#' - Excel: .xlsx, .xls
#'
#' @examples
#' \dontrun{
#' detect_file_type("data.csv")    # Returns "csv"
#' detect_file_type("data.xlsx")   # Returns "excel"
#' }
#'
#' @export
detect_file_type <- function(file_path) {
  
  # Extract file extension and convert to lowercase
  ext <- tolower(tools::file_ext(file_path))
  
  # Determine type based on extension
  if (ext == "csv") {
    return("csv")
  } else if (ext %in% c("xlsx", "xls")) {
    return("excel")
  } else {
    stop(
      "Unsupported file type: .", ext, "\n",
      "Supported formats: .csv, .xlsx, .xls"
    )
  }
}


# ==============================================================================
# FILE READING AND PARSING
# ==============================================================================
#
# Functions for reading thermogram data from CSV and Excel files.
# Handles sheet selection for Excel files and optional temperature filtering.
#
# ==============================================================================

#' Read Thermogram Data File
#'
#' @description
#' Reads thermogram data from CSV or Excel file and returns a structured list
#' containing the raw data, format information, and validation results.
#'
#' @param filepath Character. Path to the file to read
#' @param temp_min Numeric or NULL. Minimum temperature to include (C).
#'   If NULL, no lower filtering is applied. Default: NULL
#' @param temp_max Numeric or NULL. Maximum temperature to include (C).
#'   If NULL, no upper filtering is applied. Default: NULL
#' @param sheet Character or NULL. For Excel files, the name of the sheet to read.
#'   If NULL, reads the first sheet. Ignored for CSV files. Default: NULL
#'
#' @return List containing:
#'   \itemize{
#'     \item data: Data frame with thermogram data
#'     \item format_info: List with format metadata (from detect_data_format)
#'   }
#'
#' @details
#' Temperature filtering (if specified):
#' - Applied AFTER initial file reading
#' - Filters out rows where Temperature is outside [temp_min, temp_max]
#' - Useful for removing noisy data at temperature extremes
#' - Applied independently to each sample in multi-sample files
#'
#' Excel sheet selection:
#' - For Excel files with multiple sheets, use the `sheet` parameter
#' - Sheet names are case-sensitive
#' - If sheet parameter is NULL, reads the first sheet
#' - Use readxl::excel_sheets(filepath) to list available sheets
#'
#' Process:
#' 1. Detect file type (CSV or Excel)
#' 2. Read file using appropriate reader (with sheet selection for Excel)
#' 3. Detect data format structure
#' 4. Apply temperature filtering if requested
#' 5. Return structured data
#'
#' @examples
#' \dontrun{
#' # Read entire file (CSV)
#' result <- read_thermogram_file("thermogram.csv")
#'
#' # Read specific Excel sheet
#' result <- read_thermogram_file("thermogram.xlsx", sheet = "Data")
#'
#' # Read with temperature filtering
#' result <- read_thermogram_file(
#'   "thermogram.csv",
#'   temp_min = 20,
#'   temp_max = 110
#' )
#'
#' # Read specific sheet with filtering
#' result <- read_thermogram_file(
#'   "thermogram.xlsx",
#'   temp_min = 45,
#'   temp_max = 90,
#'   sheet = "Plasma Samples"
#' )
#' }
#'
#' @export
read_thermogram_file <- function(filepath, temp_min = NULL, temp_max = NULL, sheet = NULL) {
  
  # ============================================================================
  # Step 1: Detect and Read File
  # ============================================================================
  
  # Determine file type from extension
  file_type <- detect_file_type(filepath)
  
  # Read file based on type
  raw_data <- tryCatch({
    
    if (file_type == "csv") {
      # Read CSV file (sheet parameter ignored)
      readr::read_csv(filepath, show_col_types = FALSE)
      
    } else if (file_type == "excel") {
      # Read Excel file with sheet selection
      if (!is.null(sheet)) {
        # User specified a sheet
        cat(sprintf("[READ] Reading Excel sheet: '%s'\n", sheet))
        readxl::read_excel(filepath, sheet = sheet)
      } else {
        # No sheet specified, read first sheet
        readxl::read_excel(filepath)
      }
      
    } else {
      stop("Unexpected file type: ", file_type)
    }
    
  }, error = function(e) {
    stop("Error reading file: ", e$message)
  })
  
  # ============================================================================
  # Step 2: Detect Data Format
  # ============================================================================
  
  format_info <- detect_data_format(raw_data)
  
  # ============================================================================
  # Step 3: Apply Temperature Filtering (Optional)
  # ============================================================================
  
  # Only filter if temp_min or temp_max is specified
  if (!is.null(temp_min) || !is.null(temp_max)) {
    
    cat(sprintf(
      "[READ] Applying temperature filter: %.1f to %.1fC\n",
      ifelse(is.null(temp_min), -Inf, temp_min),
      ifelse(is.null(temp_max), Inf, temp_max)
    ))
    
    # Different filtering logic based on detected format
    if (format_info$format_type == "multi_sample_wide") {
      # Wide format: filter by setting values to NA (can't remove rows)
      
      for (i in seq_along(format_info$sample_ids)) {
        sample_id <- format_info$sample_ids[i]
        temp_col <- format_info$temperature_cols[i]  # Fixed: was temp_cols
        
        # Get temperature values for this sample
        temps <- raw_data[[temp_col]]
        
        # Create mask: keep non-NA temps within range
        keep_mask <- !is.na(temps)  # Start with non-NA
        
        if (!is.null(temp_min)) {
          keep_mask <- keep_mask & (temps >= temp_min)
        }
        
        if (!is.null(temp_max)) {
          keep_mask <- keep_mask & (temps <= temp_max)
        }
        
        # Apply filter by setting filtered values to NA
        # (Can't remove rows because multiple samples share rows)
        raw_data[[temp_col]][!keep_mask] <- NA
        
        # Also set corresponding dCp values to NA
        dcp_col <- format_info$dcp_cols[i]
        raw_data[[dcp_col]][!keep_mask] <- NA
        
        n_kept <- sum(keep_mask, na.rm = TRUE)
        n_total <- sum(!is.na(temps))
        
        cat(sprintf(
          "[READ] Sample %s: kept %d/%d points\n",
          sample_id,
          n_kept,
          n_total
        ))
      }
    }
    
    # Re-detect format after filtering (sample counts may have changed)
    format_info <- detect_data_format(raw_data)
  }
  
  # ============================================================================
  # Step 4: Return Results
  # ============================================================================
  
  return(list(
    data = raw_data,
    format_info = format_info
  ))
}


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
# DATA VALIDATION
# ==============================================================================
#
# Quality checks for thermogram data. Validates temperature ranges, data
# completeness, sample counts, and identifies potential issues that could
# affect processing accuracy.
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
# TLBPARAM FORMAT CONVERSION
# ==============================================================================
#
# Converts processed thermogram data to the wide format required by the
# tlbparam package for metric calculations. Handles interpolation to common
# temperature grid and restructures data for tlbparam compatibility.
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
# BASELINE DETECTION AND PROCESSING
# ==============================================================================
#
# Core processing functions that apply the ThermogramBaseline algorithm to
# detect baseline endpoints and subtract baseline from thermogram data.
# Also includes signal detection for quality assessment.
#
# Key function: process_thermogram_data()
#   - Iterates through all samples in dataset
#   - Applies baseline detection using ThermogramBaseline package
#   - Performs signal quality assessment
#   - Returns comprehensive processed data structure
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
# HELPER FUNCTIONS (Internal, not exported)
# ==============================================================================
#
# Internal utility functions used by the main processing pipeline.
# These are not exported and should not be called directly.
#
# Functions:
#   - extract_sample_data()       Extract single sample from multi-sample data
#   - apply_baseline_detection()  Run ThermogramBaseline on one sample
#   - apply_signal_detection()    Check if sample has detectable signal
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

# ==============================================================================
# FILE SAVE/LOAD OPERATIONS
# ==============================================================================
#
# Persistence functions for saving and loading processed thermogram datasets.
# Supports multiple output formats (RDS, CSV, Excel) with metadata.
#
# Functions:
#   - save_processed_data()       Save dataset to file (RDS/CSV/Excel)
#   - load_processed_data()       Load previously saved dataset
#   - list_processed_datasets()   List all saved datasets in directory
#   - delete_processed_data()     Remove saved dataset files
#
# Internal helpers:
#   - format_data_for_wide_export()  Prepare data for CSV/Excel export
#   - save_metadata_file()           Write metadata JSON alongside data
#   - create_metadata_dataframe()    Build metadata summary table
#
# ==============================================================================

#' Save Processed Thermogram Data
#'
#' Saves processed thermogram data to disk in RDS, CSV, or Excel format.
#'
#' @param data List containing processed thermogram data with structure:
#'   \itemize{
#'     \item samples: Named list of processed samples
#'     \item n_samples: Total number of samples
#'     \item n_signal: Number of samples with signal
#'     \item n_no_signal: Number of samples without signal
#'     \item processing_params: List of processing parameters
#'     \item processing_time: Timestamp
#'   }
#' @param filename Base filename without extension
#' @param format Output format: "rds", "csv", or "xlsx"
#' @param output_dir Output directory (default: "data/processed")
#'
#' @return List with success status, message, and filepath
#'
#' @export
save_processed_data <- function(data, filename, format = "rds", output_dir = "data/processed") {
  
  # Validate inputs
  if (is.null(data) || !is.list(data)) {
    return(list(
      success = FALSE,
      message = "Invalid data structure",
      filepath = NULL
    ))
  }
  
  if (is.null(filename) || nchar(filename) == 0) {
    return(list(
      success = FALSE,
      message = "Filename cannot be empty",
      filepath = NULL
    ))
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat(sprintf("[SAVE] Created directory: %s\n", output_dir))
  }
  
  # Build full filepath
  format <- tolower(format)
  extension <- switch(format,
                      "rds" = ".rds",
                      "csv" = ".csv",
                      "xlsx" = ".xlsx",
                      ".rds"  # default
  )
  
  # Remove any existing extension from filename
  filename <- gsub("\\.(rds|csv|xlsx)$", "", filename, ignore.case = TRUE)
  
  filepath <- file.path(output_dir, paste0(filename, extension))
  
  cat(sprintf("[SAVE] Saving to: %s\n", filepath))
  
  # Save based on format
  tryCatch({
    
    if (format == "rds") {
      # RDS: Save complete R object (can be fully reloaded)
      saveRDS(data, file = filepath)
      cat(sprintf("[SAVE] [OK] Saved RDS: %d samples\n", data$n_samples))
      
    } else if (format == "csv") {
      # CSV: Wide format export
      wide_data <- format_data_for_wide_export(data)
      readr::write_csv(wide_data, filepath)
      cat(sprintf("[SAVE] [OK] Saved CSV: %d samples, %d columns\n", 
                  nrow(wide_data), ncol(wide_data)))
      
      # Save metadata as companion file
      metadata_path <- file.path(output_dir, paste0(filename, "_metadata.txt"))
      save_metadata_file(data, metadata_path)
      
    } else if (format == "xlsx") {
      # Excel: Wide format with metadata sheet
      wide_data <- format_data_for_wide_export(data)
      
      # Create workbook
      wb <- openxlsx::createWorkbook()
      
      # Add data sheet
      openxlsx::addWorksheet(wb, "Data")
      openxlsx::writeData(wb, "Data", wide_data)
      
      # Add metadata sheet
      metadata_df <- create_metadata_dataframe(data)
      openxlsx::addWorksheet(wb, "Metadata")
      openxlsx::writeData(wb, "Metadata", metadata_df)
      
      # Save workbook
      openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)
      cat(sprintf("[SAVE] [OK] Saved Excel: %d samples, 2 sheets\n", nrow(wide_data)))
      
    } else {
      return(list(
        success = FALSE,
        message = sprintf("Unsupported format: %s", format),
        filepath = NULL
      ))
    }
    
    # Return success
    return(list(
      success = TRUE,
      message = sprintf("Data saved successfully as %s", basename(filepath)),
      filepath = filepath
    ))
    
  }, error = function(e) {
    cat(sprintf("[SAVE] ERROR: %s\n", e$message))
    return(list(
      success = FALSE,
      message = sprintf("Save failed: %s", e$message),
      filepath = NULL
    ))
  })
}


#' Format Data for Wide Export (CSV/Excel)
#'
#' Converts processed thermogram data to wide format with temperature columns.
#'
#' @param data Processed thermogram data list
#'
#' @return Data frame in wide format
#'
#' @keywords internal
format_data_for_wide_export <- function(data) {
  
  if (is.null(data$samples) || length(data$samples) == 0) {
    stop("No samples to export")
  }
  
  # Get common temperature grid from first sample
  first_sample <- data$samples[[1]]
  temp_grid <- first_sample$temperature
  
  # Create column names: T45, T45.1, T45.2, etc.
  temp_cols <- paste0("T", format(temp_grid, nsmall = 1))
  
  # Initialize result data frame
  result_rows <- list()
  
  for (sample_id in names(data$samples)) {
    sample <- data$samples[[sample_id]]
    
    # Skip excluded samples
    if (isTRUE(sample$excluded)) {
      cat(sprintf("[EXPORT] Skipping excluded sample: %s\n", sample_id))
      next
    }
    
    # Create row with Sample_ID and baseline-subtracted dCp values
    row_data <- c(
      Sample_ID = sample_id,
      setNames(as.list(sample$baseline_subtracted), temp_cols)
    )
    
    result_rows[[sample_id]] <- as.data.frame(row_data, stringsAsFactors = FALSE)
  }
  
  # Combine all rows
  if (length(result_rows) == 0) {
    stop("No samples to export after filtering")
  }
  
  result_df <- do.call(rbind, result_rows)
  rownames(result_df) <- NULL
  
  # Ensure temperature columns are numeric
  for (col in temp_cols) {
    result_df[[col]] <- as.numeric(result_df[[col]])
  }
  
  cat(sprintf("[EXPORT] Formatted: %d samples x %d columns\n", 
              nrow(result_df), ncol(result_df)))
  
  return(result_df)
}


#' Save Metadata to Text File
#'
#' Creates a companion metadata file for CSV exports.
#'
#' @param data Processed thermogram data list
#' @param filepath Path to metadata file
#'
#' @keywords internal
save_metadata_file <- function(data, filepath) {
  
  metadata_lines <- c(
    "# ThermogramForge Processed Data Metadata",
    sprintf("# Generated: %s", Sys.time()),
    "",
    "## Processing Summary",
    sprintf("Total Samples: %d", data$n_samples),
    sprintf("Samples with Signal: %d", data$n_signal),
    sprintf("Samples without Signal: %d", data$n_no_signal),
    sprintf("Processing Time: %s", data$processing_time),
    "",
    "## Processing Parameters",
    sprintf("Temperature Range: [%.1f, %.1f]C", 
            data$processing_params$temp_min, 
            data$processing_params$temp_max),
    sprintf("Window Size: %d", data$processing_params$window_size),
    sprintf("Exclusion Zone: [%.1f, %.1f]C",
            data$processing_params$exclusion_lower,
            data$processing_params$exclusion_upper),
    sprintf("Grid Resolution: %.2fC", data$processing_params$grid_resolution),
    sprintf("Point Selection: %s", data$processing_params$point_selection),
    "",
    "## Sample Details"
  )
  
  # Add sample-specific info
  for (sample_id in names(data$samples)) {
    sample <- data$samples[[sample_id]]
    metadata_lines <- c(
      metadata_lines,
      sprintf("Sample %s:", sample_id),
      sprintf("  - Endpoints: [%.1f, %.1f]C", 
              sample$lower_endpoint, sample$upper_endpoint),
      sprintf("  - Manual Adjustment: %s", sample$manual_adjustment),
      sprintf("  - Has Signal: %s", sample$has_signal),
      sprintf("  - Excluded: %s", sample$excluded),
      sprintf("  - Reviewed: %s", sample$reviewed),
      ""
    )
  }
  
  writeLines(metadata_lines, filepath)
  cat(sprintf("[SAVE] [OK] Metadata saved: %s\n", basename(filepath)))
}


#' Create Metadata Data Frame for Excel
#'
#' Creates a data frame of metadata for Excel export.
#'
#' @param data Processed thermogram data list
#'
#' @return Data frame with metadata
#'
#' @keywords internal
create_metadata_dataframe <- function(data) {
  
  data.frame(
    Property = c(
      "Total Samples",
      "Samples with Signal",
      "Samples without Signal",
      "Processing Time",
      "Temperature Min (C)",
      "Temperature Max (C)",
      "Window Size",
      "Exclusion Lower (C)",
      "Exclusion Upper (C)",
      "Grid Resolution (C)",
      "Point Selection"
    ),
    Value = c(
      data$n_samples,
      data$n_signal,
      data$n_no_signal,
      as.character(data$processing_time),
      data$processing_params$temp_min,
      data$processing_params$temp_max,
      data$processing_params$window_size,
      data$processing_params$exclusion_lower,
      data$processing_params$exclusion_upper,
      data$processing_params$grid_resolution,
      data$processing_params$point_selection
    ),
    stringsAsFactors = FALSE
  )
}


#' Load Processed Thermogram Data
#'
#' Loads processed thermogram data from RDS, CSV, or Excel files.
#'
#' @param filepath Path to file to load
#'
#' @return List containing loaded data and metadata
#'
#' @export
load_processed_data <- function(filepath) {
  
  if (!file.exists(filepath)) {
    stop(sprintf("File not found: %s", filepath))
  }
  
  # Detect format
  ext <- tolower(tools::file_ext(filepath))
  
  cat(sprintf("[LOAD] Loading file: %s\n", basename(filepath)))
  
  tryCatch({
    
    if (ext == "rds") {
      # RDS: Full object restore
      data <- readRDS(filepath)
      
      # Verify structure
      if (!is.list(data) || is.null(data$samples)) {
        stop("Invalid RDS file structure")
      }
      
      cat(sprintf("[LOAD] RDS loaded: %d samples\n", length(data$samples)))
      
      return(list(
        data = data,
        data_type = "full",  # Can be used in Review Endpoints
        format = "rds"
      ))
      
    } else if (ext == "csv") {
      # CSV: Wide format (report-only)
      wide_data <- readr::read_csv(filepath, show_col_types = FALSE)
      
      cat(sprintf("[LOAD] CSV loaded: %d samples\n", nrow(wide_data)))
      
      return(list(
        data = wide_data,
        data_type = "report_only",  # Cannot repopulate Review Endpoints
        format = "csv"
      ))
      
    } else if (ext %in% c("xlsx", "xls")) {
      # Excel: Wide format with metadata
      wide_data <- readxl::read_excel(filepath, sheet = "Data")
      
      # Try to load metadata if it exists
      metadata <- tryCatch({
        readxl::read_excel(filepath, sheet = "Metadata")
      }, error = function(e) NULL)
      
      cat(sprintf("[LOAD] Excel loaded: %d samples\n", nrow(wide_data)))
      
      return(list(
        data = wide_data,
        metadata = metadata,
        data_type = "report_only",
        format = "xlsx"
      ))
      
    } else {
      stop(sprintf("Unsupported file format: .%s", ext))
    }
    
  }, error = function(e) {
    cat(sprintf("[LOAD] ERROR: %s\n", e$message))
    stop(e$message)
  })
}


#' List Processed Datasets
#'
#' Scans directory for processed data files and returns metadata.
#'
#' @param output_dir Directory to scan (default: "data/processed")
#'
#' @return Data frame with file information
#'
#' @export
list_processed_datasets <- function(output_dir = "data/processed") {
  
  if (!dir.exists(output_dir)) {
    return(data.frame(
      filename = character(0),
      format = character(0),
      size_mb = numeric(0),
      modified = character(0),
      can_load_full = logical(0)
    ))
  }
  
  # Get all data files
  files <- list.files(
    output_dir,
    pattern = "\\.(rds|csv|xlsx)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(files) == 0) {
    return(data.frame(
      filename = character(0),
      format = character(0),
      size_mb = numeric(0),
      modified = character(0),
      can_load_full = logical(0)
    ))
  }
  
  # Get file info
  file_info <- file.info(files)
  
  data.frame(
    filename = basename(files),
    format = toupper(tools::file_ext(files)),
    size_mb = round(file_info$size / 1024^2, 2),
    modified = format(file_info$mtime, "%Y-%m-%d %H:%M"),
    can_load_full = tolower(tools::file_ext(files)) == "rds",
    filepath = files,
    stringsAsFactors = FALSE
  )
}


#' Delete Processed Data File
#'
#' Safely deletes a processed data file and any companion files.
#'
#' @param filepath Path to file to delete
#'
#' @return Logical indicating success
#'
#' @export
delete_processed_data <- function(filepath) {
  
  if (!file.exists(filepath)) {
    cat(sprintf("[DELETE] File not found: %s\n", basename(filepath)))
    return(list(
      success = FALSE,
      message = sprintf("File not found: %s", basename(filepath))
    ))
  }
  
  tryCatch({
    
    # Delete main file
    file.remove(filepath)
    cat(sprintf("[DELETE] [OK] Deleted: %s\n", basename(filepath)))
    
    # Delete companion metadata file if it exists (for CSV)
    if (tolower(tools::file_ext(filepath)) == "csv") {
      base_path <- tools::file_path_sans_ext(filepath)
      metadata_path <- paste0(base_path, "_metadata.txt")
      
      if (file.exists(metadata_path)) {
        file.remove(metadata_path)
        cat(sprintf("[DELETE] [OK] Deleted metadata: %s\n", basename(metadata_path)))
      }
    }
    
    
    return(list(
      success = TRUE,
      message = sprintf("Successfully deleted: %s", basename(filepath))
    ))
    
  }, error = function(e) {
    cat(sprintf("[DELETE] ERROR: %s\n", e$message))
    return(list(
      success = FALSE,
      message = sprintf("Delete failed: %s", e$message)
    ))
  })
}

# ==============================================================================
# CALCULATE TLBPARAM METRICS
# ==============================================================================
#
# Integration with the tlbparam package for thermogram metric calculations.
# Converts processed data to tlbparam format and calculates user-selected
# metrics from the 24 available options across 6 categories.
#
# Metric Categories:
#   - Peak metrics (Peak 1-4, Peak Ratios)
#   - Area metrics (AUC, partial AUCs)
#   - Temperature metrics (Tm, TFM, TCM, Width)
#   - Shape metrics (Asymmetry, Kurtosis)
#   - Derivative metrics (First/Second derivatives)
#   - Curve fit metrics (Gaussian parameters)
#
# Key function: calculate_tlbparam_metrics()
#   - Validates processed data structure
#   - Maps UI-friendly names to tlbparam names
#   - Calls tlbparam::clean_thermograms()
#   - Returns metrics data frame
#
# ==============================================================================

#' Calculate Thermogram Metrics Using tlbparam
#'
#' @description
#' Converts processed baseline-subtracted thermogram data to tlbparam format
#' and calculates requested metrics using the tlbparam R package. This function
#' handles the mapping between UI-friendly metric names (e.g., "peak_1", "tfm")
#' and tlbparam's expected names (e.g., "Peak 1", "TFM").
#'
#' @details
#' **Workflow:**
#' 1. Validates input data structure
#' 2. Maps UI metric names to tlbparam metric names
#' 3. Converts processed data to tlbparam wide format (SampleCode + T45:T90 columns)
#' 4. Calls `tlbparam::clean_thermograms()` with requested metrics
#' 5. Extracts only requested metrics (temperature columns excluded)
#' 6. Renames result columns back to UI names
#'
#' **Supported Metrics (UI Name → tlbparam Name):**
#' \itemize{
#'   \item Peak metrics: peak_1→Peak 1, peak_2→Peak 2, peak_3→Peak 3, peak_f→Peak F
#'   \item Temperature peaks: tpeak_1→TPeak 1, tpeak_2→TPeak 2, tpeak_3→TPeak 3, tpeak_f→TPeak F
#'   \item Global: area→Area, tfm→TFM, width→Width, max_dcp→Max, tmax→TMax,
#'     min_dcp→Min, tmin→TMin, median_dcp→Median
#'   \item Valley: v12→V1.2, tv12→TV1.2
#'   \item Ratios: peak1_peak2_ratio→Peak 1 / Peak 2, peak1_peak3_ratio→Peak 1 / Peak 3,
#'     peak2_peak3_ratio→Peak 2 / Peak 3, v12_peak1_ratio→V1.2 / Peak 1,
#'     v12_peak2_ratio→V1.2 / Peak 2, v12_peak3_ratio→V1.2 / Peak 3
#' }
#'
#' @param processed_data
#'   List containing processed thermogram data. Expected structure:
#'   \itemize{
#'     \item `samples`: Named list of sample objects, each containing:
#'       \itemize{
#'         \item `sample_id`: Sample identifier string
#'         \item `temp_on_grid`: Numeric vector of temperatures (C)
#'         \item `dcp_baseline_subtracted`: Numeric vector of baseline-subtracted dCp values
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


# ==============================================================================
# CSV FORMAT VALIDATION
# ==============================================================================
#
# Provides detailed validation for CSV files with user-friendly error messages.
# Helps users understand and fix format issues in their data files.
#
# Returns structured feedback with:
#   - Errors:       Critical issues that prevent processing
#   - Warnings:     Non-critical issues that may affect results
#   - Suggestions:  Actionable advice for fixing problems
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