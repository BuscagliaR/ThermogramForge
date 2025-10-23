# ==============================================================================
# Data Utilities for ThermogramForge
# ==============================================================================
#
# PURPOSE:
#   Core functions for reading, parsing, validating, and converting thermogram
#   data files (CSV and Excel formats)
#
# FUNCTIONS:
#   1. detect_file_type()          - Determine if file is CSV or Excel
#   2. read_thermogram_file()      - Read and parse thermogram data
#   3. detect_data_format()        - Identify data structure/format
#   4. validate_thermogram_data()  - Check data quality and completeness
#   5. convert_to_tlbparam_format() - Convert to tlbparam wide format
#
# DATA FORMATS SUPPORTED:
#   - Single sample: Temperature, dCp columns
#   - Multi-sample long: Sample_ID, Temperature, dCp columns
#   - Multi-sample wide: T1a, 1a, T1b, 1b column pairs
#
# AUTHOR: Chris Reger
# LAST UPDATED: October 22, 2025
# ==============================================================================

# ==============================================================================
# File Type Detection
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
# File Reading and Parsing
# ==============================================================================

#' Read Thermogram Data File
#'
#' @description
#' Reads thermogram data from CSV or Excel file and returns a structured list
#' containing the raw data, format information, and validation results.
#'
#' @param filepath Character. Path to the file to read
#' @param temp_min Numeric or NULL. Minimum temperature to include (°C).
#'   If NULL, no lower filtering is applied. Default: NULL
#' @param temp_max Numeric or NULL. Maximum temperature to include (°C).
#'   If NULL, no upper filtering is applied. Default: NULL
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
#' Process:
#' 1. Detect file type (CSV or Excel)
#' 2. Read file using appropriate reader
#' 3. Detect data format structure
#' 4. Apply temperature filtering if requested
#' 5. Return structured data
#'
#' @examples
#' \dontrun{
#' # Read entire file
#' result <- read_thermogram_file("thermogram.csv")
#'
#' # Read with temperature filtering
#' result <- read_thermogram_file(
#'   "thermogram.csv",
#'   temp_min = 20,
#'   temp_max = 110
#' )
#' }
#'
#' @export
read_thermogram_file <- function(filepath, temp_min = NULL, temp_max = NULL) {
  
  # ============================================================================
  # Step 1: Detect and Read File
  # ============================================================================
  
  # Determine file type from extension
  file_type <- detect_file_type(filepath)
  
  # Read file based on type
  raw_data <- tryCatch({
    
    if (file_type == "csv") {
      # Read CSV file
      readr::read_csv(filepath, show_col_types = FALSE)
      
    } else if (file_type == "excel") {
      # Read Excel file (first sheet by default)
      readxl::read_excel(filepath)
      
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
  
  # Only filter if at least one bound is specified
  if (!is.null(temp_min) || !is.null(temp_max)) {
    
    cat(sprintf(
      "[READ] Applying temperature filter: %.1f to %.1f°C\n",
      ifelse(is.null(temp_min), -Inf, temp_min),
      ifelse(is.null(temp_max), Inf, temp_max)
    ))
    
    # Filter differently based on data format
    if (format_info$data_format == "standard") {
      # Standard format: single Temperature column
      
      temp_col <- format_info$temperature_col
      n_before <- nrow(raw_data)
      
      # Create filter mask
      keep_mask <- rep(TRUE, nrow(raw_data))
      
      if (!is.null(temp_min)) {
        keep_mask <- keep_mask & (raw_data[[temp_col]] >= temp_min)
      }
      
      if (!is.null(temp_max)) {
        keep_mask <- keep_mask & (raw_data[[temp_col]] <= temp_max)
      }
      
      # Apply filter
      raw_data <- raw_data[keep_mask, , drop = FALSE]
      
      cat(sprintf(
        "[READ] Filtered: kept %d/%d points\n",
        nrow(raw_data),
        n_before
      ))
      
    } else if (format_info$data_format == "wide") {
      # Wide format: multiple temperature columns (one per sample)
      
      # Filter each sample's temperature column independently
      for (i in seq_along(format_info$temperature_cols)) {
        temp_col <- format_info$temperature_cols[i]
        sample_id <- format_info$sample_ids[i]
        
        # Get temperature values for this sample
        temps <- raw_data[[temp_col]]
        
        # Create filter mask for this sample
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
# Data Format Detection
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
# Data Validation
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
          "Unusual temperature range: %.1f to %.1f°C",
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
# tlbparam Format Conversion
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
    "[FORMAT] Temperature grid: %d points from %.1f to %.1f°C\n",
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
      "[FORMAT] ✓ Sample %s: interpolated to %d points\n",
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
    "\n[FORMAT] ✅ Conversion complete: %d samples x %d columns\n",
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
#'     \item temp_min: Minimum temperature (°C) for filtering
#'     \item temp_max: Maximum temperature (°C) for filtering
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
  cat(sprintf("  - Temperature range: [%.1f, %.1f]°C\n", temp_params$temp_min, temp_params$temp_max))
  cat(sprintf("  - Window size: %d\n", window_size))
  cat(sprintf("  - Exclusion zone: [%.1f, %.1f]°C\n", exclusion_lower, exclusion_upper))
  cat(sprintf("  - Grid resolution: %.2f°C\n", grid_resolution))
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
      reviewed = FALSE
    )
    
    cat(sprintf("[PROCESS_UTIL] ✓ Sample %s processed successfully (endpoints: %.1f, %.1f)\n", 
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
    
    cat(sprintf("[PROCESS_UTIL] ✓ Baseline detection complete (endpoints: %.1f, %.1f°C)\n", 
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
    
    cat(sprintf("[PROCESS_UTIL] ✓ Signal detection: %s\n", result_string))
    
    return(list(has_signal = has_sig))
    
  }, error = function(e) {
    cat(sprintf("[PROCESS_UTIL] WARNING: Signal detection failed for %s: %s\n", sample_id, e$message))
    cat("[PROCESS_UTIL] Defaulting to 'has_signal = TRUE'\n")
    return(list(has_signal = TRUE))
  })
}