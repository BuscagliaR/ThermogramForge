# Data Utilities for ThermogramForge
# File parsing and validation functions

#' Detect file format type
#'
#' @param file_path Path to uploaded file
#' @return Character: "csv" or "excel"
detect_file_type <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))
  
  if (ext == "csv") {
    return("csv")
  } else if (ext %in% c("xlsx", "xls")) {
    return("excel")
  } else {
    stop("Unsupported file type. Please upload CSV or Excel files.")
  }
}

#' Read Thermogram File (CSV or Excel)
#' 
#' @param filepath Path to the file
#' @param temp_min Minimum temperature to include (default: NULL = no filtering)
#' @param temp_max Maximum temperature to include (default: NULL = no filtering)
#' 
#' @return List with data and format_info, or NULL on error
#' 
#' @export
read_thermogram_file <- function(filepath, temp_min = NULL, temp_max = NULL) {
  
  tryCatch({
    if (file_type == "csv") {
      data <- readr::read_csv(
        file_path,
        show_col_types = FALSE
      )
    } else if (file_type == "excel") {
      data <- readxl::read_excel(file_path)
    }
    
    # Apply temperature filtering if specified
    if (!is.null(temp_min) || !is.null(temp_max)) {
      
      cat(sprintf("[READ] Applying temperature filter: %.1f to %.1f°C\n",
                  ifelse(is.null(temp_min), -Inf, temp_min),
                  ifelse(is.null(temp_max), Inf, temp_max)))
      
      # Filter each sample's temperature data
      for (sample_name in names(data)) {
        sample_data <- data[[sample_name]]
        
        # Assume temperature column exists
        if ("Temperature" %in% names(sample_data)) {
          
          # Create filter mask
          keep_mask <- rep(TRUE, nrow(sample_data))
          
          if (!is.null(temp_min)) {
            keep_mask <- keep_mask & (sample_data$Temperature >= temp_min)
          }
          
          if (!is.null(temp_max)) {
            keep_mask <- keep_mask & (sample_data$Temperature <= temp_max)
          }
          
          # Apply filter
          data[[sample_name]] <- sample_data[keep_mask, , drop = FALSE]
          
          cat(sprintf("[READ] Sample %s: kept %d/%d points\n",
                      sample_name,
                      sum(keep_mask),
                      length(keep_mask)))
        }
      }
    }    
    
    return(list(
      data = data,
      format_info = format_info
    ))
    
  }, error = function(e) {
    stop(paste("Error reading file:", e$message))
  })
}

#' Detect thermogram data format
#'
#' @param data Data frame to analyze
#' @return List with format type and metadata
detect_data_format <- function(data) {
  
  cols <- colnames(data)
  cols_lower <- tolower(cols)
  
  # Check for standard single-sample format (Temperature, dCp columns)
  has_temp_col <- any(grepl("^temp", cols_lower))
  has_dcp_col <- any(grepl("^dcp|^d_cp|^delta_cp", cols_lower))
  
  if (has_temp_col && has_dcp_col) {
    # Standard single-sample format
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
      data_points = rep(nrow(data), n_samples),  # All samples have same row count in long format
      all_columns = cols,
      data_format = "standard"
    ))
  }
  
  # Check for multi-sample wide format (T1a, 1a, T1b, 1b pattern)
  # Temperature columns typically start with 'T' followed by identifier
  # dCp columns are the identifiers without 'T'
  temp_cols <- cols[grepl("^T[0-9]", cols)]
  
  if (length(temp_cols) > 0) {
    # Extract sample identifiers from temperature columns
    sample_ids <- gsub("^T", "", temp_cols)
    
    # Check if corresponding dCp columns exist
    dcp_cols <- sample_ids[sample_ids %in% cols]
    
    if (length(dcp_cols) > 0) {
      # Check which samples actually have data
      valid_samples <- logical(length(dcp_cols))
      data_points <- integer(length(dcp_cols))
      
      for (i in seq_along(dcp_cols)) {
        dcp_data <- data[[dcp_cols[i]]][!is.na(data[[dcp_cols[i]]])]
        valid_samples[i] <- length(dcp_data) > 0
        data_points[i] <- length(dcp_data)
      }
      
      # Filter to only valid samples
      valid_idx <- which(valid_samples)
      
      if (length(valid_idx) == 0) {
        stop("No samples with valid data found in file")
      }
      
      n_samples_total <- length(dcp_cols)
      n_samples_valid <- length(valid_idx)
      n_samples_empty <- n_samples_total - n_samples_valid
      
      # Multi-sample wide format detected
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
  
  # If we get here, required columns not found
  stop(
    "Required columns not found. Data must either:\n",
    "1. Contain 'Temperature' and 'dCp' columns (standard format), OR\n",
    "2. Contain paired columns like 'T1a' and '1a' (multi-sample wide format)\n\n",
    "Found columns: ", paste(cols, collapse = ", ")
  )
}

#' Validate thermogram data
#'
#' @param data Data frame to validate
#' @param format_info Format information from detect_data_format()
#' @return List with validation results
validate_thermogram_data <- function(data, format_info) {
  
  errors <- character()
  warnings <- character()
  
  # Check row count
  if (format_info$n_rows < 10) {
    errors <- c(errors, "Too few data points (minimum 10 required)")
  }
  
  # Validate based on format type
  if (format_info$data_format == "standard") {
    # Standard format validation
    temp_na <- sum(is.na(data[[format_info$temperature_col]]))
    dcp_na <- sum(is.na(data[[format_info$dcp_col]]))
    
    if (temp_na > 0) {
      errors <- c(
        errors, 
        sprintf("%d missing values in Temperature column", temp_na)
      )
    }
    
    if (dcp_na > 0) {
      warnings <- c(
        warnings,
        sprintf("%d missing values in dCp column", dcp_na)
      )
    }
    
    # Check temperature range
    temp_range <- range(data[[format_info$temperature_col]], na.rm = TRUE)
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
    
  } else if (format_info$data_format == "wide") {
    # Wide format validation - check each temperature column
    # Note: In wide format, NAs at the end are expected due to different sample lengths
    
    for (i in seq_along(format_info$temperature_cols)) {
      temp_col <- format_info$temperature_cols[i]
      dcp_col <- format_info$dcp_cols[i]
      sample_id <- format_info$sample_ids[i]
      
      # Count non-NA values (actual data points for this sample)
      temp_data <- data[[temp_col]][!is.na(data[[temp_col]])]
      n_points <- length(temp_data)
      
      # Check if dCp column is entirely empty (would be a real problem)
      dcp_data <- data[[dcp_col]][!is.na(data[[dcp_col]])]
      if (length(dcp_data) == 0) {
        warnings <- c(
          warnings,
          sprintf("Sample %s has no dCp data (will be skipped)", sample_id)
        )
        next  # Skip other checks for this empty sample
      }
      
      # Only warn if sample has very few points
      if (n_points < 10 && n_points > 0) {
        warnings <- c(
          warnings,
          sprintf("Sample %s has only %d data points", sample_id, n_points)
        )
      }
      
      # Check temperature range only for non-NA values
      if (n_points > 0) {
        temp_range <- range(temp_data, na.rm = TRUE)
        if (temp_range[1] < 0 || temp_range[2] > 150) {
          warnings <- c(
            warnings,
            sprintf(
              "Unusual temperature range in sample %s: %.1f to %.1f°C", 
              sample_id,
              temp_range[1], 
              temp_range[2]
            )
          )
        }
      }
    }
  }
  
  # Check sample count limit
  if (format_info$n_samples > 1000) {
    errors <- c(
      errors,
      sprintf(
        "Too many samples (%d). Maximum 1000 samples per file.",
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

#' Process uploaded thermogram file
#'
#' @param file_path Path to uploaded file
#' @param file_name Original filename
#' @return List with processed data and metadata
process_upload <- function(file_path, file_name) {
  
  # Detect file type
  file_type <- detect_file_type(file_path)
  
  # Read data
  data <- read_thermogram_file(file_path, file_type)
  
  # Detect format
  format_info <- detect_data_format(data)
  
  # Validate
  validation <- validate_thermogram_data(data, format_info)
  
  # Return comprehensive results
  list(
    success = validation$valid,
    data = if (validation$valid) data else NULL,
    file_name = file_name,
    file_type = file_type,
    format_info = format_info,
    validation = validation,
    upload_time = Sys.time()
  )
}