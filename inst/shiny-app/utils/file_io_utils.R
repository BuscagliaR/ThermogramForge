# ==============================================================================
# File I/O Utilities for ThermogramForge
# ==============================================================================
#
# FILE: file_io_utils.R
# VERSION: 1.0.0
# AUTHOR: Chris Reger
# LAST UPDATED: December 9, 2024
#
# ==============================================================================
# PURPOSE
# ==============================================================================
#
# File input/output operations for thermogram data. Handles:
#
#   - Reading CSV and Excel files with format detection
#   - Saving processed data in multiple formats (RDS, CSV, Excel)
#   - Loading previously saved datasets
#   - Listing and deleting saved datasets
#
# ==============================================================================
# FUNCTIONS
# ==============================================================================
#
# File Reading:
#   - detect_file_type()           Determine if file is CSV or Excel
#   - read_thermogram_file()       Read and parse thermogram data with options
#
# File Saving:
#   - save_processed_data()        Save dataset to RDS/CSV/Excel
#   - format_data_for_wide_export() (internal) Prepare data for CSV/Excel
#   - save_metadata_file()         (internal) Write metadata text file
#   - create_metadata_dataframe()  (internal) Build metadata summary
#
# File Loading:
#   - load_processed_data()        Load previously saved dataset
#   - list_processed_datasets()    List all saved datasets in directory
#   - delete_processed_data()      Remove saved dataset files
#
# ==============================================================================
# DEPENDENCIES
# ==============================================================================
#
# Required packages:
#   - readr:    CSV file reading/writing
#   - readxl:   Excel file reading
#   - openxlsx: Excel file writing (for multi-sheet workbooks)
#   - tools:    File extension utilities
#
# Required project files:
#   - format_utils.R: detect_data_format() for format detection after reading
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