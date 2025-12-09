# ==============================================================================
# Data Utilities for ThermogramForge
# ==============================================================================
#
# PURPOSE:
#   This file provides data handling utilities for ThermogramForge.
#   
# NOTE: Core functions for file reading, format detection, and validation
#   are defined in processing_utils.R to avoid duplication:
#     - detect_file_type()
#     - read_thermogram_file()
#     - detect_data_format()
#     - validate_thermogram_data()
#
# This file is kept for backwards compatibility and any future data-specific
# utilities that don't belong in the processing pipeline.
#
# AUTHOR: Chris Reger
# LAST UPDATED: December 8, 2024
# ==============================================================================


# ==============================================================================
# TEMPLATE GENERATION UTILITIES
# ==============================================================================
# These functions generate downloadable template files for users

#' Generate Single Sample CSV Template
#'
#' @description
#' Creates a minimal CSV template demonstrating single-sample format.
#'
#' @return Data frame with example thermogram data
#'
#' @examples
#' \dontrun{
#' template <- generate_single_sample_template()
#' write_csv(template, "single_sample_template.csv")
#' }
#'
#' @export
generate_single_sample_template <- function() {
  # Generate example temperature values (typical DSC range)
  temps <- seq(45, 90, by = 0.5)
  
  # Generate realistic dCp values (Gaussian-like peak)
  dcp <- dnorm(temps, mean = 65, sd = 8) * 10 - 0.1
  
  data.frame(
    Temperature = temps,
    dCp = round(dcp, 6)
  )
}


#' Generate Multi-Sample Long Format Template
#'
#' @description
#' Creates a CSV template demonstrating multi-sample long format with Sample_ID.
#'
#' @param n_samples Number of example samples to generate (default: 3)
#'
#' @return Data frame with example thermogram data for multiple samples
#'
#' @export
generate_multi_sample_long_template <- function(n_samples = 3) {
  
  temps <- seq(45, 90, by = 0.5)
  
  samples <- lapply(seq_len(n_samples), function(i) {
    # Vary peak position slightly for each sample
    peak_temp <- 65 + (i - 2) * 2
    dcp <- dnorm(temps, mean = peak_temp, sd = 8) * 10 - 0.1
    
    data.frame(
      Sample_ID = paste0("Sample_", LETTERS[i]),
      Temperature = temps,
      dCp = round(dcp, 6)
    )
  })
  
  do.call(rbind, samples)
}


#' Generate Multi-Sample Wide Format Template
#'
#' @description
#' Creates a CSV template demonstrating multi-sample wide format (T1a, 1a pairs).
#'
#' @param n_samples Number of example samples to generate (default: 3)
#'
#' @return Data frame with example thermogram data in wide format
#'
#' @export
generate_multi_sample_wide_template <- function(n_samples = 3) {
  
  temps <- seq(45, 90, by = 0.5)
  n_rows <- length(temps)
  
  result <- data.frame(matrix(ncol = 0, nrow = n_rows))
  
  sample_ids <- c("1a", "1b", "2a", "2b", "3a")[seq_len(n_samples)]
  
  for (i in seq_len(n_samples)) {
    sample_id <- sample_ids[i]
    
    # Vary peak position slightly for each sample
    peak_temp <- 65 + (i - 2) * 2
    dcp <- dnorm(temps, mean = peak_temp, sd = 8) * 10 - 0.1
    
    # Add temperature column (T1a, T1b, etc.)
    result[[paste0("T", sample_id)]] <- temps
    
    # Add dCp column (1a, 1b, etc.)
    result[[sample_id]] <- round(dcp, 6)
  }
  
  result
}


# ==============================================================================
# DATA SUMMARY UTILITIES
# ==============================================================================

#' Summarize Thermogram Dataset
#'
#' @description
#' Generates a summary of thermogram dataset characteristics.
#'
#' @param data Data frame containing thermogram data
#' @param format_info Format information from detect_data_format()
#'
#' @return List with dataset summary statistics
#'
#' @export
summarize_thermogram_data <- function(data, format_info) {
  
  summary_list <- list(
    format_type = format_info$format_type,
    n_samples = format_info$n_samples,
    n_rows = nrow(data),
    n_columns = ncol(data)
  )
  
  # Add format-specific details
  if (format_info$format_type == "single_sample") {
    
    temp_range <- range(data[[format_info$temperature_col]], na.rm = TRUE)
    dcp_range <- range(data[[format_info$dcp_col]], na.rm = TRUE)
    
    summary_list$temperature_range <- temp_range
    summary_list$dcp_range <- dcp_range
    summary_list$data_points <- nrow(data)
    
  } else if (format_info$format_type == "multi_sample_wide") {
    
    summary_list$sample_ids <- format_info$sample_ids
    summary_list$data_points_per_sample <- format_info$data_points
    summary_list$avg_data_points <- mean(format_info$data_points)
    
    if (!is.null(format_info$empty_sample_ids)) {
      summary_list$empty_samples <- format_info$empty_sample_ids
    }
    
  } else if (format_info$format_type == "multi_sample_long") {
    
    # Count data points per sample
    sample_counts <- table(data[[format_info$sample_id_col]])
    summary_list$data_points_per_sample <- as.list(sample_counts)
    summary_list$avg_data_points <- mean(sample_counts)
  }
  
  summary_list
}


#' Format Validation Results as HTML
#'
#' @description
#' Formats validation results as HTML for display in Shiny notifications.
#'
#' @param validation List from validate_thermogram_data()
#'
#' @return HTML string suitable for shiny::showNotification()
#'
#' @export
format_validation_html <- function(validation) {
  
  parts <- character()
  
  if (!validation$valid) {
    parts <- c(parts, "<b>Errors:</b><ul>")
    for (err in validation$errors) {
      parts <- c(parts, sprintf("<li>%s</li>", err))
    }
    parts <- c(parts, "</ul>")
  }
  
  if (length(validation$warnings) > 0) {
    parts <- c(parts, "<b>Warnings:</b><ul>")
    for (warn in validation$warnings) {
      parts <- c(parts, sprintf("<li>%s</li>", warn))
    }
    parts <- c(parts, "</ul>")
  }
  
  HTML(paste(parts, collapse = ""))
}


# ==============================================================================
# DEPRECATED: Use processing_utils.R versions instead
# ==============================================================================
# 
# The following functions have been moved to processing_utils.R:
#   - detect_file_type()        -> processing_utils.R line 50
#   - read_thermogram_file()    -> processing_utils.R line 138
#   - detect_data_format()      -> processing_utils.R line 267
#   - validate_thermogram_data() -> processing_utils.R line 386
#   - process_upload()          -> Use read_thermogram_file() + detect_data_format()
#
# These are NOT re-defined here to avoid conflicts when both files are sourced.
# ==============================================================================