# ==============================================================================
# Capture the V1 (ThermogramForge 0.1.0) processing results as test oracles.
# ==============================================================================
# Runs the V1 pipeline — inst/shiny-app/utils/*.R on top of
# ThermogramBaseline (sha b506d14) and tlbparam (sha c140f5d) — over the
# committed raw fixtures and a grid of parameter sets, and writes:
#
#   core/tests/testthat/fixtures/oracle/<dataset>__<paramset>.rds
#       list(params, samples = list(<id> = list(temperature_original,
#            dcp_original, lower, upper, has_signal, temperature_grid,
#            subtracted_grid)), versions)
#   core/tests/testthat/fixtures/oracle/endpoints.csv
#   core/tests/testthat/fixtures/oracle/subtracted__<dataset>__<paramset>.csv
#   core/tests/testthat/fixtures/oracle/metrics__<dataset>.csv   (default only)
#
# Run from the repo root on a machine with the two science packages installed:
#   Rscript core/data-raw/capture_v1_oracle.R
#
# V1 source is taken from inst/shiny-app if it is still on this branch,
# otherwise from a worktree of main at ../tf-v1 (git worktree add ../tf-v1 main).
# ==============================================================================

suppressPackageStartupMessages({
  library(ThermogramBaseline)
  library(tlbparam)
  library(forecast)
  library(dplyr)
  library(readr)
  library(readxl)
})

expected_sha <- c(ThermogramBaseline = "b506d143ebaad9a577a6a00e314c55e180869a48",
                  tlbparam = "c140f5d6b7157a5b0f9b7dc54eb9e67c7297f55a")
for (pkg in names(expected_sha)) {
  sha <- packageDescription(pkg)$RemoteSha
  if (!identical(sha, expected_sha[[pkg]])) {
    stop(sprintf("%s is at sha %s, oracle requires %s", pkg, sha, expected_sha[[pkg]]))
  }
}

v1_dir <- if (dir.exists("inst/shiny-app/utils")) "inst/shiny-app" else "../tf-v1/inst/shiny-app"
stopifnot(dir.exists(file.path(v1_dir, "utils")))
for (f in c("format_utils.R", "file_io_utils.R", "baseline_utils.R", "metrics_utils.R")) {
  source(file.path(v1_dir, "utils", f))
}

raw_dir <- "core/tests/testthat/fixtures/raw"
out_dir <- "core/tests/testthat/fixtures/oracle"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

datasets <- c("SuperSmall", "Small", "sample_multi_long", "sample_single_sample", "sample_multi_wide")

base_params <- list(temp_min = 20, temp_max = 110, window_size = 90,
                    exclusion_lower = 60, exclusion_upper = 80,
                    grid_resolution = 0.1, point_selection = "innermost")
param_sets <- list(
  default   = base_params,
  w50       = modifyList(base_params, list(window_size = 50)),
  w120      = modifyList(base_params, list(window_size = 120)),
  mid       = modifyList(base_params, list(point_selection = "mid")),
  outmost   = modifyList(base_params, list(point_selection = "outmost")),
  excl55_85 = modifyList(base_params, list(exclusion_lower = 55, exclusion_upper = 85))
)

versions <- list(
  R = R.version.string,
  platform = R.version$platform,
  ThermogramBaseline_sha = expected_sha[["ThermogramBaseline"]],
  tlbparam_sha = expected_sha[["tlbparam"]],
  forecast = as.character(packageVersion("forecast")),
  captured = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
)

# V1 silences nothing; keep the console readable.
quiet <- function(expr) {
  utils::capture.output(res <- suppressMessages(suppressWarnings(expr)))
  res
}

# ------------------------------------------------------------------------------
# Per-sample processing through V1's own helpers.
# process_thermogram_data() only works for the wide format (V1 never sets
# sample_ids for long/single), so every dataset goes through the same
# per-sample path here: extract_sample_data() -> temperature filter ->
# apply_baseline_detection() -> apply_signal_detection(). This is exactly
# what process_thermogram_data() does per sample for the wide case.
# ------------------------------------------------------------------------------
# V1 format_type values: multi_sample_wide, multi_sample_long, single_sample.
# Only the wide detector populates sample_ids; derive the others here.
sample_ids_for <- function(data, format_info) {
  fmt <- format_info$format_type
  if (identical(fmt, "multi_sample_wide")) {
    return(list(type = fmt, ids = format_info$sample_ids))
  }
  if (identical(fmt, "multi_sample_long")) {
    return(list(type = fmt, ids = as.character(unique(data$Sample_ID))))
  }
  list(type = "single_sample", ids = "sample")
}

process_one <- function(sample_data, params, id) {
  # Same filter and grid construction as V1 process_thermogram_data():
  # the grid comes from the parameter bounds, not the data, and V1 never sorts.
  sd <- sample_data[sample_data$Temperature >= params$temp_min &
                    sample_data$Temperature <= params$temp_max, , drop = FALSE]
  grid <- seq(max(45, params$temp_min), min(90, params$temp_max), by = params$grid_resolution)

  bl <- quiet(apply_baseline_detection(
    sample_data = sd, window_size = params$window_size,
    exclusion_lower = params$exclusion_lower, exclusion_upper = params$exclusion_upper,
    grid_temp = grid, point_selection = params$point_selection, sample_id = id))
  if (is.null(bl)) return(NULL)

  sig <- quiet(apply_signal_detection(bl$processed_df, id))

  list(temperature_original = sd$Temperature, dcp_original = sd$dCp,
       lower = bl$lower_endpoint, upper = bl$upper_endpoint,
       has_signal = isTRUE(sig$has_signal),
       temperature_grid = bl$temperature, subtracted_grid = bl$baseline_subtracted)
}

endpoint_rows <- list()

for (ds in datasets) {
  path <- file.path(raw_dir, paste0(ds, ".csv"))
  cat(sprintf("== %s\n", ds))
  read <- quiet(read_thermogram_file(path))
  data <- read$data
  ids <- sample_ids_for(data, read$format_info)
  cat(sprintf("   format=%s samples=%d\n", ids$type, length(ids$ids)))

  for (ps in names(param_sets)) {
    params <- param_sets[[ps]]
    samples <- list()
    for (id in ids$ids) {
      res <- process_one(extract_sample_data(data, id, ids$type), params, id)
      if (is.null(res)) { cat(sprintf("   [%s] %s: FAILED in V1 (skipped)\n", ps, id)); next }
      samples[[id]] <- res
      endpoint_rows[[length(endpoint_rows) + 1]] <- data.frame(
        dataset = ds, paramset = ps, sample_id = id,
        lower = res$lower, upper = res$upper, has_signal = res$has_signal)
    }
    saveRDS(list(params = params, samples = samples, versions = versions),
            file.path(out_dir, sprintf("%s__%s.rds", ds, ps)))

    if (length(samples)) {
      grid <- samples[[1]]$temperature_grid
      mat <- do.call(rbind, lapply(samples, function(s) s$subtracted_grid))
      wide <- data.frame(SampleCode = names(samples), mat, check.names = FALSE)
      names(wide)[-1] <- paste0("T", format(grid, nsmall = 1, trim = TRUE))
      write.csv(wide, file.path(out_dir, sprintf("subtracted__%s__%s.csv", ds, ps)), row.names = FALSE)
    }
    cat(sprintf("   [%s] %d samples\n", ps, length(samples)))
  }

  # Metrics on the default parameter set, through V1's own conversion path.
  default_file <- file.path(out_dir, sprintf("%s__default.rds", ds))
  o <- readRDS(default_file)
  if (length(o$samples)) {
    processed <- list(samples = lapply(names(o$samples), function(id) {
      s <- o$samples[[id]]
      list(sample_id = id, success = TRUE, excluded = FALSE,
           temperature = s$temperature_grid, baseline_subtracted = s$subtracted_grid,
           lower_endpoint = s$lower, upper_endpoint = s$upper, has_signal = s$has_signal)
    }))
    names(processed$samples) <- names(o$samples)
    all_metrics <- c("peak_1", "peak_2", "peak_3", "peak_f", "tpeak_1", "tpeak_2", "tpeak_3",
                     "tpeak_f", "ratio_12", "ratio_13", "ratio_23", "fwhm", "area", "max_dcp",
                     "tmax", "tfm", "median", "v12", "tv12", "v12_peak1_ratio",
                     "v12_peak2_ratio", "v12_peak3_ratio", "min_dcp", "tmin")
    metrics <- tryCatch(quiet(calculate_tlbparam_metrics(processed, all_metrics)),
                        error = function(e) { cat("   metrics FAILED:", conditionMessage(e), "\n"); NULL })
    if (!is.null(metrics)) {
      write.csv(metrics, file.path(out_dir, sprintf("metrics__%s.csv", ds)), row.names = FALSE)
      cat(sprintf("   metrics: %d x %d\n", nrow(metrics), ncol(metrics)))
    }
  }
}

endpoints <- do.call(rbind, endpoint_rows)
write.csv(endpoints, file.path(out_dir, "endpoints.csv"), row.names = FALSE)
writeLines(c("# Oracle provenance", "", sprintf("- %s: %s", names(versions), unlist(versions))),
           file.path(out_dir, "VERSIONS.md"))

# ------------------------------------------------------------------------------
# Self-check against the app-produced Small_RDS.rds (V1 run of 2025-10-23).
# ------------------------------------------------------------------------------
cat("\n== Self-check: Small__default vs app_Small_RDS.rds\n")
app <- readRDS(file.path(out_dir, "app_Small_RDS.rds"))
ora <- readRDS(file.path(out_dir, "Small__default.rds"))
ok <- TRUE
for (id in names(app$samples)) {
  a <- app$samples[[id]]; o <- ora$samples[[id]]
  if (is.null(o)) { cat(sprintf("   %s: missing in oracle\n", id)); ok <- FALSE; next }
  checks <- c(
    lower  = identical(a$lower_endpoint, o$lower),
    upper  = identical(a$upper_endpoint, o$upper),
    signal = identical(isTRUE(a$has_signal), o$has_signal),
    curve  = identical(a$baseline_subtracted, o$subtracted_grid))
  if (!all(checks)) {
    ok <- FALSE
    cat(sprintf("   %s: %s\n", id, paste(names(checks)[!checks], collapse = ", ")))
    if (!checks[["curve"]]) print(all.equal(a$baseline_subtracted, o$subtracted_grid, tolerance = 0))
  }
}
cat(if (ok) "   ALL TRUE\n" else "   MISMATCHES (see above)\n")
