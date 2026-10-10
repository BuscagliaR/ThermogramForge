# ==============================================================================
# V1-equivalent per-sample pipeline
# ==============================================================================
# Reproduces what ThermogramForge 0.1.0 did per sample in
# process_thermogram_data(): temperature filter -> endpoint detection ->
# baseline subtraction -> spline interpolation to the grid -> signal test on
# the interpolated curve. Used by the oracle tests and the timing harness.

#' Process one thermogram the V1 way
#'
#' @param temperature,dcp Raw sample, sorted by temperature.
#' @param endpoint_method An [EndpointMethod]; default is the V1 reference.
#' @param signal_method A [SignalMethod]; default is the V1 reference.
#' @param grid Interpolation grid.
#' @param temp_min,temp_max Temperature filter applied before anything else.
#' @return A list with `lower`, `upper`, `baseline`, `subtracted` (raw axis),
#'   `grid`, `subtracted_grid`, `has_signal`, `timings` (seconds per stage).
#' @export
process_sample_v1 <- function(temperature, dcp,
                              endpoint_method = ep_rolling_sd_v1(),
                              signal_method = sig_auto_arima_v1(),
                              grid = seq(45, 90, by = 0.1),
                              temp_min = 20, temp_max = 110) {
  keep <- temperature >= temp_min & temperature <= temp_max
  temperature <- temperature[keep]
  dcp <- dcp[keep]

  t0 <- proc.time()[["elapsed"]]
  ep <- detect_endpoints(endpoint_method, temperature, dcp)
  t1 <- proc.time()[["elapsed"]]
  sub_grid <- interpolate_v1(temperature, ep@subtracted, grid)
  t2 <- proc.time()[["elapsed"]]
  sig <- classify_signal(signal_method, grid, sub_grid)
  t3 <- proc.time()[["elapsed"]]

  list(
    lower = ep@lower, upper = ep@upper,
    baseline = ep@baseline, subtracted = ep@subtracted,
    temperature = temperature, dcp = dcp,
    grid = grid, subtracted_grid = sub_grid,
    has_signal = sig@has_signal,
    timings = c(endpoints_and_baseline = t1 - t0, interpolate = t2 - t1,
                signal = t3 - t2)
  )
}

#' Process every sample in a set
#'
#' @param set A [ThermogramSet].
#' @inheritParams process_sample_v1
#' @param progress Optional function called with `(i, n, id)` after each
#'   sample.
#' @return Named list of [process_sample_v1()] results; failed samples hold
#'   the error condition instead.
#' @export
process_set_v1 <- function(set, endpoint_method = ep_rolling_sd_v1(),
                           signal_method = sig_auto_arima_v1(),
                           grid = seq(45, 90, by = 0.1),
                           temp_min = 20, temp_max = 110, progress = NULL) {
  ids <- names(set@samples)
  out <- vector("list", length(ids))
  names(out) <- ids
  for (i in seq_along(ids)) {
    s <- set@samples[[i]]
    out[[i]] <- tryCatch(
      process_sample_v1(s$Temperature, s$dCp, endpoint_method, signal_method,
                        grid, temp_min, temp_max),
      error = function(e) e
    )
    if (!is.null(progress)) progress(i, length(ids), ids[i])
  }
  out
}
