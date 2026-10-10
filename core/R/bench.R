# ==============================================================================
# Timing harness for the Phase-0 performance gate
# ==============================================================================
# Runs the same per-stage pipeline natively and inside webR so the two can be
# compared like for like. Stages: read (done by the caller), endpoints +
# baseline, interpolation, signal.

#' Time the pipeline over the first `n` samples of a set
#'
#' @param set A [ThermogramSet].
#' @param n Number of samples to process (default all).
#' @param endpoint_method,signal_method Methods to time. Defaults are the
#'   current defaults; pass the `_v1` reference objects to time those.
#' @param grid Interpolation grid.
#' @return A list: `per_sample` (data.frame of seconds per stage and sample),
#'   `totals` (named numeric, seconds), `n`, `failed` (sample ids that
#'   errored), `session` (R version and platform).
#' @export
bench_pipeline <- function(set, n = n_samples(set),
                           endpoint_method = ep_rolling_sd(),
                           signal_method = sig_kpss(),
                           grid = seq(45, 90, by = 0.1)) {
  ids <- utils::head(names(set@samples), n)
  rows <- vector("list", length(ids))
  failed <- character()
  t_start <- proc.time()[["elapsed"]]
  for (i in seq_along(ids)) {
    s <- set@samples[[ids[i]]]
    res <- tryCatch(
      suppressWarnings(process_sample_v1(
        s$Temperature, s$dCp, endpoint_method, signal_method, grid)),
      error = function(e) e)
    if (inherits(res, "error")) {
      failed <- c(failed, ids[i])
      rows[[i]] <- data.frame(sample_id = ids[i], n_points = nrow(s),
                              endpoints_and_baseline = NA_real_,
                              interpolate = NA_real_, signal = NA_real_,
                              has_signal = NA)
    } else {
      rows[[i]] <- data.frame(sample_id = ids[i], n_points = nrow(s),
                              endpoints_and_baseline = res$timings[["endpoints_and_baseline"]],
                              interpolate = res$timings[["interpolate"]],
                              signal = res$timings[["signal"]],
                              has_signal = res$has_signal)
    }
  }
  wall <- proc.time()[["elapsed"]] - t_start
  per_sample <- do.call(rbind, rows)
  totals <- c(
    endpoints_and_baseline = sum(per_sample$endpoints_and_baseline, na.rm = TRUE),
    interpolate = sum(per_sample$interpolate, na.rm = TRUE),
    signal = sum(per_sample$signal, na.rm = TRUE),
    wall = wall
  )
  list(per_sample = per_sample, totals = totals, n = length(ids),
       failed = failed,
       session = list(R = R.version.string, platform = R.version$platform,
                      endpoint_method = endpoint_method@id,
                      signal_method = signal_method@id))
}

#' One-line summary of a benchmark result
#' @param b Result of [bench_pipeline()].
#' @export
format_bench <- function(b) {
  t <- b$totals
  sprintf("%d samples in %.2fs wall (endpoints+baseline %.2fs, interpolate %.2fs, signal %.2fs; %d failed) [%s/%s, %s]",
          b$n, t[["wall"]], t[["endpoints_and_baseline"]], t[["interpolate"]],
          t[["signal"]], length(b$failed), b$session$endpoint_method,
          b$session$signal_method, b$session$platform)
}
