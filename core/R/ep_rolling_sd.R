# ==============================================================================
# Default endpoint method: rolling-SD scan, vectorised
# ==============================================================================
# Same algorithm as ep_rolling_sd_v1 with the O(n^2) data.frame growth
# removed. Per-window standard deviations are computed by calling sd() on
# the identical slices, so the SD profiles and the chosen windows are
# bit-identical to the reference; test-ep-equivalence.R enforces that.
#
# Deliberate differences from the reference:
#   - preconditions are classed errors, not printed-and-ignored
#   - point_selection is validated at construction
#   - ties: by default the first window in V1 scan order is taken and the tie
#     count is recorded in diagnostics; keep_ties = TRUE reproduces V1's
#     vector-valued endpoints.

#' Rolling-SD endpoint method (default)
#'
#' @inheritParams ep_rolling_sd_v1
#' @param keep_ties If `TRUE`, return every tied minimum-SD endpoint as V1
#'   did. Default `FALSE` returns the first in scan order.
#' @export
ep_rolling_sd <- S7::new_class(
  "ep_rolling_sd",
  parent = EndpointMethod,
  properties = list(
    w = S7::new_property(
      S7::class_numeric, default = 90,
      validator = function(value) {
        if (length(value) != 1L || value < 3 || value != round(value)) {
          "must be a single whole number >= 3"
        }
      }),
    exclusion_lower = S7::new_property(S7::class_numeric, default = 60),
    exclusion_upper = S7::new_property(S7::class_numeric, default = 80),
    point_selection = S7::new_property(
      S7::class_character, default = "innermost",
      validator = function(value) {
        if (!value %in% c("innermost", "mid", "outmost")) {
          "must be one of 'innermost', 'mid', 'outmost'"
        }
      }),
    keep_ties = S7::new_property(S7::class_logical, default = FALSE)
  ),
  constructor = function(w = 90, exclusion_lower = 60, exclusion_upper = 80,
                         point_selection = "innermost", keep_ties = FALSE) {
    S7::new_object(
      S7::S7_object(),
      id = "rolling_sd", reference = FALSE,
      w = w, exclusion_lower = exclusion_lower,
      exclusion_upper = exclusion_upper, point_selection = point_selection,
      keep_ties = keep_ties
    )
  },
  validator = function(self) {
    if (self@exclusion_lower >= self@exclusion_upper) {
      "exclusion_lower must be below exclusion_upper"
    }
  }
)

S7::method(detect_endpoints, ep_rolling_sd) <- function(method, temperature,
                                                        dcp, ...) {
  check_thermogram_input(temperature, dcp)
  w <- method@w
  k <- switch(method@point_selection, outmost = w, innermost = 0,
              mid = round(w / 2))
  n <- length(temperature)
  P <- sum(temperature < method@exclusion_lower)
  S <- n - sum(temperature > method@exclusion_upper)

  if (P <= w) {
    stop_input(sprintf(
      "only %d points below exclusion_lower; need more than w = %d", P, w))
  }
  if (n - w <= S) {
    stop_input(sprintf(
      "only %d points above exclusion_upper; need more than w = %d", n - S, w))
  }

  r <- stats::residuals(stats::smooth.spline(temperature, dcp, cv = TRUE))
  scan <- scan_windows(r, temperature, w, k, P, S, method@keep_ties)

  if (length(scan$lower) != 1L || length(scan$upper) != 1L) {
    stop_method(sprintf(
      "tied minimum-SD windows produced %d lower and %d upper endpoints",
      length(scan$lower), length(scan$upper)))
  }

  bl <- subtract_baseline_v1(temperature, dcp, scan$lower, scan$upper)
  EndpointResult(
    lower = scan$lower, upper = scan$upper,
    baseline = bl$baseline, subtracted = bl$subtracted,
    diagnostics = scan$diagnostics
  )
}

# The window scan on a residual vector, separated from the spline fit so the
# tie semantics can be tested on constructed residuals.
scan_windows <- function(r, temperature, w, k, P, S, keep_ties = FALSE) {
  n <- length(r)
  # Lower: V1 windows (1+i):(w+i) for i = 0 .. P-w-1 -> starts 1..P-w, width w.
  lower_sd <- rolling_sd(r, start = 1L, end = P - 1L, width = w)
  lower_stop <- as.numeric(seq.int(w, P - 1L))
  low <- which(lower_sd == min(lower_sd))
  if (!keep_ties) low <- low[1L]
  lower <- temperature[lower_stop[low] - k + 1]

  # Upper: V1 windows (n-w-j):(n-j) for j = 0 .. n-w-S-1 -> starts n-w down
  # to S+1, width w+1. V1 scanned from the top; reverse to keep its tie order.
  upper_sd <- rev(rolling_sd(r, start = S + 1L, end = n, width = w + 1L))
  upper_start <- as.numeric(seq.int(n - w, S + 1L))
  up <- which(upper_sd == min(upper_sd))
  if (!keep_ties) up <- up[1L]
  upper <- temperature[upper_start[up] + k]

  list(
    lower = lower, upper = upper,
    diagnostics = list(
      lower_sd = lower_sd, lower_stop = lower_stop,
      upper_sd = upper_sd, upper_start = upper_start,
      k = k, n_ties_lower = sum(lower_sd == min(lower_sd)),
      n_ties_upper = sum(upper_sd == min(upper_sd))
    )
  )
}

# sd() of every window of `width` consecutive points within x[start:end].
# Calls stats::sd on the same slices V1 did, so results are bit-identical.
rolling_sd <- function(x, start, end, width) {
  starts <- seq.int(start, end - width + 1L)
  vapply(starts, function(s) stats::sd(x[s:(s + width - 1L)]), numeric(1))
}

register_method(
  id = "rolling_sd", kind = "endpoint", constructor = ep_rolling_sd,
  label = "Rolling SD", default = TRUE,
  params = list(
    list(name = "w", type = "integer", default = 90L, min = 30L, max = 200L,
         label = "Window size (points)"),
    list(name = "exclusion_lower", type = "numeric", default = 60, min = 40,
         max = 80, label = "Exclusion zone lower bound (deg C)"),
    list(name = "exclusion_upper", type = "numeric", default = 80, min = 60,
         max = 95, label = "Exclusion zone upper bound (deg C)"),
    list(name = "point_selection", type = "choice", default = "innermost",
         choices = c("innermost", "mid", "outmost"), label = "Point selection")
  )
)
