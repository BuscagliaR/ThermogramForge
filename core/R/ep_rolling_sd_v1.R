# ==============================================================================
# Reference endpoint method: literal port of ThermogramBaseline (sha b506d14)
# ==============================================================================
# This file reproduces ThermogramBaseline::endpoint.detection() and
# baseline.subtraction.byhand() as used in Reger et al., Cancers 2025.
# It is frozen: do not optimise or "fix" it. Behavioural quirks are kept on
# purpose and called out inline. The vectorised default in ep_rolling_sd.R is
# tested against this implementation.
#
# Differences from the original that cannot affect numeric results:
#   - no tidyverse; base subsetting instead of slice()/filter()
#   - per-window SDs are collected in preallocated vectors, not rbind()
#   - the original wrapped its guards in try() so they printed and continued;
#     here they are warnings with the same text, and the function continues.
#   - an unknown point_selection raised "object 'k' not found" in the
#     original; here it is a classed error with the same message.

#' V1 rolling-SD endpoint method (Cancers 2025 reference)
#'
#' @param w Window size in points.
#' @param exclusion_lower,exclusion_upper Exclusion zone bounds (deg C).
#' @param point_selection One of `"innermost"`, `"mid"`, `"outmost"`.
#' @export
ep_rolling_sd_v1 <- S7::new_class(
  "ep_rolling_sd_v1",
  parent = EndpointMethod,
  properties = list(
    w = S7::new_property(S7::class_numeric, default = 90),
    exclusion_lower = S7::new_property(S7::class_numeric, default = 60),
    exclusion_upper = S7::new_property(S7::class_numeric, default = 80),
    point_selection = S7::new_property(S7::class_character,
                                       default = "innermost")
  ),
  constructor = function(w = 90, exclusion_lower = 60, exclusion_upper = 80,
                         point_selection = "innermost") {
    S7::new_object(
      S7::S7_object(),
      id = "rolling_sd_v1", reference = TRUE,
      w = w, exclusion_lower = exclusion_lower,
      exclusion_upper = exclusion_upper, point_selection = point_selection
    )
  }
)

S7::method(detect_endpoints, ep_rolling_sd_v1) <- function(method, temperature,
                                                           dcp, ...) {
  check_thermogram_input(temperature, dcp)
  scan <- v1_scan(temperature, dcp, method@w, method@exclusion_lower,
                  method@exclusion_upper, method@point_selection)
  # The original passed possibly-vector endpoints straight on; with ties the
  # downstream `if (lwr.temp < ...)` fails in R >= 4.2. Mirror that by
  # refusing to subtract when ties produced more than one endpoint.
  if (length(scan$lower) != 1L || length(scan$upper) != 1L) {
    stop_method(sprintf(
      "tied minimum-SD windows produced %d lower and %d upper endpoints",
      length(scan$lower), length(scan$upper)))
  }
  bl <- subtract_baseline_v1(temperature, dcp, scan$lower, scan$upper)
  EndpointResult(
    lower = scan$lower, upper = scan$upper,
    baseline = bl$baseline, subtracted = bl$subtracted,
    diagnostics = list(
      lower_sd = scan$lower_sd, lower_stop = scan$lower_stop,
      upper_sd = scan$upper_sd, upper_start = scan$upper_start,
      k = scan$k
    )
  )
}

# Literal port of endpoint.detection(). Returns the per-window SD profiles
# alongside the endpoints so the vectorised method can be compared window
# by window.
v1_scan <- function(temperature, dcp, w, exclusion_lower, exclusion_upper,
                    point_selection) {
  k <- switch(point_selection,
    outmost = w,
    innermost = 0,
    mid = round(w / 2),
    NULL
  )
  if (is.null(k)) stop_method("object 'k' not found")

  n <- length(temperature)
  points_in_lower <- sum(temperature < exclusion_lower)
  points_in_upper <- sum(temperature > exclusion_upper)

  # Original guards: try(stop(...)) -- printed, never stopped.
  if (min(temperature) > exclusion_lower) warning("Exclusion zone is too low")
  if (max(temperature) < exclusion_upper) warning("Exclusion zone is too high")
  if (points_in_lower < w) warning("Not enough points. Need to lower w")
  if (points_in_upper < w) warning("Not enough points. Need to lower w")

  full_fit <- stats::smooth.spline(temperature, dcp, cv = TRUE)
  r <- stats::residuals(full_fit)

  # Lower scan: windows (1+i):(w+i) of exactly w points, while w+i < P.
  n_lower <- max(points_in_lower - w, 0L)
  lower_stop <- numeric(n_lower)
  lower_sd <- numeric(n_lower)
  i <- 0L
  while ((w + i) < points_in_lower) {
    idx <- (1L + i):(w + i)
    lower_stop[i + 1L] <- w + i
    lower_sd[i + 1L] <- stats::sd(r[idx])
    i <- i + 1L
  }
  if (n_lower == 0L) stop_method("no lower windows: not enough points below exclusion_lower")
  low <- which(lower_sd == min(lower_sd))          # all ties, scan order
  lower <- temperature[lower_stop[low] - k + 1]

  # Upper scan: windows (n-w-j):(n-j) of w+1 points, while n-w-j > S.
  smallest_point_in_upr <- n - points_in_upper
  n_upper <- max(n - w - smallest_point_in_upr, 0L)
  upper_start <- numeric(n_upper)
  upper_sd <- numeric(n_upper)
  j <- 0L
  while ((n - w - j) > smallest_point_in_upr) {
    idx <- (n - w - j):(n - j)
    upper_start[j + 1L] <- n - w - j
    upper_sd[j + 1L] <- stats::sd(r[idx])
    j <- j + 1L
  }
  if (n_upper == 0L) stop_method("no upper windows: not enough points above exclusion_upper")
  up <- which(upper_sd == min(upper_sd))
  upper <- temperature[upper_start[up] + k]

  list(lower = lower, upper = upper, k = k,
       lower_sd = lower_sd, lower_stop = lower_stop,
       upper_sd = upper_sd, upper_start = upper_start)
}

# Literal port of baseline.subtraction.byhand(), returning the fitted
# baseline that the original computed and then discarded.
subtract_baseline_v1 <- function(temperature, dcp, lower, upper) {
  if (lower < min(temperature) + 1) lower <- lower + 1
  if (upper > max(temperature) - 1) upper <- upper - 1

  lo <- temperature < lower
  hi <- temperature > upper
  mid <- temperature >= lower & temperature <= upper      # dplyr::between()

  if (sum(lo) < 4L || sum(hi) < 4L) {
    stop_method("fewer than 4 points outside the endpoints; cannot fit baseline splines")
  }

  sp_lo <- stats::smooth.spline(temperature[lo], dcp[lo], cv = TRUE)
  sp_hi <- stats::smooth.spline(temperature[hi], dcp[hi], cv = TRUE)
  # $y is the fit at the sorted unique x; input is sorted and unique so this
  # lines up positionally, exactly as the original assumed.
  fit_lo <- sp_lo$y
  fit_hi <- sp_hi$y

  connect <- data.frame(
    Temperature = c(max(temperature[lo]), min(temperature[hi])),
    fit = c(fit_lo[length(fit_lo)], fit_hi[1L])
  )
  connect_lm <- stats::lm(fit ~ Temperature, data = connect)
  fit_mid <- stats::predict(connect_lm,
                            data.frame(Temperature = temperature[mid]))
  fit_mid <- unname(fit_mid)

  # rbind(lower, connect, upper) then full_join by Temperature: with sorted
  # unique input the join is the identity ordering lo, mid, hi.
  baseline <- numeric(length(temperature))
  baseline[lo] <- fit_lo
  baseline[mid] <- fit_mid
  baseline[hi] <- fit_hi

  list(baseline = baseline, subtracted = dcp - baseline,
       lower_used = lower, upper_used = upper)
}

# Literal port of final.sample.interpolate().
interpolate_v1 <- function(temperature, y, grid) {
  fit <- stats::smooth.spline(temperature, y, cv = TRUE)
  stats::predict(fit, grid)$y
}

register_method(
  id = "rolling_sd_v1", kind = "endpoint", constructor = ep_rolling_sd_v1,
  label = "Rolling SD (Cancers 2025 reference)", reference = TRUE,
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
