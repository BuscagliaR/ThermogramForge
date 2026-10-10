# ==============================================================================
# Core S7 classes and generics
# ==============================================================================
# Every endpoint-detection method is an S7 object inheriting from
# EndpointMethod; detect_endpoints() dispatches on it and returns an
# EndpointResult. Signal classification follows the same pattern. Results are
# immutable values so that undo/redo in the app is just "keep the previous
# object".
#
# File is named aaa- so that it is sourced before the method files that
# register S7 methods on these generics (collate order is alphabetical).

#' Abstract parent for endpoint-detection methods
#'
#' Concrete methods add their own validated properties (window size,
#' exclusion bounds, ...).
#' @param id Registry key for the method.
#' @param reference `TRUE` for a frozen implementation kept for reproducing
#'   published results.
#' @export
EndpointMethod <- S7::new_class(
  "EndpointMethod",
  abstract = TRUE,
  properties = list(
    id = S7::class_character,
    reference = S7::new_property(S7::class_logical, default = FALSE)
  )
)

#' Result of endpoint detection on one thermogram
#'
#' @param lower,upper Endpoint temperatures. Length 1 normally; the V1
#'   reference method can return several when windows tie exactly.
#' @param baseline Fitted baseline on the raw temperature axis, or
#'   `numeric()` when the method does not produce one.
#' @param subtracted `dcp - baseline` on the raw temperature axis.
#' @param diagnostics Free-form list (rolling-sd profiles, tie counts, ...).
#' @export
EndpointResult <- S7::new_class(
  "EndpointResult",
  properties = list(
    lower = S7::class_double,
    upper = S7::class_double,
    baseline = S7::class_double,
    subtracted = S7::class_double,
    diagnostics = S7::class_list
  ),
  validator = function(self) {
    if (length(self@lower) == 0L) return("@lower must have length >= 1")
    if (length(self@upper) == 0L) return("@upper must have length >= 1")
    if (length(self@baseline) != length(self@subtracted)) {
      return("@baseline and @subtracted must have the same length")
    }
    NULL
  }
)

#' Detect baseline endpoints on one thermogram
#'
#' @param method An [EndpointMethod] object.
#' @param temperature,dcp Numeric vectors of equal length, sorted by
#'   temperature with unique values.
#' @param ... Passed to the method implementation.
#' @return An [EndpointResult].
#' @export
detect_endpoints <- S7::new_generic(
  "detect_endpoints", "method",
  function(method, temperature, dcp, ...) S7::S7_dispatch()
)

#' Abstract parent for signal-classification methods
#' @inheritParams EndpointMethod
#' @export
SignalMethod <- S7::new_class(
  "SignalMethod",
  abstract = TRUE,
  properties = list(
    id = S7::class_character,
    reference = S7::new_property(S7::class_logical, default = FALSE)
  )
)

#' Result of signal classification on one thermogram
#' @param has_signal Single logical.
#' @param diagnostics Free-form list (differencing order, model orders, ...).
#' @export
SignalResult <- S7::new_class(
  "SignalResult",
  properties = list(
    has_signal = S7::class_logical,
    diagnostics = S7::class_list
  ),
  validator = function(self) {
    if (length(self@has_signal) != 1L) return("@has_signal must have length 1")
    NULL
  }
)

#' Classify whether a baseline-subtracted thermogram carries a signal
#'
#' @param method A [SignalMethod] object.
#' @param temperature,dcp Numeric vectors (baseline-subtracted, typically on
#'   the interpolation grid).
#' @param ... Passed to the method implementation.
#' @return A [SignalResult].
#' @export
classify_signal <- S7::new_generic(
  "classify_signal", "method",
  function(method, temperature, dcp, ...) S7::S7_dispatch()
)

# Shared input check for every method. Raises classed errors so callers can
# distinguish bad input from algorithm failure.
check_thermogram_input <- function(temperature, dcp, call = rlang_caller()) {
  if (!is.numeric(temperature) || !is.numeric(dcp)) {
    stop_input("temperature and dcp must be numeric")
  }
  if (length(temperature) != length(dcp)) {
    stop_input("temperature and dcp must have the same length")
  }
  if (anyNA(temperature) || anyNA(dcp)) {
    stop_input("temperature and dcp must not contain NA")
  }
  if (is.unsorted(temperature, strictly = TRUE)) {
    stop_input("temperature must be strictly increasing (sorted, unique)")
  }
  invisible(TRUE)
}

rlang_caller <- function() NULL

stop_input <- function(msg) {
  stop(structure(
    class = c("thermogramforge_input_error", "error", "condition"),
    list(message = msg, call = NULL)
  ))
}

stop_method <- function(msg) {
  stop(structure(
    class = c("thermogramforge_method_error", "error", "condition"),
    list(message = msg, call = NULL)
  ))
}
