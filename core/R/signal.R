# ==============================================================================
# Signal classification methods
# ==============================================================================
# V1 (ThermogramBaseline::signal.detection) fitted auto.arima() to the first
# differences and called the sample "Signal" when the model's differencing
# polynomial was non-empty, i.e. d >= 1. auto.arima fixes d with
# ndiffs(test = "kpss", alpha = 0.05) before any model search, so the
# classification is exactly the KPSS decision; the ARMA search only costs
# time. Both are kept: the reference for replication, the KPSS form as the
# default. test-signal.R asserts they agree on every fixture.

#' V1 auto.arima signal method (Cancers 2025 reference)
#' @importFrom forecast auto.arima ndiffs
#' @export
sig_auto_arima_v1 <- S7::new_class(
  "sig_auto_arima_v1",
  parent = SignalMethod,
  constructor = function() {
    S7::new_object(S7::S7_object(), id = "auto_arima_v1", reference = TRUE)
  }
)

S7::method(classify_signal, sig_auto_arima_v1) <- function(method, temperature,
                                                           dcp, ...) {
  d <- diff(dcp)
  fit <- forecast::auto.arima(d)
  delta <- fit$model$Delta
  SignalResult(
    has_signal = length(delta) > 0L,
    diagnostics = list(d = length(delta), arma = fit$arma)
  )
}

#' KPSS signal method (default)
#'
#' `forecast::ndiffs(diff(dcp), test = "kpss", alpha = 0.05) >= 1`. Agrees
#' with [sig_auto_arima_v1] by construction.
#' @param alpha KPSS significance level.
#' @export
sig_kpss <- S7::new_class(
  "sig_kpss",
  parent = SignalMethod,
  properties = list(alpha = S7::new_property(S7::class_numeric, default = 0.05)),
  constructor = function(alpha = 0.05) {
    S7::new_object(S7::S7_object(), id = "kpss", reference = FALSE,
                   alpha = alpha)
  }
)

S7::method(classify_signal, sig_kpss) <- function(method, temperature, dcp,
                                                  ...) {
  d <- diff(dcp)
  if (length(d) < 3L || isTRUE(all(d == d[1L]))) {
    return(SignalResult(has_signal = FALSE,
                        diagnostics = list(d = 0L, note = "constant series")))
  }
  nd <- forecast::ndiffs(d, test = "kpss", alpha = method@alpha)
  SignalResult(has_signal = nd >= 1L, diagnostics = list(d = nd))
}

register_method(
  id = "auto_arima_v1", kind = "signal", constructor = sig_auto_arima_v1,
  label = "auto.arima (Cancers 2025 reference)", reference = TRUE
)
register_method(
  id = "kpss", kind = "signal", constructor = sig_kpss,
  label = "KPSS stationarity test", default = TRUE,
  params = list(
    list(name = "alpha", type = "numeric", default = 0.05, min = 0.01,
         max = 0.2, label = "Significance level")
  )
)
