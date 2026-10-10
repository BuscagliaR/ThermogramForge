small_sample <- function() {
  o <- readRDS(oracle_fixture("SuperSmall__default.rds"))
  o$samples[[1L]]
}

test_that("point_selection maps to k exactly as V1", {
  s <- small_sample()
  inner <- suppressWarnings(v1_scan(s$temperature_original, s$dcp_original,
                                    90, 60, 80, "innermost"))
  mid <- suppressWarnings(v1_scan(s$temperature_original, s$dcp_original,
                                  90, 60, 80, "mid"))
  out <- suppressWarnings(v1_scan(s$temperature_original, s$dcp_original,
                                  90, 60, 80, "outmost"))
  expect_identical(inner$k, 0)
  expect_identical(mid$k, 45)
  expect_identical(out$k, 90)
  # Same windows are chosen; only the offset differs.
  expect_identical(inner$lower_sd, mid$lower_sd)
  expect_identical(inner$upper_sd, out$upper_sd)
  # round-half-even as in base R
  expect_identical(suppressWarnings(v1_scan(s$temperature_original,
                                            s$dcp_original, 7, 60, 80,
                                            "mid"))$k, 4)
})

test_that("unknown point_selection reproduces V1's failure", {
  s <- small_sample()
  expect_error(
    v1_scan(s$temperature_original, s$dcp_original, 90, 60, 80, "outermost"),
    "object 'k' not found", class = "thermogramforge_method_error")
  expect_error(ep_rolling_sd(point_selection = "outermost"),
               "innermost|mid|outmost")
})

test_that("too few points for the window is an error in the default, a warning in V1", {
  s <- small_sample()
  keep <- s$temperature_original > 59.5     # ~50 points below 60 < w = 90
  expect_warning(
    expect_error(v1_scan(s$temperature_original[keep], s$dcp_original[keep],
                         90, 60, 80, "innermost"),
                 class = "thermogramforge_method_error"),
    "Not enough points")
  expect_error(
    detect_endpoints(ep_rolling_sd(), s$temperature_original[keep],
                     s$dcp_original[keep]),
    class = "thermogramforge_input_error")
})

test_that("input validation rejects unsorted or NA input", {
  s <- small_sample()
  expect_error(detect_endpoints(ep_rolling_sd(), rev(s$temperature_original),
                                s$dcp_original),
               class = "thermogramforge_input_error")
  d <- s$dcp_original
  d[10] <- NA
  expect_error(detect_endpoints(ep_rolling_sd(), s$temperature_original, d),
               class = "thermogramforge_input_error")
})

test_that("method objects validate their parameters at construction", {
  expect_error(ep_rolling_sd(w = 2))
  expect_error(ep_rolling_sd(exclusion_lower = 80, exclusion_upper = 60))
  m <- ep_rolling_sd(w = 50L)
  expect_identical(m@w, 50L)
  expect_false(m@reference)
  expect_true(ep_rolling_sd_v1()@reference)
})
