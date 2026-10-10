# The full V1 pipeline (endpoints -> baseline -> interpolation) must
# reproduce the oracle's baseline-subtracted curve on the grid.
# Strict identity locally; 1e-10 on CI (see helper-fixtures.R).

oracle_files <- list.files(fixture_path("oracle"), pattern = "__.*\\.rds$",
                           full.names = TRUE)

for (f in oracle_files) {
  label <- sub("\\.rds$", "", basename(f))
  o <- readRDS(f)

  test_that(paste0("pipeline reproduces oracle subtracted curve: ", label), {
    skip_if(length(o$samples) == 0L, "oracle has no samples")
    p <- o$params
    m <- ep_rolling_sd_v1(w = p$window_size, exclusion_lower = p$exclusion_lower,
                          exclusion_upper = p$exclusion_upper,
                          point_selection = p$point_selection)
    for (id in names(o$samples)) {
      s <- o$samples[[id]]
      res <- suppressWarnings(process_sample_v1(
        s$temperature_original, s$dcp_original, endpoint_method = m,
        grid = s$temperature_grid, temp_min = p$temp_min, temp_max = p$temp_max))
      expect_curve_equal(res$subtracted_grid, s$subtracted_grid)
      expect_identical(res$lower, s$lower)
      expect_identical(res$upper, s$upper)
    }
  })
}

test_that("the fitted baseline is returned and consistent with subtraction", {
  o <- readRDS(oracle_fixture("SuperSmall__default.rds"))
  s <- o$samples[[1L]]
  res <- suppressWarnings(detect_endpoints(ep_rolling_sd_v1(),
                                           s$temperature_original,
                                           s$dcp_original))
  expect_length(res@baseline, length(s$temperature_original))
  expect_identical(res@subtracted, s$dcp_original - res@baseline)
  # The baseline is a straight line between the inner spline ends.
  mid <- s$temperature_original >= res@lower & s$temperature_original <= res@upper
  fit <- stats::lm(res@baseline[mid] ~ s$temperature_original[mid])
  expect_lt(max(abs(stats::residuals(fit))), 1e-10)
})

test_that("endpoint nudging near the data edges matches V1", {
  temperature <- seq(20, 100, by = 0.1)
  dcp <- sin(temperature / 10)
  r <- subtract_baseline_v1(temperature, dcp, lower = 20.5, upper = 99.5)
  expect_identical(r$lower_used, 21.5)
  expect_identical(r$upper_used, 98.5)
})

test_that("the app-produced Small_RDS.rds agrees with the captured oracle", {
  # Small_RDS.rds was written by the V1 app on 2025-10-23 (R 4.5.1). The
  # oracle was re-captured with the same V1 code on 2026-10-09 (R 4.5.2).
  # Endpoints and flags are identical; the fitted curves differ at ~1e-15,
  # i.e. V1 itself is not bit-reproducible across R/BLAS builds. That is why
  # the project policy is "equivalent within tolerance" plus a frozen
  # reference implementation, rather than bit-identity across machines.
  app <- readRDS(oracle_fixture("app_Small_RDS.rds"))
  o <- readRDS(oracle_fixture("Small__default.rds"))
  for (id in names(app$samples)) {
    a <- app$samples[[id]]
    s <- o$samples[[id]]
    expect_identical(a$lower_endpoint, s$lower, info = id)
    expect_identical(a$upper_endpoint, s$upper, info = id)
    expect_identical(isTRUE(a$has_signal), s$has_signal, info = id)
    expect_equal(a$baseline_subtracted, s$subtracted_grid, tolerance = 1e-8)
  }
})
