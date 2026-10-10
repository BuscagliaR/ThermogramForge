oracle_files <- list.files(fixture_path("oracle"), pattern = "__.*\\.rds$",
                           full.names = TRUE)

for (f in oracle_files) {
  label <- sub("\\.rds$", "", basename(f))
  o <- readRDS(f)

  test_that(paste0("signal methods agree with each other and the oracle: ", label), {
    skip_if(length(o$samples) == 0L, "oracle has no samples")
    for (id in names(o$samples)) {
      s <- o$samples[[id]]
      v1 <- classify_signal(sig_auto_arima_v1(), s$temperature_grid, s$subtracted_grid)
      kp <- classify_signal(sig_kpss(), s$temperature_grid, s$subtracted_grid)
      expect_identical(v1@has_signal, s$has_signal, info = paste(label, id, "v1"))
      expect_identical(kp@has_signal, v1@has_signal, info = paste(label, id, "kpss"))
    }
  })
}

test_that("constant series is No Signal for both methods", {
  g <- seq(45, 90, by = 0.1)
  flat <- rep(0.3, length(g))
  expect_false(classify_signal(sig_auto_arima_v1(), g, flat)@has_signal)
  expect_false(classify_signal(sig_kpss(), g, flat)@has_signal)
})

test_that("white noise is No Signal", {
  # (A noiseless Gaussian peak is also classified stationary by KPSS on the
  # first differences; the method is a screen for real, noisy data, so the
  # positive cases are covered by the oracle fixtures above.)
  g <- seq(45, 90, by = 0.1)
  set.seed(42)
  noise <- rnorm(length(g), sd = 0.01)
  expect_false(classify_signal(sig_kpss(), g, noise)@has_signal)
  expect_false(classify_signal(sig_auto_arima_v1(), g, noise)@has_signal)
})
