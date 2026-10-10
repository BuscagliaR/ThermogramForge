# Every oracle file: the V1 port must reproduce V1's endpoints and signal
# flags exactly, and the vectorised default must reproduce the port's
# per-window SD profiles and endpoints bit for bit.

oracle_files <- list.files(fixture_path("oracle"), pattern = "__.*\\.rds$",
                           full.names = TRUE)

method_from_params <- function(ctor, p) {
  ctor(w = p$window_size, exclusion_lower = p$exclusion_lower,
       exclusion_upper = p$exclusion_upper,
       point_selection = p$point_selection)
}

for (f in oracle_files) {
  label <- sub("\\.rds$", "", basename(f))
  o <- readRDS(f)

  test_that(paste0("V1 port reproduces oracle endpoints: ", label), {
    skip_if(length(o$samples) == 0L, "oracle has no samples")
    m <- method_from_params(ep_rolling_sd_v1, o$params)
    for (id in names(o$samples)) {
      s <- o$samples[[id]]
      res <- suppressWarnings(
        detect_endpoints(m, s$temperature_original, s$dcp_original))
      expect_identical(res@lower, s$lower, info = paste(label, id, "lower"))
      expect_identical(res@upper, s$upper, info = paste(label, id, "upper"))
    }
  })

  test_that(paste0("vectorised method matches V1 port window by window: ", label), {
    skip_if(length(o$samples) == 0L, "oracle has no samples")
    m1 <- method_from_params(ep_rolling_sd_v1, o$params)
    m2 <- method_from_params(ep_rolling_sd, o$params)
    for (id in names(o$samples)) {
      s <- o$samples[[id]]
      r1 <- suppressWarnings(
        detect_endpoints(m1, s$temperature_original, s$dcp_original))
      r2 <- detect_endpoints(m2, s$temperature_original, s$dcp_original)
      expect_identical(r2@diagnostics$lower_sd, r1@diagnostics$lower_sd,
                       info = paste(label, id, "lower_sd"))
      expect_identical(r2@diagnostics$lower_stop, r1@diagnostics$lower_stop,
                       info = paste(label, id, "lower_stop"))
      expect_identical(r2@diagnostics$upper_sd, r1@diagnostics$upper_sd,
                       info = paste(label, id, "upper_sd"))
      expect_identical(r2@diagnostics$upper_start, r1@diagnostics$upper_start,
                       info = paste(label, id, "upper_start"))
      expect_identical(r2@lower, r1@lower, info = paste(label, id, "lower"))
      expect_identical(r2@upper, r1@upper, info = paste(label, id, "upper"))
      expect_identical(r2@baseline, r1@baseline, info = paste(label, id, "baseline"))
      expect_identical(r2@subtracted, r1@subtracted, info = paste(label, id, "subtracted"))
    }
  })
}

test_that("tied minimum-SD windows: keep_ties reproduces V1, default takes first", {
  # Constructed residuals: a strictly alternating +/-a pattern makes every
  # window of even width have the same sd, so every window ties. Width 4
  # (lower) and 5 (upper) differ, so break the pattern so only the lower
  # side ties.
  n <- 60L
  temperature <- seq_len(n) + 0
  r <- rep(c(0.1, -0.1), n / 2)
  r[31:60] <- (1:30)^2 * 1e-3      # upper side: convex, so window sds are unique
  P <- 30L; S <- 30L; w <- 4L; k <- 0

  all_ties <- scan_windows(r, temperature, w, k, P, S, keep_ties = TRUE)
  expect_identical(all_ties$diagnostics$n_ties_lower, P - w)
  expect_length(all_ties$lower, P - w)
  expect_identical(all_ties$diagnostics$n_ties_upper, 1L)
  expect_length(all_ties$upper, 1L)

  first <- scan_windows(r, temperature, w, k, P, S, keep_ties = FALSE)
  expect_length(first$lower, 1L)
  expect_identical(first$lower, all_ties$lower[1L])
  expect_identical(first$diagnostics$n_ties_lower, P - w)
})
