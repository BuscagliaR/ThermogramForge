# Paths to committed fixtures and the numeric-equality policy.
#
# The oracle was produced by V1 on one machine (its platform is recorded in
# each oracle file's `versions$platform`). On that platform, with the same
# R build, results are bit-identical and the tests demand it. Anywhere else
# two things move at the last bit and get amplified by smooth.spline's
# cross-validated lambda search: base read.csv's number parsing (not
# correctly rounded on every platform) and the BLAS. The CI run on
# 2026-10-09 (x86_64 linux, R 4.5.3) showed curve differences up to ~2e-7
# dCp on the short-segment parameter sets (outmost / mid / w120) and 1-ulp
# input differences. The cross-platform policy is therefore an absolute
# bound, stated in dCp units, that is far below anything a metric can see.
#
# Endpoint temperatures and signal flags are always compared exactly.

fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}

raw_fixture <- function(name) fixture_path("raw", name)
oracle_fixture <- function(name) fixture_path("oracle", name)

on_ci <- function() isTRUE(as.logical(Sys.getenv("CI", "false")))

# Bit-identity is demanded only on the platform that produced the oracle.
# Set TF_STRICT_ORACLE=true to force it, or =false to force tolerance.
strict_oracle <- function(oracle_platform = NULL) {
  forced <- Sys.getenv("TF_STRICT_ORACLE", "")
  if (nzchar(forced)) return(isTRUE(as.logical(forced)))
  !on_ci() && !is.null(oracle_platform) &&
    identical(R.version$platform, oracle_platform)
}

# Absolute bounds for the tolerant mode.
curve_abs_tol <- 1e-5      # dCp units; peaks are ~0.05-0.3
input_abs_tol <- 1e-9      # parsed raw values; 1-ulp differences are ~1e-15

expect_curve_equal <- function(object, expected, oracle_platform = NULL) {
  testthat::expect_identical(length(object), length(expected))
  if (strict_oracle(oracle_platform)) {
    testthat::expect_identical(object, expected)
  } else {
    testthat::expect_lt(max(abs(object - expected)), curve_abs_tol)
  }
}

expect_input_equal <- function(object, expected, oracle_platform = NULL) {
  testthat::expect_identical(length(object), length(expected))
  if (strict_oracle(oracle_platform)) {
    testthat::expect_identical(object, expected)
  } else {
    testthat::expect_lt(max(abs(object - expected)), input_abs_tol)
  }
}
