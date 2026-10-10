# Paths to committed fixtures and the numeric-equality policy.
#
# Locally (same machine/BLAS that produced the oracle) results are expected
# to be bit-identical to V1. On CI, smooth.spline can differ at the 1e-12
# level across BLAS builds, so comparisons of fitted curves use a tolerance
# there. Endpoint temperatures and signal flags are always compared exactly.

fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}

raw_fixture <- function(name) fixture_path("raw", name)
oracle_fixture <- function(name) fixture_path("oracle", name)

on_ci <- function() isTRUE(as.logical(Sys.getenv("CI", "false")))

curve_tolerance <- function() if (on_ci()) 1e-10 else 0

expect_curve_equal <- function(object, expected) {
  tol <- curve_tolerance()
  if (tol == 0) {
    testthat::expect_identical(object, expected)
  } else {
    testthat::expect_equal(object, expected, tolerance = tol)
  }
}
