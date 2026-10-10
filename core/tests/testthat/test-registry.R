test_that("built-in methods are registered with the expected roles", {
  ep <- list_methods("endpoint")
  expect_true(all(c("rolling_sd", "rolling_sd_v1") %in% names(ep)))
  expect_true(ep$rolling_sd_v1$reference)
  expect_false(ep$rolling_sd$reference)
  expect_identical(default_method_id("endpoint"), "rolling_sd")

  sg <- list_methods("signal")
  expect_true(all(c("kpss", "auto_arima_v1") %in% names(sg)))
  expect_identical(default_method_id("signal"), "kpss")
})

test_that("make_method builds objects from spec defaults and overrides", {
  m <- make_method("rolling_sd", "endpoint")
  expect_true(S7::S7_inherits(m, ep_rolling_sd))
  expect_true(S7::S7_inherits(m, EndpointMethod))
  expect_identical(m@w, 90L)
  m2 <- make_method("rolling_sd", "endpoint", w = 50L, point_selection = "mid")
  expect_identical(m2@w, 50L)
  expect_identical(m2@point_selection, "mid")
  expect_error(get_method("nope", "endpoint"),
               class = "thermogramforge_input_error")
})

test_that("param specs carry what a UI needs", {
  spec <- get_method("rolling_sd", "endpoint")$params
  nms <- vapply(spec, `[[`, "", "name")
  expect_setequal(nms, c("w", "exclusion_lower", "exclusion_upper",
                         "point_selection"))
  choice <- spec[[which(nms == "point_selection")]]
  expect_identical(choice$type, "choice")
  expect_setequal(choice$choices, c("innermost", "mid", "outmost"))
  for (p in spec) expect_true(all(c("label", "default") %in% names(p)))
})
