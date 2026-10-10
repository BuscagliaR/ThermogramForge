test_that("wide CSV with BOM reads into the right samples", {
  set <- read_thermogram_csv(raw_fixture("SuperSmall.csv"))
  expect_true(S7::S7_inherits(set, ThermogramSet))
  expect_identical(set@format, "wide")
  expect_identical(names(set@samples), c("1a", "1b"))
  expect_identical(nrow(set@samples[["1a"]]), 5389L)   # 5388 after the 20-110 filter
  expect_identical(n_samples(set), 2L)
})

test_that("wide CSV parses identically to V1's reader", {
  # The oracle stores V1's parsed (and temperature-filtered) vectors.
  set <- read_thermogram_csv(raw_fixture("SuperSmall.csv"))
  o <- readRDS(oracle_fixture("SuperSmall__default.rds"))
  for (id in names(o$samples)) {
    s <- set@samples[[id]]
    keep <- s$Temperature >= 20 & s$Temperature <= 110
    expect_identical(s$Temperature[keep], o$samples[[id]]$temperature_original, info = id)
    expect_identical(s$dCp[keep], o$samples[[id]]$dcp_original, info = id)
  }
})

test_that("Small.csv has 20 samples", {
  set <- read_thermogram_csv(raw_fixture("Small.csv"))
  expect_identical(n_samples(set), 20L)
})

test_that("long and single layouts read", {
  long <- read_thermogram_csv(raw_fixture("sample_multi_long.csv"))
  expect_identical(long@format, "long")
  expect_identical(n_samples(long), 3L)
  expect_identical(nrow(long@samples[[1L]]), 451L)

  single <- read_thermogram_csv(raw_fixture("sample_single_sample.csv"))
  expect_identical(single@format, "single")
  expect_identical(n_samples(single), 1L)
  expect_identical(nrow(single@samples[[1L]]), 451L)

  wide <- read_thermogram_csv(raw_fixture("sample_multi_wide.csv"))
  expect_identical(wide@format, "wide")
  expect_identical(n_samples(wide), 2L)
})

test_that("missing file and unknown layout are input errors", {
  expect_error(read_thermogram_csv("nope.csv"), class = "thermogramforge_input_error")
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), tmp)
  expect_error(read_thermogram_csv(tmp), class = "thermogramforge_input_error")
})
