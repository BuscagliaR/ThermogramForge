test_that("committed fixtures are present", {
  expect_true(file.exists(raw_fixture("Small.csv")))
  expect_true(file.exists(raw_fixture("SuperSmall.csv")))
  expect_true(file.exists(raw_fixture("sample_multi_long.csv")))
  expect_true(file.exists(raw_fixture("sample_single_sample.csv")))
  expect_true(file.exists(raw_fixture("sample_multi_wide.csv")))
  expect_true(file.exists(oracle_fixture("app_Small_RDS.rds")))
})
