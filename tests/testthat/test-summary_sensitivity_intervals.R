test_that("summary.sensitivity_intervals works for widest and narrowest", {
  # 'test_obj' is created in a helper file and is a valid 'sensitivity_intervals' object

  # 1. Default summary (widest)
  widest_output <- capture.output(summary(test_obj))
  expect_true(any(grepl("WIDEST intervals", widest_output)),
              "Default summary should mention 'WIDEST intervals'")
  # Check mention of columns
  expect_true(any(grepl("lb", widest_output)), "Output should mention 'lb'")
  expect_true(any(grepl("ub", widest_output)), "Output should mention 'ub'")

  # 2. Narrowest
  narrowest_output <- capture.output(summary(test_obj, type = "narrowest"))
  expect_true(any(grepl("NARROWEST intervals", narrowest_output)),
              "Narrowest summary should mention 'NARROWEST intervals'")
  # Check mention of columns
  expect_true(any(grepl("lb", narrowest_output)), "Output should mention 'lb'")
  expect_true(any(grepl("ub", narrowest_output)), "Output should mention 'ub'")
})
