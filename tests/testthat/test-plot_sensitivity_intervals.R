test_that("plot.sensitivity_intervals returns a ggplot object without honest_data", {
  # test_obj is a simple sensitivity_intervals object defined in a helper
  expect_s3_class(test_obj, "sensitivity_intervals")

  plt <- plot(test_obj)
  expect_s3_class(plt, "ggplot")
})
