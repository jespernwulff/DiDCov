test_that("plot.sensitivity_intervals returns a ggplot object", {
  # Original simple test object without honest_data
  expect_s3_class(test_obj, "sensitivity_intervals")
  plt <- plot(test_obj)
  expect_s3_class(plt, "ggplot")

  # Test with honest_data
  expect_s3_class(test_obj_with_honest, "sensitivity_intervals")
  plt_honest <- plot(test_obj_with_honest, honest_data = honest_data)
  expect_s3_class(plt_honest, "ggplot")

  # Test with multiple Mbar values
  expect_s3_class(test_obj_multi_mbar, "sensitivity_intervals")
  plt_multi <- plot(test_obj_multi_mbar)
  expect_s3_class(plt_multi, "ggplot")

  # Test with multiple Mbar values and honest_data
  plt_multi_honest <- plot(test_obj_multi_mbar, honest_data = honest_data_multi)
  expect_s3_class(plt_multi_honest, "ggplot")

  # Additional test with detailed Mbar range
  result <- generate_multi_mbar_test_obj()
  plt_detailed <- plot(result$test_obj, honest_data = result$honest_data)
  expect_s3_class(plt_detailed, "ggplot")
})
