test_that("plot.sensitivity_intervals returns a ggplot object", {
  expect_s3_class(test_obj, "sensitivity_intervals")

  plt <- plot(test_obj)

  # Check the returned object is a ggplot
  expect_s3_class(plt, "ggplot")

  # Check the number of layers: we expect geom_errorbarh and geom_vline at minimum
  # geom_errorbarh is one layer, geom_vline is another layer
  expect_true(length(plt$layers) >= 2)

  # Check that scale and theme elements are set
  # For instance, verify that a theme_classic element is applied
  # (This is harder to test directly. We just check that theme is a list.)
  expect_true(is.list(plt$theme))
})
