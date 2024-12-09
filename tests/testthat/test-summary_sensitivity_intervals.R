test_that("summary.sensitivity_intervals prints expected output", {
  expect_s3_class(test_obj, "sensitivity_intervals")

  # Capture the printed output of summary
  summary_output <- capture.output(summary(test_obj))

  # Check that key headings and statistics are present
  expect_true(any(grepl("Sensitivity Interval Summary", summary_output)))
  expect_true(any(grepl("Widest Interval:", summary_output)))
  expect_true(any(grepl("Narrowest Interval:", summary_output)))
})
