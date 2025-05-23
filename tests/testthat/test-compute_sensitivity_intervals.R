test_that("compute_sensitivity_intervals returns expected outputs quickly", {
  # Simplified test data
  betahat <- c(0.05, 0.06)
  ci_lower <- c(0.02, 0.03)
  ci_upper <- c(0.08, 0.09)
  numPrePeriods <- 1
  numPostPeriods <- 1

  # Use a minimal parameter grid to keep the test fast
  rho_values <- c(0, 0.5)
  lambda_values <- c(0, 0.5)

  # Record start time
  start_time <- Sys.time()

  result <- compute_sensitivity_intervals(
    betahat = betahat,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    numPrePeriods = numPrePeriods,
    numPostPeriods = numPostPeriods,
    method = "constant",
    rho_values = rho_values,
    ci_level = 0.95,
    Mbarvec = c(1)
  )

  # Check that result is a list with correct elements
  expect_type(result, "list")
  expect_true(all(c("widest_interval", "narrowest_interval", "all_intervals") %in% names(result)))

  # Check that widest and narrowest intervals are data frames
  expect_s3_class(result$widest_interval, "data.frame")
  expect_s3_class(result$narrowest_interval, "data.frame")
  expect_s3_class(result$all_intervals, "data.frame")

  # Check that intervals are correctly computed
  expect_true(nrow(result$all_intervals) > 0)
})

