test_that("Function computes variances correctly with symmetric confidence intervals", {
  years <- c(2008, 2009, 2010)
  effect_sizes <- c(0.1, 0.15, 0.2)
  ci_lower <- c(0.05, 0.1, 0.15)
  ci_upper <- c(0.15, 0.2, 0.25)
  ci_level <- 0.95

  result <- compute_variances_from_ci(years, effect_sizes, ci_lower, ci_upper, ci_level)

  z_value <- qnorm(1 - (1 - ci_level) / 2)
  expected_se <- (ci_upper - ci_lower) / (2 * z_value)
  expected_variances <- expected_se^2

  expect_equal(result$variances, expected_variances)
  expect_equal(result$years, years)
})

test_that("Function handles asymmetric confidence intervals", {
  years <- c(2008, 2009)
  effect_sizes <- c(0.2, 0.3)
  ci_lower <- c(0.1, 0.25)
  ci_upper <- c(0.25, 0.35)
  ci_level <- 0.95

  result <- compute_variances_from_ci(years, effect_sizes, ci_lower, ci_upper, ci_level)

  z_value <- qnorm(1 - (1 - ci_level) / 2)
  expected_se <- (ci_upper - ci_lower) / (2 * z_value)
  expected_variances <- expected_se^2

  expect_equal(result$variances, expected_variances)
})

test_that("Function handles invalid input lengths", {
  years <- c(2008, 2009)
  effect_sizes <- c(0.1)
  ci_lower <- c(0.05, 0.1)
  ci_upper <- c(0.15, 0.2)

  expect_error(
    compute_variances_from_ci(years, effect_sizes, ci_lower, ci_upper),
    "All input vectors .* must have the same length."
  )
})

test_that("Function handles ci_upper less than ci_lower", {
  years <- c(2008)
  effect_sizes <- c(0.1)
  ci_lower <- c(0.15)
  ci_upper <- c(0.05)

  expect_error(
    compute_variances_from_ci(years, effect_sizes, ci_lower, ci_upper),
    "Each element of ci_upper must be greater than or equal to the corresponding element of ci_lower."
  )
})

test_that("Function handles invalid ci_level", {
  years <- c(2008)
  effect_sizes <- c(0.1)
  ci_lower <- c(0.05)
  ci_upper <- c(0.15)

  expect_error(
    compute_variances_from_ci(years, effect_sizes, ci_lower, ci_upper, ci_level = 1.5),
    "ci_level must be a numeric value between 0 and 1 \\(exclusive\\)."
  )
})

test_that("Function works with different confidence levels", {
  years <- c(2008)
  effect_sizes <- c(0.1)
  ci_lower <- c(0.07)
  ci_upper <- c(0.13)
  ci_level <- 0.8

  result <- compute_variances_from_ci(years, effect_sizes, ci_lower, ci_upper, ci_level)

  z_value <- qnorm(1 - (1 - ci_level) / 2)
  expected_se <- (ci_upper - ci_lower) / (2 * z_value)
  expected_variances <- expected_se^2

  expect_equal(result$variances, expected_variances)
})
