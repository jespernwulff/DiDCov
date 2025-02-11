library(testthat)

test_that("compute_bound returns a numeric value for valid inputs", {
  # We have 5 total time points -> 3 pre-periods + 2 post-periods = 5
  # HonestDiD requires numPrePeriods >= 3 to avoid subscript issues.
  betahat <- c(0.1, 0.2, 0.15, 0.25, 0.3)
  ci_lower <- betahat - 0.05
  ci_upper <- betahat + 0.05
  rho_values <- c(-0.5, 0, 0.5)

  # numPrePeriods = 3
  result <- compute_bound(
    betahat       = betahat,
    ci_lower      = ci_lower,
    ci_upper      = ci_upper,
    numPrePeriods = 3,
    rho_values    = rho_values,   # correlation assumptions
    bound_type    = "lower"       # could also test "upper"
  )

  # Should return one numeric value
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("compute_bound correctly applies aggregators", {
  betahat <- c(0.1, 0.2, 0.15, 0.25, 0.3)
  ci_lower <- betahat - 0.05
  ci_upper <- betahat + 0.05
  rho_values <- c(-0.5, 0, 0.5)

  smallest_bound <- compute_bound(
    betahat, ci_lower, ci_upper,
    numPrePeriods = 3,
    rho_values    = rho_values,
    aggregator    = "smallest",
    bound_type    = "lower"
  )
  largest_bound <- compute_bound(
    betahat, ci_lower, ci_upper,
    numPrePeriods = 3,
    rho_values    = rho_values,
    aggregator    = "largest",
    bound_type    = "lower"
  )
  median_bound <- compute_bound(
    betahat, ci_lower, ci_upper,
    numPrePeriods = 3,
    rho_values    = rho_values,
    aggregator    = "median",
    bound_type    = "lower"
  )

  # We do not know exact numeric values,
  # but "smallest" <= "median" <= "largest" should generally hold.
  expect_true(smallest_bound <= median_bound && median_bound <= largest_bound)
})

test_that("compute_bound distinguishes lower and upper bounds", {
  betahat <- c(0.1, 0.2, 0.15, 0.25, 0.3)
  ci_lower <- betahat - 0.05
  ci_upper <- betahat + 0.05
  rho_values <- c(0.5)  # Just one rho value to keep it simple

  lower_bound <- compute_bound(
    betahat, ci_lower, ci_upper,
    numPrePeriods = 3,
    rho_values    = rho_values,
    bound_type    = "lower"
  )
  upper_bound <- compute_bound(
    betahat, ci_lower, ci_upper,
    numPrePeriods = 3,
    rho_values    = rho_values,
    bound_type    = "upper"
  )

  # Usually, the lower bound is <= the upper bound
  expect_true(lower_bound <= upper_bound)
})

test_that("compute_bound errors on mismatched input lengths", {
  betahat  <- c(0.1, 0.2, 0.3, 0.4)    # length 4
  ci_lower <- c(0.05, 0.15, 0.25)      # length 3
  ci_upper <- c(0.15, 0.25, 0.35)      # length 3

  expect_error(
    compute_bound(
      betahat, ci_lower, ci_upper,
      numPrePeriods = 3,
      rho_values    = c(0.5)
    ),
    "betahat, ci_lower, and ci_upper must have the same length."
  )
})

test_that("compute_bound errors when ci_lower > ci_upper", {
  betahat  <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  ci_lower <- betahat + 0.01     # always above betahat
  ci_upper <- betahat - 0.01     # always below betahat

  expect_error(
    compute_bound(
      betahat, ci_lower, ci_upper,
      numPrePeriods = 3,
      rho_values    = c(0)
    ),
    "Each ci_lower must be <= the corresponding ci_upper."
  )
})



