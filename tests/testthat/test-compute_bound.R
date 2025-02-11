test_that("compute_bound works with reference data example", {
  betahat <- c(
    -625.41919, -67.62180, -124.48790, -79.65192, 75.70340,
    247.32845, 504.78656, 1091.64929, 1315.04797, 1208.80237,
    1173.42163, 1059.37341, 1337.43188, 1877.21277, 1644.50647,
    1513.41162, 987.30634, 844.51910, 679.04529, 594.85004,
    919.84174, 699.61328, 778.14233, 1031.26465, 501.77036
  )

  ci_lower <- c(
    -1512.259036, -588.665658, -511.946828, -440.293910, -214.670648,
    69.463211, 162.198384, 654.200277, 770.835210, 571.565122,
    348.439424, 316.510702, 495.737773, 375.105111, 252.128748,
    118.265747, -511.282963, -658.482012, -493.256353, -314.834611,
    9.135403, -313.022139, -251.185135, -476.305708, -597.485377
  )

  ci_upper <- c(
    261.4207, 453.4221, 262.9710, 280.9901, 366.0774,
    425.1937, 847.3747, 1529.0983, 1859.2607, 1846.0396,
    1998.4038, 1802.2361, 2179.1260, 3379.3204, 3036.8842,
    2908.5575, 2485.8956, 2347.5202, 1851.3469, 1504.5347,
    1830.5481, 1712.2487, 1807.4698, 2538.8350, 1601.0261
  )

  numPrePeriods <- 5  # as in your example
  rho_value     <- 0.5

  # We'll expect a single bound, because we pass a single rho_value
  # with aggregator = "largest" (which doesn't matter if there's only one rho)
  bound_result <- compute_bound(
    betahat       = betahat,
    ci_lower      = ci_lower,
    ci_upper      = ci_upper,
    numPrePeriods = numPrePeriods,
    rho_values    = rho_value,
    aggregator    = "largest",
    bound_type    = "upper",
    scale         = TRUE
  )

  # From your line-by-line approach, the result was ~16.19611
  # We'll allow some numerical tolerance
  expect_equal(bound_result, 16.19611, tolerance = 1e-5)
})

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



