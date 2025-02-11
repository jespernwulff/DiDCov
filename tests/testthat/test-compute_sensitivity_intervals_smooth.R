test_that("compute_sensitivity_intervals_smooth runs with benchmark aggregator (largest)", {
  # Reference data
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

  numPrePeriods  <- 5
  numPostPeriods <- length(betahat) - numPrePeriods
  rho_value      <- 0.5

  # We'll suppress known warnings from HonestDiD
  suppressWarnings({
    out <- compute_sensitivity_intervals_smooth(
      betahat       = betahat,
      ci_lower      = ci_lower,
      ci_upper      = ci_upper,
      numPrePeriods = numPrePeriods,
      numPostPeriods= numPostPeriods,
      method        = "constant",
      rho_values    = rho_value,          # single value
      benchmark_aggregator = "largest",   # compute an upper bound & multiply Mvec
      scale         = TRUE
    )
  })

  # Basic checks on the returned structure
  expect_s3_class(out, "sensitivity_intervals")
  expect_true(all(c("widest_interval", "narrowest_interval", "all_intervals") %in% names(out)))

  if (nrow(out$all_intervals) > 0) {
    expect_true(all(c("lb", "ub", "interval_width") %in% colnames(out$all_intervals)))
  }
})

