test_that("Function returns PSD covariance matrix for specified decay_param", {
  years <- c(2008, 2009, 2010)
  variances <- c(0.0001, 0.0002, 0.00015)
  decay_param <- 0.1
  result <- construct_cov_matrix_decay(
    years = years,
    variances = variances,
    decay_type = 'exponential',
    decay_param = decay_param
  )
  eigenvalues <- eigen(result$cov_matrix, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues >= -1e-8))
})

test_that("Function adjusts decay_param when initial value results in non-PSD matrix", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0.0001)
  decay_param <- -0.1  # Invalid decay_param
  expect_error(
    construct_cov_matrix_decay(
      years = years,
      variances = variances,
      decay_type = 'exponential',
      decay_param = decay_param
    ),
    "decay_param must be numeric and within"
  )
})

test_that("Function handles decay_param at bounds", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0.0001)
  decay_param <- 0  # Perfect correlation
  result <- construct_cov_matrix_decay(
    years = years,
    variances = variances,
    decay_type = 'exponential',
    decay_param = decay_param
  )
  expect_equal(result$decay_param, 0)
  expect_true(all(result$cov_matrix == (sqrt(variances) %o% sqrt(variances))))
})
