test_that("Function returns correct covariance matrix with exponential decay", {
  years <- c(2000, 2001, 2002)
  variances <- c(1, 1, 1)
  lambda <- 0.1

  cov_matrix <- construct_cov_matrix_decay(years, variances, decay_type = "exponential", lambda)

  time_diff_matrix <- as.matrix(dist(years, diag = TRUE, upper = TRUE))
  corr_matrix <- exp(-lambda * time_diff_matrix)
  sd_vec <- sqrt(variances)
  expected_cov_matrix <- corr_matrix * (sd_vec %o% sd_vec)

  # Set dimnames on expected_cov_matrix to match cov_matrix
  dimnames(expected_cov_matrix) <- dimnames(cov_matrix)

  expect_equal(cov_matrix, expected_cov_matrix)
})

test_that("Function returns correct covariance matrix with linear decay", {
  years <- c(2000, 2001, 2002)
  variances <- c(1, 1, 1)
  lambda <- 0.5

  cov_matrix <- construct_cov_matrix_decay(years, variances, decay_type = "linear", lambda)

  time_diff_matrix <- as.matrix(dist(years, diag = TRUE, upper = TRUE))
  corr_matrix <- pmax(1 - lambda * time_diff_matrix, 0)
  sd_vec <- sqrt(variances)
  expected_cov_matrix <- corr_matrix * (sd_vec %o% sd_vec)

  # Set dimnames on expected_cov_matrix to match cov_matrix
  dimnames(expected_cov_matrix) <- dimnames(cov_matrix)

  expect_equal(cov_matrix, expected_cov_matrix)
})

test_that("Function handles invalid decay_type inputs", {
  years <- c(2000, 2001)
  variances <- c(1, 1)

  expect_error(
    construct_cov_matrix_decay(years, variances, decay_type = "invalid", lambda = 0.1),
    "decay_type must be either 'exponential' or 'linear'."
  )
})

test_that("Function handles invalid lambda inputs", {
  years <- c(2000, 2001)
  variances <- c(1, 1)

  expect_error(
    construct_cov_matrix_decay(years, variances, decay_type = "exponential", lambda = -0.1),
    "lambda must be a non-negative numeric value."
  )

  expect_error(
    construct_cov_matrix_decay(years, variances, decay_type = "linear", lambda = "invalid"),
    "lambda must be a non-negative numeric value."
  )

  expect_error(
    construct_cov_matrix_decay(years, variances, decay_type = "linear", lambda = NA),
    "lambda must be a non-negative numeric value."
  )
})

test_that("Function handles negative variances", {
  years <- c(2000, 2001)
  variances <- c(1, -1)

  expect_error(
    construct_cov_matrix_decay(years, variances, decay_type = "exponential", lambda = 0.1),
    "Variances must be non-negative."
  )
})

test_that("Function handles mismatched lengths of years and variances", {
  years <- c(2000, 2001)
  variances <- c(1)

  expect_error(
    construct_cov_matrix_decay(years, variances, decay_type = "linear", lambda = 0.1),
    "The lengths of years and variances must match."
  )
})

test_that("Covariance matrix is PSD for various lambda values and decay types", {
  years <- c(2000, 2001, 2002, 2003)
  variances <- c(1, 2, 3, 4)
  lambda_values <- c(0, 0.1, 1, 10)
  decay_types <- c("exponential", "linear")

  for (decay_type in decay_types) {
    for (lambda in lambda_values) {
      cov_matrix <- construct_cov_matrix_decay(years, variances, decay_type = decay_type, lambda = lambda)

      # Check that covariance matrix is PSD
      eigenvalues <- eigen(cov_matrix, symmetric = TRUE, only.values = TRUE)$values
      expect_true(all(eigenvalues >= -1e-8), info = paste("lambda =", lambda, "decay_type =", decay_type))
    }
  }
})

test_that("Function handles zero variances appropriately", {
  years <- c(2000, 2001)
  variances <- c(1, 0)

  cov_matrix <- construct_cov_matrix_decay(years, variances, decay_type = "linear", lambda = 0.1)

  time_diff_matrix <- as.matrix(dist(years, diag = TRUE, upper = TRUE))
  corr_matrix <- pmax(1 - 0.1 * time_diff_matrix, 0)
  sd_vec <- sqrt(variances)
  expected_cov_matrix <- corr_matrix * (sd_vec %o% sd_vec)

  # Set dimnames on expected_cov_matrix to match cov_matrix
  dimnames(expected_cov_matrix) <- dimnames(cov_matrix)

  expect_equal(cov_matrix, expected_cov_matrix)
})

test_that("Function handles single-year input", {
  years <- c(2000)
  variances <- c(1)

  cov_matrix <- construct_cov_matrix_decay(years, variances, decay_type = "exponential", lambda = 0.1)

  expected_cov_matrix <- matrix(variances)

  # Set dimnames on expected_cov_matrix to match cov_matrix
  dimnames(expected_cov_matrix) <- dimnames(cov_matrix)

  expect_equal(cov_matrix, expected_cov_matrix)
})

test_that("Function computes covariance matrix correctly with large lambda", {
  years <- c(2000, 2001, 2002)
  variances <- c(1, 2, 3)
  lambda <- 100  # Large lambda should result in near-zero off-diagonal elements

  cov_matrix_exp <- construct_cov_matrix_decay(years, variances, decay_type = "exponential", lambda = lambda)
  cov_matrix_lin <- construct_cov_matrix_decay(years, variances, decay_type = "linear", lambda = lambda)

  expected_cov_matrix <- diag(variances)

  # Set dimnames on expected_cov_matrix to match cov_matrix
  dimnames(expected_cov_matrix) <- dimnames(cov_matrix_exp)

  expect_equal(cov_matrix_exp, expected_cov_matrix, tolerance = 1e-6)
  expect_equal(cov_matrix_lin, expected_cov_matrix, tolerance = 1e-6)
})

test_that("Covariance matrix is symmetric", {
  years <- c(2000, 2001, 2002)
  variances <- c(1, 1, 1)
  lambda <- 0.5

  cov_matrix_exp <- construct_cov_matrix_decay(years, variances, decay_type = "exponential", lambda = lambda)
  cov_matrix_lin <- construct_cov_matrix_decay(years, variances, decay_type = "linear", lambda = lambda)

  expect_true(isSymmetric(cov_matrix_exp))
  expect_true(isSymmetric(cov_matrix_lin))
})
