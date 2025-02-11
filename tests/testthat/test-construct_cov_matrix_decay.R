test_that("Function returns correct covariance matrix with exponential decay", {
  variances <- c(1, 1, 1)
  lambda <- 0.1
  time_diff_matrix <- matrix(c(0, 1, 2, 1, 0, 1, 2, 1, 0), nrow = 3)

  cov_matrix <- construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "exponential", lambda)

  corr_matrix <- exp(-lambda * time_diff_matrix)
  sd_vec <- sqrt(variances)
  expected_cov_matrix <- corr_matrix * (sd_vec %o% sd_vec)

  expect_equal(cov_matrix, expected_cov_matrix)
})

test_that("Function returns correct covariance matrix with linear decay", {
  variances <- c(1, 1, 1)
  lambda <- 0.5
  time_diff_matrix <- matrix(c(0, 1, 2, 1, 0, 1, 2, 1, 0), nrow = 3)

  cov_matrix <- construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "linear", lambda)

  corr_matrix <- pmax(1 - lambda * time_diff_matrix, 0)
  sd_vec <- sqrt(variances)
  expected_cov_matrix <- corr_matrix * (sd_vec %o% sd_vec)

  expect_equal(cov_matrix, expected_cov_matrix)
})

test_that("Function handles invalid decay_type inputs", {
  variances <- c(1, 1)
  time_diff_matrix <- matrix(c(0, 1, 1, 0), nrow = 2)

  expect_error(
    construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "invalid", lambda = 0.1),
    "decay_type must be either 'exponential' or 'linear'."
  )
})

test_that("Function handles invalid lambda inputs", {
  variances <- c(1, 1)
  time_diff_matrix <- matrix(c(0, 1, 1, 0), nrow = 2)

  expect_error(
    construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "exponential", lambda = -0.1),
    "lambda must be a non-negative numeric value."
  )

  expect_error(
    construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "linear", lambda = "invalid"),
    "lambda must be a non-negative numeric value."
  )

  expect_error(
    construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "linear", lambda = NA),
    "lambda must be a non-negative numeric value."
  )
})

test_that("Function handles negative variances", {
  variances <- c(1, -1)
  time_diff_matrix <- matrix(c(0, 1, 1, 0), nrow = 2)

  expect_error(
    construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "exponential", lambda = 0.1),
    "Variances must be non-negative."
  )
})

test_that("Covariance matrix is PSD for various lambda values and decay types", {
  variances <- c(1, 2, 3, 4)
  time_diff_matrix <- matrix(c(0, 1, 2, 3, 1, 0, 1, 2, 2, 1, 0, 1, 3, 2, 1, 0), nrow = 4)
  lambda_values <- c(0, 0.1, 1, 10)
  decay_types <- c("exponential", "linear")

  for (decay_type in decay_types) {
    for (lambda in lambda_values) {
      cov_matrix <- construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = decay_type, lambda = lambda)

      # Check that covariance matrix is PSD
      eigenvalues <- eigen(cov_matrix, symmetric = TRUE, only.values = TRUE)$values
      expect_true(all(eigenvalues >= -1e-8), info = paste("lambda =", lambda, "decay_type =", decay_type))
    }
  }
})

test_that("Function handles zero variances appropriately", {
  variances <- c(1, 0)
  time_diff_matrix <- matrix(c(0, 1, 1, 0), nrow = 2)

  cov_matrix <- construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "linear", lambda = 0.1)

  corr_matrix <- pmax(1 - 0.1 * time_diff_matrix, 0)
  sd_vec <- sqrt(variances)
  expected_cov_matrix <- corr_matrix * (sd_vec %o% sd_vec)

  expect_equal(cov_matrix, expected_cov_matrix)
})

test_that("Function handles single-element input", {
  variances <- c(1)
  time_diff_matrix <- matrix(0, nrow = 1)

  cov_matrix <- construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "exponential", lambda = 0.1)

  expected_cov_matrix <- matrix(variances)

  expect_equal(cov_matrix, expected_cov_matrix)
})

test_that("Function computes covariance matrix correctly with large lambda", {
  variances <- c(1, 2, 3)
  time_diff_matrix <- matrix(c(0, 1, 2, 1, 0, 1, 2, 1, 0), nrow = 3)
  lambda <- 100  # Large lambda should result in near-zero off-diagonal elements

  cov_matrix_exp <- construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "exponential", lambda = lambda)
  cov_matrix_lin <- construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "linear", lambda = lambda)

  expected_cov_matrix <- diag(variances)

  expect_equal(cov_matrix_exp, expected_cov_matrix, tolerance = 1e-6)
  expect_equal(cov_matrix_lin, expected_cov_matrix, tolerance = 1e-6)
})

test_that("Covariance matrix is symmetric", {
  variances <- c(1, 1, 1)
  time_diff_matrix <- matrix(c(0, 1, 2, 1, 0, 1, 2, 1, 0), nrow = 3)
  lambda <- 0.5

  cov_matrix_exp <- construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "exponential", lambda = lambda)
  cov_matrix_lin <- construct_cov_matrix_decay(variances, time_diff_matrix, decay_type = "linear", lambda = lambda)

  expect_true(isSymmetric(cov_matrix_exp))
  expect_true(isSymmetric(cov_matrix_lin))
})
