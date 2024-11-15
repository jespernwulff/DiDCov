# Load necessary library for testing
library(testthat)

# Source the function file if needed
# source("path/to/your/construct_cov_matrix.R")

# Begin unit tests
test_that("Function returns correct covariance matrix with specified rho", {
  years <- c(2008, 2009, 2010)
  variances <- c(0.0001, 0.0002, 0.00015)
  rho_value <- 0.5
  result <- construct_cov_matrix(years, variances, rho = rho_value)

  expected_cov_matrix <- matrix(rho_value, nrow = 3, ncol = 3)
  diag(expected_cov_matrix) <- 1
  sd_vec <- sqrt(variances)
  expected_cov_matrix <- expected_cov_matrix * (sd_vec %o% sd_vec)

  expect_equal(result$cov_matrix, expected_cov_matrix)
  expect_equal(result$rho, rho_value)
})

test_that("Function computes maximum rho correctly", {
  years <- c(2008, 2009, 2010)
  variances <- c(0.0001, 0.0002, 0.00015)
  result <- construct_cov_matrix(years, variances, rho = 'max')
  expect_true(result$rho <= 1)
  expect_true(result$rho >= -1)

  # Check that covariance matrix is PSD
  eigenvalues <- eigen(result$cov_matrix, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues >= -1e-8))
})

test_that("Function handles rho = 'min' correctly", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0.0001)
  result <- construct_cov_matrix(years, variances, rho = 'min')

  expected_cov_matrix <- diag(variances)

  expect_equal(result$cov_matrix, expected_cov_matrix)
  expect_equal(result$rho, 0)
})

test_that("Function throws error when specified rho is out of bounds", {
  years <- c(2008, 2009, 2010)
  variances <- c(0.0001, 0.0002, 0.00015)
  expect_error(
    construct_cov_matrix(years, variances, rho = 1.5),
    "rho must be 'max', 'min', or a numeric value between -1 and 1."
  )
})

test_that("Function handles invalid rho inputs", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0.0001)
  expect_error(
    construct_cov_matrix(years, variances, rho = "invalid"),
    "rho must be 'max', 'min', or a numeric value between -1 and 1."
  )
})

test_that("Covariance matrix is PSD even with high rho and variance differences", {
  years <- c(2008, 2009, 2010)
  variances <- c(1, 1, 10000)
  result <- construct_cov_matrix(years, variances, rho = 0.9999)

  # Check that covariance matrix is PSD
  eigenvalues <- eigen(result$cov_matrix, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues >= -1e-8))
})

test_that("Function handles negative variances", {
  years <- c(2008, 2009)
  variances <- c(0.0001, -0.0001)
  expect_error(
    construct_cov_matrix(years, variances),
    "Variances must be non-negative."
  )
})

test_that("Function handles zero variances appropriately", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0)
  result <- construct_cov_matrix(years, variances, rho = "min")

  expected_cov_matrix <- diag(variances)

  expect_equal(result$cov_matrix, expected_cov_matrix)
  expect_equal(result$rho, 0)
})

test_that("Covariance matrix elements are computed correctly with negative rho", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0.0001)
  rho <- -0.5
  result <- construct_cov_matrix(years, variances, rho = rho)
  expected_cov <- rho * sqrt(variances[1]) * sqrt(variances[2])
  expect_equal(result$cov_matrix[1, 2], expected_cov)
  expect_equal(result$cov_matrix[2, 1], expected_cov)
})

test_that("Function handles non-standard rho values appropriately", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0.0001)
  expect_error(
    construct_cov_matrix(years, variances, rho = TRUE),
    "rho must be 'max', 'min', or a numeric value between -1 and 1."
  )
})

test_that("Function handles mismatched lengths of years and variances", {
  years <- c(2008, 2009)
  variances <- c(0.0001)
  expect_error(
    construct_cov_matrix(years, variances),
    "The lengths of years and variances must match."
  )
})

test_that("Function computes maximum rho correctly when variances are equal", {
  years <- c(2008, 2009, 2010)
  variances <- c(0.0001, 0.0001, 0.0001)
  result <- construct_cov_matrix(years, variances, rho = 'max')
  expect_equal(result$rho, 1)

  # Check that covariance matrix is PSD
  eigenvalues <- eigen(result$cov_matrix, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues >= -1e-8))
})

test_that("Function handles variances with zero values", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0)
  result <- construct_cov_matrix(years, variances, rho = "min")

  expected_cov_matrix <- diag(variances)

  expect_equal(result$cov_matrix, expected_cov_matrix)
  expect_equal(result$rho, 0)
})

test_that("Function handles negative rho inputs", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0.0002)
  rho <- -0.8
  result <- construct_cov_matrix(years, variances, rho = rho)

  # Check that covariance matrix is PSD or throws an error if not
  eigenvalues <- eigen(result$cov_matrix, symmetric = TRUE, only.values = TRUE)$values
  if (!all(eigenvalues >= -1e-8)) {
    expect_error(
      construct_cov_matrix(years, variances, rho = rho),
      "The specified rho results in a covariance matrix that is not positive semi-definite."
    )
  } else {
    expect_equal(result$rho, rho)
  }
})

test_that("Function handles rho at boundary values correctly", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0.0002)

  # Test rho = -1
  result_neg1 <- construct_cov_matrix(years, variances, rho = -1)
  eigenvalues_neg1 <- eigen(result_neg1$cov_matrix, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues_neg1 >= -1e-8))

  # Test rho = 1
  result_pos1 <- construct_cov_matrix(years, variances, rho = 1)
  expect_equal(result_pos1$rho, 1)

  # Check that covariance matrix is PSD
  eigenvalues_pos1 <- eigen(result_pos1$cov_matrix, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues_pos1 >= -1e-8))
})

test_that("Function handles large differences in variances", {
  years <- c(2008, 2009, 2010)
  variances <- c(1e-10, 1, 100)
  result <- construct_cov_matrix(years, variances, rho = 'max')

  expect_true(result$rho <= 1)
  expect_true(result$rho >= -1)

  # Check that covariance matrix is PSD
  eigenvalues <- eigen(result$cov_matrix, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues >= -1e-8))
})

test_that("Function handles single-year input", {
  years <- c(2008)
  variances <- c(0.0001)
  result <- construct_cov_matrix(years, variances, rho = 'max')

  expect_equal(result$cov_matrix, matrix(variances))
  expect_equal(result$rho, 1)
})

