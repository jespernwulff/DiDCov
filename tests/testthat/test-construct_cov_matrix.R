test_that("Function returns correct covariance matrix with specified rho", {
  years <- c(2008, 2009, 2010)
  variances <- c(0.0001, 0.0002, 0.00015)
  rho_value <- 0.5
  cov_matrix <- construct_cov_matrix(years, variances, rho = rho_value)

  expected_corr_matrix <- matrix(rho_value, nrow = 3, ncol = 3)
  diag(expected_corr_matrix) <- 1
  sd_vec <- sqrt(variances)
  expected_cov_matrix <- expected_corr_matrix * (sd_vec %o% sd_vec)

  expect_equal(cov_matrix, expected_cov_matrix)
})

test_that("Function handles invalid rho inputs", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0.0001)

  expect_error(
    construct_cov_matrix(years, variances, rho = "invalid"),
    "rho must be a numeric value between -1 and 1."
  )

  expect_error(
    construct_cov_matrix(years, variances, rho = 1.5),
    "rho must be a numeric value between -1 and 1."
  )

  expect_error(
    construct_cov_matrix(years, variances, rho = NA),
    "rho must be a numeric value between -1 and 1."
  )
})

test_that("Function handles negative variances", {
  years <- c(2008, 2009)
  variances <- c(0.0001, -0.0001)

  expect_error(
    construct_cov_matrix(years, variances, rho = 0.5),
    "Variances must be non-negative."
  )
})

test_that("Function handles zero variances appropriately", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0)

  cov_matrix <- construct_cov_matrix(years, variances, rho = 0.5)

  expected_corr_matrix <- matrix(0.5, nrow = 2, ncol = 2)
  diag(expected_corr_matrix) <- 1
  sd_vec <- sqrt(variances)
  expected_cov_matrix <- expected_corr_matrix * (sd_vec %o% sd_vec)

  expect_equal(cov_matrix, expected_cov_matrix)
})

test_that("Covariance matrix elements are computed correctly with negative rho", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0.0001)
  rho <- -0.5

  cov_matrix <- construct_cov_matrix(years, variances, rho = rho)
  expected_cov <- rho * sqrt(variances[1]) * sqrt(variances[2])

  expect_equal(cov_matrix[1, 2], expected_cov)
  expect_equal(cov_matrix[2, 1], expected_cov)
})

test_that("Function handles mismatched lengths of years and variances", {
  years <- c(2008, 2009)
  variances <- c(0.0001)

  expect_error(
    construct_cov_matrix(years, variances, rho = 0.5),
    "The lengths of years and variances must match."
  )
})

test_that("Function handles negative rho inputs", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0.0002)
  rho <- -0.8

  cov_matrix <- construct_cov_matrix(years, variances, rho = rho)

  # Check if covariance matrix is PSD
  eigenvalues <- eigen(cov_matrix, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues >= -1e-8))
})

test_that("Function handles rho at boundary values correctly", {
  years <- c(2008, 2009)
  variances <- c(0.0001, 0.0002)

  # Test rho = -1
  cov_matrix_neg1 <- construct_cov_matrix(years, variances, rho = -1)
  eigenvalues_neg1 <- eigen(cov_matrix_neg1, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues_neg1 >= -1e-8))

  # Test rho = 1
  cov_matrix_pos1 <- construct_cov_matrix(years, variances, rho = 1)
  eigenvalues_pos1 <- eigen(cov_matrix_pos1, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues_pos1 >= -1e-8))
})

test_that("Function handles large differences in variances", {
  years <- c(2008, 2009, 2010)
  variances <- c(1e-10, 1, 100)

  cov_matrix <- construct_cov_matrix(years, variances, rho = 0.9)

  # Check that covariance matrix is PSD
  eigenvalues <- eigen(cov_matrix, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(eigenvalues >= -1e-8))
})

test_that("Function handles single-year input", {
  years <- c(2008)
  variances <- c(0.0001)

  cov_matrix <- construct_cov_matrix(years, variances, rho = 0.5)

  expect_equal(cov_matrix, matrix(variances))
})

test_that("Function detects invalid rho outside [-1, 1]", {
  years <- c(2008, 2009)
  variances <- c(1, 10000)

  rho_values <- c(-1.1, 1.1)

  for (rho_value in rho_values) {
    expect_error(
      construct_cov_matrix(years, variances, rho = rho_value),
      "rho must be a numeric value between -1 and 1."
    )
  }
})
