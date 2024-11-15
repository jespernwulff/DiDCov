construct_cov_matrix <- function(years, variances, rho = 'max') {
  if (length(years) != length(variances)) {
    stop("The lengths of years and variances must match.")
  }
  if (any(variances < 0)) {
    stop("Variances must be non-negative.")
  }

  # Check if rho is valid
  if (is.character(rho)) {
    if (!(rho %in% c('max', 'min'))) {
      stop("If rho is a character, it must be either 'max' or 'min'.")
    }
  } else if (is.numeric(rho)) {
    if (rho < 0 || rho > 1) {
      stop("If rho is numeric, it must be between 0 and 1.")
    }
  } else {
    stop("rho must be either 'max', 'min', or a numeric value between 0 and 1.")
  }

  p <- length(years)
  sd_vec <- sqrt(variances)
  sd_mat <- outer(sd_vec, sd_vec)

  # Function to check positive semi-definiteness
  is_psd <- function(rho_value) {
    V <- diag(variances) - rho_value * (sd_mat - diag(variances))
    eigenvalues <- eigen(V, symmetric = TRUE, only.values = TRUE)$values
    all(eigenvalues >= -1e-10)
  }

  # Determine rho value
  if (rho == 'max') {
    # Find the maximum allowable rho
    rho_values <- seq(1, 0, length.out = 10000)
    final_rho <- NA
    for (rho_candidate in rho_values) {
      if (is_psd(rho_candidate)) {
        final_rho <- rho_candidate
        break
      }
    }
    if (is.na(final_rho)) {
      stop("Cannot find a rho value that makes the covariance matrix positive semi-definite.")
    }
  } else if (rho == 'min') {
    final_rho <- 0  # Minimum rho
  } else {
    final_rho <- rho  # Use the specified rho
    if (!is_psd(final_rho)) {
      stop("The specified rho value results in a covariance matrix that is not positive semi-definite.")
    }
  }

  # Construct the covariance matrix
  V <- diag(variances) - final_rho * (sd_mat - diag(variances))

  return(list(cov_matrix = V, rho = final_rho))
}
