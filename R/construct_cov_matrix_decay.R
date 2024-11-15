construct_cov_matrix_decay <- function(
    years,
    variances,
    decay_type = 'exponential',
    decay_param,
    min_decay_param = 0,
    max_decay_param = 1,
    n_steps = 1000  # Number of steps for adjusting decay_param
) {
  # Input validation
  if (length(years) != length(variances)) {
    stop("The lengths of years and variances must match.")
  }
  if (any(variances < 0)) {
    stop("Variances must be non-negative.")
  }
  if (!decay_type %in% c('exponential', 'linear')) {
    stop("Supported decay types are 'exponential' and 'linear'.")
  }
  if (!is.numeric(decay_param) || decay_param < min_decay_param || decay_param > max_decay_param) {
    stop("decay_param must be numeric and within [min_decay_param, max_decay_param].")
  }
  if (min_decay_param < 0 || max_decay_param < min_decay_param) {
    stop("Invalid decay parameter bounds.")
  }
  if (!is.numeric(n_steps) || n_steps <= 0) {
    stop("n_steps must be a positive integer.")
  }

  # Compute time differences
  time_diff <- outer(years, years, FUN = function(x, y) abs(x - y))

  # Function to compute correlation matrix
  compute_corr_matrix <- function(lambda) {
    if (decay_type == 'exponential') {
      rho_mat <- exp(-lambda * time_diff)
    } else if (decay_type == 'linear') {
      rho_mat <- 1 - lambda * time_diff
      rho_mat[rho_mat < 0] <- 0  # Set negative correlations to zero
    }
    diag(rho_mat) <- 1  # Ensure diagonal elements are 1
    return(rho_mat)
  }

  # Function to check positive semi-definiteness
  is_psd <- function(cov_matrix) {
    eigenvalues <- eigen(cov_matrix, symmetric = TRUE, only.values = TRUE)$values
    all(eigenvalues >= -1e-8)
  }

  # Try the specified decay_param
  rho_mat <- compute_corr_matrix(decay_param)
  sd_vec <- sqrt(variances)
  cov_matrix <- rho_mat * (sd_vec %o% sd_vec)

  if (is_psd(cov_matrix)) {
    return(list(cov_matrix = cov_matrix, decay_param = decay_param))
  } else {
    # Adjust decay_param to find the closest PSD value
    # Generate a sequence of decay parameters around the initial value
    delta <- (max_decay_param - min_decay_param) / n_steps
    lambda_values <- seq(min_decay_param, max_decay_param, by = delta)
    # Find the closest lambda to the initial decay_param
    lambda_values <- lambda_values[order(abs(lambda_values - decay_param))]

    for (lambda_candidate in lambda_values) {
      rho_mat <- compute_corr_matrix(lambda_candidate)
      cov_matrix <- rho_mat * (sd_vec %o% sd_vec)
      if (is_psd(cov_matrix)) {
        return(list(cov_matrix = cov_matrix, decay_param = lambda_candidate))
      }
    }

    stop("Cannot find a decay parameter that results in a positive semi-definite covariance matrix within the specified range.")
  }
}
