#' Construct Covariance Matrix with Decay
#'
#' Constructs a covariance matrix based on provided years, variances, and a decay parameter.
#' The decay can be either "exponential" or "linear".
#'
#' @param years A numeric vector of years.
#' @param variances A numeric vector of variances corresponding to each year.
#' @param decay_type A character string specifying the type of decay. Must be either "exponential" or "linear".
#' @param lambda A non-negative numeric value specifying the decay parameter.
#'
#' @return A covariance matrix.
#' @examples
#' # Example usage with exponential decay
#' years <- c(2000, 2001, 2002)
#' variances <- c(1, 1, 1)
#' cov_matrix <- construct_cov_matrix_decay(years, variances, decay_type = "exponential", lambda = 0.1)
#' print(cov_matrix)
#'
#' # Example usage with linear decay
#' cov_matrix_linear <- construct_cov_matrix_decay(years, variances, decay_type = "linear", lambda = 0.1)
#' print(cov_matrix_linear)
#' @export
construct_cov_matrix_decay <- function(years, variances, decay_type = "exponential", lambda) {
  # Input validation
  if (length(years) != length(variances)) {
    stop("The lengths of years and variances must match.")
  }
  if (any(variances < 0)) {
    stop("Variances must be non-negative.")
  }
  if (!is.numeric(lambda) || length(lambda) != 1 || is.na(lambda) || lambda < 0) {
    stop("lambda must be a non-negative numeric value.")
  }
  if (!decay_type %in% c("exponential", "linear")) {
    stop("decay_type must be either 'exponential' or 'linear'.")
  }

  # Compute time differences
  time_diff_matrix <- as.matrix(dist(years, diag = TRUE, upper = TRUE))

  # Compute correlation matrix based on decay type
  if (decay_type == "exponential") {
    corr_matrix <- exp(-lambda * time_diff_matrix)
  } else if (decay_type == "linear") {
    corr_matrix <- pmax(1 - lambda * time_diff_matrix, 0)
  }

  # Construct the covariance matrix
  sd_vec <- sqrt(variances)
  cov_matrix <- corr_matrix * (sd_vec %o% sd_vec)

  # Check if the covariance matrix is PSD
  if (!is_psd(cov_matrix)) {
    stop("The specified parameters result in a covariance matrix that is not positive semi-definite.")
  }

  # Return the covariance matrix
  return(cov_matrix)
}

# Function to check positive semi-definiteness
is_psd <- function(matrix) {
  eigenvalues <- eigen(matrix, symmetric = TRUE, only.values = TRUE)$values
  all(eigenvalues >= -1e-8)
}

