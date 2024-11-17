#' Construct Covariance Matrix with Specified Rho
#'
#' Constructs a covariance matrix based on provided years, variances, and a specified correlation coefficient (rho).
#'
#' @param years A numeric vector of years.
#' @param variances A numeric vector of variances corresponding to each year.
#' @param rho A numeric value between -1 and 1 specifying the correlation coefficient to be used for all off-diagonal elements.
#'
#' @return A list containing the covariance matrix (`cov_matrix`) and the `rho` used.
#' @examples
#' # Example usage
#' years <- c(2008, 2009, 2010)
#' variances <- c(0.0001, 0.0002, 0.00015)
#' result <- construct_cov_matrix(years, variances, rho = 0.5)
#' print(result$cov_matrix)
#' print(result$rho)
#' @export
construct_cov_matrix <- function(years, variances, rho) {
  # Input validation
  if (length(years) != length(variances)) {
    stop("The lengths of years and variances must match.")
  }
  if (any(variances < 0)) {
    stop("Variances must be non-negative.")
  }
  if (!is.numeric(rho) || length(rho) != 1 || is.na(rho) || rho < -1 || rho > 1) {
    stop("rho must be a numeric value between -1 and 1.")
  }

  # Construct the correlation matrix
  n <- length(years)
  corr_matrix <- matrix(rho, nrow = n, ncol = n)
  diag(corr_matrix) <- 1

  # Construct the covariance matrix
  sd_vec <- sqrt(variances)
  cov_matrix <- corr_matrix * (sd_vec %o% sd_vec)

  # Check if the covariance matrix is PSD
  if (!is_psd(cov_matrix)) {
    stop("The specified rho results in a covariance matrix that is not positive semi-definite.")
  }

  # Return the covariance matrix and rho used
  return(list(cov_matrix = cov_matrix, rho = rho))
}

# Function to check positive semi-definiteness
is_psd <- function(matrix) {
  eigenvalues <- eigen(matrix, symmetric = TRUE, only.values = TRUE)$values
  all(eigenvalues >= -1e-8)
}


