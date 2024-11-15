#' Construct a Covariance Matrix with Constant Correlation
#'
#' Constructs a covariance matrix for Difference-in-Differences (DiD) estimates,
#' assuming a constant correlation coefficient between different periods.
#' The function can compute the maximum allowable correlation coefficient that ensures
#' the resulting covariance matrix is positive semi-definite (PSD).
#'
#' @param years Numeric vector of years corresponding to the estimates.
#' @param variances Numeric vector of variances for each estimate. Must be non-negative
#'   and the same length as \code{years}.
#' @param rho Either \code{"max"}, \code{"min"}, or a numeric value between 0 and 1
#'   specifying the correlation coefficient. If \code{"max"}, the function calculates
#'   the maximum allowable correlation coefficient that results in a positive semi-definite
#'   covariance matrix. If \code{"min"}, the correlation coefficient is set to 0.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{cov_matrix}}{The constructed covariance matrix.}
#'   \item{\code{rho}}{The correlation coefficient used to construct the covariance matrix.}
#' }
#' @export
#'
#' @examples
#' # Example 1: Using the maximum allowable correlation coefficient
#' years <- c(2008, 2009, 2010, 2011)
#' variances <- c(0.0001, 0.0002, 0.00015, 0.00012)
#' result_max <- construct_cov_matrix(years, variances, rho = 'max')
#' print(result_max$cov_matrix)
#' print(result_max$rho)
#'
#' # Example 2: Using a specified correlation coefficient
#' rho_value <- 0.5
#' result_specified <- construct_cov_matrix(years, variances, rho = rho_value)
#' print(result_specified$cov_matrix)
#' print(result_specified$rho)
#'
#' # Example 3: Using the minimum correlation coefficient (rho = 0)
#' result_min <- construct_cov_matrix(years, variances, rho = 'min')
#' print(result_min$cov_matrix)
#' print(result_min$rho)
#'
#' # Example 4: Handling variances of different magnitudes
#' years <- c(2010, 2011, 2012)
#' variances <- c(0.0005, 0.0001, 0.0003)
#' result <- construct_cov_matrix(years, variances, rho = 'max')
#' print(result$cov_matrix)
#' print(result$rho)
construct_cov_matrix <- function(years, variances, rho = 'max') {
  # Input validation
  if (length(years) != length(variances)) {
    stop("The lengths of years and variances must match.")
  }
  if (any(variances < 0)) {
    stop("Variances must be non-negative.")
  }

  # Validate rho
  if (rho == 'max') {
    # Compute maximum allowable rho that results in PSD covariance matrix
    rho <- compute_max_rho(variances)
  } else if (rho == 'min') {
    rho <- 0
  } else if (!is.numeric(rho) || rho < -1 || rho > 1) {
    stop("rho must be 'max', 'min', or a numeric value between -1 and 1.")
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

# Helper function to compute maximum allowable rho
compute_max_rho <- function(variances) {
  sd_vec <- sqrt(variances)
  sd_min <- min(sd_vec)
  sd_max <- max(sd_vec)
  rho_max <- (2 * sd_min * sd_max) / (sd_min^2 + sd_max^2)
  rho_max <- max(-1, min(1, rho_max))
  return(rho_max)
}

# Function to check positive semi-definiteness
is_psd <- function(matrix) {
  eigenvalues <- eigen(matrix, symmetric = TRUE, only.values = TRUE)$values
  all(eigenvalues >= -1e-8)
}

