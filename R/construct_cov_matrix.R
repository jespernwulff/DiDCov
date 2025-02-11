#' Construct Covariance Matrix with Specified Rho
#'
#' Constructs a covariance matrix based on provided variances and a specified correlation coefficient (rho).
#'
#' @param variances A numeric vector of variances.
#' @param rho A numeric value between -1 and 1 specifying the correlation coefficient to be used for all off-diagonal elements.
#'
#' @return A covariance matrix.
#' @importFrom Matrix nearPD
#' @examples
#' # Example usage
#' variances <- c(0.0001, 0.0002, 0.00015)
#' cov_matrix <- construct_cov_matrix(variances, rho = 0.5)
#' print(cov_matrix)
#' @export
construct_cov_matrix <- function(variances, rho) {
  # Input validation
  if (any(variances < 0)) {
    stop("Variances must be non-negative.")
  }
  if (!is.numeric(rho) || length(rho) != 1 || is.na(rho) || rho < -1 || rho > 1) {
    stop("rho must be a numeric value between -1 and 1.")
  }

  # Determine the size of the covariance matrix
  n <- length(variances)

  # Construct the correlation matrix
  corr_matrix <- matrix(rho, nrow = n, ncol = n)
  diag(corr_matrix) <- 1

  # Construct the covariance matrix
  sd_vec <- sqrt(variances)
  cov_matrix <- corr_matrix * (sd_vec %o% sd_vec)

  # Adjust covariance matrix to be positive semi-definite
  cov_matrix <- as.matrix(nearPD(cov_matrix)$mat)

  # Return the covariance matrix
  return(cov_matrix)
}





