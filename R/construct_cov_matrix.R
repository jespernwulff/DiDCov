#' Construct Covariance Matrix with Specified Rho
#'
#' Constructs a covariance matrix based on provided years, variances, and a specified correlation coefficient (rho).
#'
#' @param years A numeric vector of years.
#' @param variances A numeric vector of variances corresponding to each year.
#' @param rho A numeric value between -1 and 1 specifying the correlation coefficient to be used for all off-diagonal elements.
#'
#' @return A covariance matrix.
#' @importFrom Matrix nearPD
#' @examples
#' # Example usage
#' years <- c(2008, 2009, 2010)
#' variances <- c(0.0001, 0.0002, 0.00015)
#' cov_matrix <- construct_cov_matrix(years, variances, rho = 0.5)
#' print(cov_matrix)
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
  sd_vec <- as.numeric(sqrt(variances))
  cov_matrix <- corr_matrix * (sd_vec %o% sd_vec)

  # Adjust sigma to be positive semi-definite
  cov_matrix <- as.matrix(nearPD(cov_matrix)$mat)

  # Return the covariance matrix
  return(cov_matrix)
}





