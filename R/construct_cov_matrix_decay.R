#' Construct a Covariance Matrix with Decaying Correlations
#'
#' Constructs a covariance matrix for Difference-in-Differences (DiD) estimates,
#' assuming that covariances decay over time according to a specified decay function.
#' The function ensures that the resulting covariance matrix is positive semi-definite (PSD)
#' by adjusting the decay parameter if necessary.
#'
#' @param years Numeric vector of years corresponding to the estimates.
#' @param variances Numeric vector of variances for each estimate. Must be non-negative
#'   and the same length as \code{years}.
#' @param decay_type Character string specifying the type of decay function to use.
#'   Supported options are \code{"exponential"} and \code{"linear"}.
#' @param decay_param Numeric value specifying the decay parameter (\eqn{\lambda}) for
#'   the decay function. Must be within \code{[min_decay_param, max_decay_param]}.
#' @param min_decay_param Numeric value specifying the minimum allowable decay parameter.
#'   Default is \code{0}.
#' @param max_decay_param Numeric value specifying the maximum allowable decay parameter.
#'   Default is \code{1}.
#' @param n_steps Integer specifying the number of steps for adjusting the decay parameter
#'   to find a positive semi-definite covariance matrix. Default is \code{1000}.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{cov_matrix}}{The constructed covariance matrix, which is positive semi-definite.}
#'   \item{\code{decay_param}}{The decay parameter used to construct the covariance matrix.
#'     If the initial \code{decay_param} resulted in a non-PSD matrix, this is the adjusted value.}
#' }
#' @export
#'
#' @examples
#' # Example 1: Using a specified decay parameter (exponential decay)
#' years <- c(2008, 2009, 2010, 2011)
#' variances <- c(0.0001, 0.0002, 0.00015, 0.00012)
#' decay_param <- 0.1
#' result_exp <- construct_cov_matrix_decay(
#'   years = years,
#'   variances = variances,
#'   decay_type = "exponential",
#'   decay_param = decay_param
#' )
#' print(result_exp$cov_matrix)
#' print(result_exp$decay_param)
#'
#' # Example 2: Using a specified decay parameter (linear decay)
#' decay_param <- 0.2
#' result_lin <- construct_cov_matrix_decay(
#'   years = years,
#'   variances = variances,
#'   decay_type = "linear",
#'   decay_param = decay_param
#' )
#' print(result_lin$cov_matrix)
#' print(result_lin$decay_param)
#'
#' # Example 3: Specified decay parameter resulting in non-PSD matrix (adjusted)
#' decay_param <- 0.05  # May result in non-PSD matrix
#' result_adjusted <- construct_cov_matrix_decay(
#'   years = years,
#'   variances = variances,
#'   decay_type = "exponential",
#'   decay_param = decay_param
#' )
#' print(result_adjusted$cov_matrix)
#' print(result_adjusted$decay_param)
#'
#' # Example 4: Perfect correlation (decay_param = 0)
#' decay_param <- 0
#' result_perfect <- construct_cov_matrix_decay(
#'   years = years,
#'   variances = variances,
#'   decay_type = "exponential",
#'   decay_param = decay_param
#' )
#' print(result_perfect$cov_matrix)
#' print(result_perfect$decay_param)

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
