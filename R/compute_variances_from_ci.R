#' Compute Variances from Confidence Intervals and Effect Sizes
#'
#' Computes the variances from given confidence intervals and effect sizes.
#'
#' @param effect_sizes A numeric vector of effect sizes (estimates).
#' @param ci_lower A numeric vector of lower bounds of the confidence intervals.
#' @param ci_upper A numeric vector of upper bounds of the confidence intervals.
#' @param ci_level A numeric value between 0 and 1 indicating the confidence level of the confidence intervals. Default is \code{0.95}.
#'
#' @return A numeric vector of variances computed from the confidence intervals.
#' @importFrom stats qnorm
#' @examples
#' # Example data
#' effect_sizes <- c(0.1, 0.15, 0.2)
#' ci_lower <- c(0.05, 0.1, 0.15)
#' ci_upper <- c(0.15, 0.2, 0.25)
#'
#' variances <- compute_variances_from_ci(effect_sizes, ci_lower, ci_upper)
#' print(variances)
#'
#' # Using the variances with the covariance matrix function
#' cov_matrix <- construct_cov_matrix(variances, rho = 0.5)
#' print(cov_matrix)
#' @export
compute_variances_from_ci <- function(effect_sizes, ci_lower, ci_upper, ci_level = 0.95) {
  # Input validation
  if (length(effect_sizes) != length(ci_lower) || length(effect_sizes) != length(ci_upper)) {
    stop("All input vectors (effect_sizes, ci_lower, ci_upper) must have the same length.")
  }
  if (any(ci_upper < ci_lower)) {
    stop("Each element of ci_upper must be greater than or equal to the corresponding element of ci_lower.")
  }
  if (!is.numeric(ci_level) || length(ci_level) != 1 || is.na(ci_level) ||
      ci_level <= 0 || ci_level >= 1) {
    stop("ci_level must be a numeric value between 0 and 1 (exclusive).")
  }

  # Compute z value from ci_level
  z_value <- qnorm(1 - (1 - ci_level) / 2)

  # Compute standard errors
  standard_errors <- (ci_upper - ci_lower) / (2 * z_value)

  # Compute variances
  variances <- standard_errors^2

  # Return variances
  return(variances)
}

