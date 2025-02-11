#' Construct Covariance Matrix with Decay
#'
#' Constructs a covariance matrix based on provided variances, a decay parameter, and a distance matrix.
#' The decay can be either "exponential" or "linear".
#'
#' @param variances A numeric vector of variances.
#' @param time_diff_matrix A numeric matrix representing the pairwise time differences or distances.
#' @param decay_type A character string specifying the type of decay. Must be either "exponential" or "linear".
#' @param lambda A non-negative numeric value specifying the decay parameter.
#'
#' @return A covariance matrix.
#' @importFrom Matrix nearPD
#' @examples
#' # Example usage with exponential decay
#' variances <- c(1, 1, 1)
#' time_diff_matrix <- matrix(c(0, 1, 2, 1, 0, 1, 2, 1, 0), nrow = 3)
#' cov_matrix <- construct_cov_matrix_decay(variances, time_diff_matrix,
#'                                          decay_type = "exponential",
#'                                          lambda = 0.1)
#' print(cov_matrix)
#'
#' # Example usage with linear decay
#' cov_matrix_linear <- construct_cov_matrix_decay(variances, time_diff_matrix,
#'                                                 decay_type = "linear",
#'                                                 lambda = 0.1)
#' print(cov_matrix_linear)
#' @export
construct_cov_matrix_decay <- function(variances, time_diff_matrix, decay_type = "exponential", lambda) {
  # Input validation
  if (!is.matrix(time_diff_matrix) || nrow(time_diff_matrix) != length(variances) || ncol(time_diff_matrix) != length(variances)) {
    stop("time_diff_matrix must be a square matrix with dimensions matching the length of variances.")
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

  # Compute correlation matrix based on decay type
  if (decay_type == "exponential") {
    corr_matrix <- exp(-lambda * time_diff_matrix)
  } else if (decay_type == "linear") {
    corr_matrix <- pmax(1 - lambda * time_diff_matrix, 0)
  }

  # Construct the covariance matrix
  sd_vec <- sqrt(variances)
  cov_matrix <- corr_matrix * (sd_vec %o% sd_vec)

  # Adjust covariance matrix to be positive semi-definite
  cov_matrix <- as.matrix(nearPD(cov_matrix)$mat)

  # Return the covariance matrix
  return(cov_matrix)
}


