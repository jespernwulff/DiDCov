#' Compute Sensitivity Intervals Over Different Covariance Structures
#'
#' Computes sensitivity results for the post-treatment coefficients over a range of covariance structures.
#' This function generates variance-covariance matrices for specified methods and parameters using the
#' provided functions and computes the corresponding confidence intervals using the `HonestDiD` package.
#'
#' @param betahat A numeric vector of coefficients (effect sizes) for each time period.
#' @param ci_lower A numeric vector of lower bounds of the confidence intervals corresponding to each time period.
#' @param ci_upper A numeric vector of upper bounds of the confidence intervals corresponding to each time period.
#' @param numPrePeriods An integer specifying the number of pre-treatment periods.
#' @param numPostPeriods An integer specifying the number of post-treatment periods.
#' @param method A character string specifying the covariance matrix construction method.
#'   Must be one of "all", "constant", or "decay". Default is "constant".
#' @param rho_values A numeric vector of rho values to use when method is "constant" or "all".
#'   Default is \code{c(0, 0.3, 0.5, 0.8)}.
#' @param decay_types A character vector specifying decay types when method is "decay" or "all".
#'   Possible values are "exponential" and "linear". Default is \code{c("exponential", "linear")}.
#' @param lambda_values A numeric vector of lambda values to use when method is "decay" or "all".
#'   Default is \code{c(0.1, 0.2, 0.5, 1)}.
#' @param ci_level A numeric value between 0 and 1 indicating the confidence level of the confidence intervals. Default is \code{0.95}.
#' @param Mbarvec A numeric vector of values for Mbar to be used in the `HonestDiD` function. Default is \code{c(1)}.
#' @param ... Additional arguments to pass to \code{HonestDiD::createSensitivityResults_relativeMagnitudes}.
#'
#' @return A list containing:
#' \describe{
#'   \item{\code{widest_interval}}{A data frame with the widest interval, including the method and parameter used.}
#'   \item{\code{narrowest_interval}}{A data frame with the narrowest interval, including the method and parameter used.}
#'   \item{\code{all_intervals}}{A data frame with all computed intervals, including methods and parameters.}
#' }
#' @importFrom HonestDiD createSensitivityResults_relativeMagnitudes
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom stats sd
#' @examples
#' \donttest{
#' # Simple example
#' betahat <- c(0.05, 0.06, 0.07)
#' ci_lower <- c(0.02, 0.03, 0.04)
#' ci_upper <- c(0.08, 0.09, 0.10)
#' numPrePeriods <- 1
#' numPostPeriods <- 2
#'
#' # Compute sensitivity intervals using fewer parameter values
#' result <- compute_sensitivity_intervals(
#'   betahat = betahat,
#'   ci_lower = ci_lower,
#'   ci_upper = ci_upper,
#'   numPrePeriods = numPrePeriods,
#'   numPostPeriods = numPostPeriods,
#'   method = "constant",
#'   rho_values = c(0, 0.5, 1),
#'   ci_level = 0.95,
#'   Mbarvec = c(1)
#' )
#' print(result$widest_interval)
#' print(result$narrowest_interval)
#' }
#' @export
compute_sensitivity_intervals <- function(
    betahat,
    ci_lower,
    ci_upper,
    numPrePeriods,
    numPostPeriods,
    method = "constant",
    rho_values = c(0, 0.3, 0.5, 0.8),
    decay_types = c("exponential", "linear"),
    lambda_values = c(0.1, 0.2, 0.5, 1),
    ci_level = 0.95,
    Mbarvec = c(1),
    ...
) {
  # Define the time index internally
  years <- seq_along(betahat)

  # Input validation
  n <- length(betahat)
  if (length(ci_lower) != n || length(ci_upper) != n) {
    stop("betahat, ci_lower, and ci_upper must all be the same length.")
  }
  if (any(ci_upper < ci_lower)) {
    stop("Each element of ci_upper must be greater than or equal to the corresponding element of ci_lower.")
  }
  if (!is.numeric(ci_level) || length(ci_level) != 1 || is.na(ci_level) ||
      ci_level <= 0 || ci_level >= 1) {
    stop("ci_level must be a numeric value between 0 and 1 (exclusive).")
  }
  if (!is.numeric(numPrePeriods) || length(numPrePeriods) != 1 || numPrePeriods < 0) {
    stop("numPrePeriods must be a non-negative integer.")
  }
  if (!is.numeric(numPostPeriods) || length(numPostPeriods) != 1 || numPostPeriods < 1) {
    stop("numPostPeriods must be a positive integer.")
  }
  if (!method %in% c("all", "constant", "decay")) {
    stop("method must be one of 'all', 'constant', or 'decay'.")
  }

  # Compute variances from confidence intervals
  variances_result <- compute_variances_from_ci(years, betahat, ci_lower, ci_upper, ci_level)
  variances <- variances_result$variances

  # Initialize list to store intervals
  intervals_list <- list()

  # Initialize list to store invalid parameter values
  invalid_params <- list()

  # Define methods to run
  methods_to_run <- if (method == "all") c("constant", "decay") else method

  # Calculate total number of iterations for the progress bar
  total_iterations <- 0
  if ("constant" %in% methods_to_run) {
    total_iterations <- total_iterations + length(rho_values)
  }
  if ("decay" %in% methods_to_run) {
    total_iterations <- total_iterations + length(decay_types) * length(lambda_values)
  }

  # Initialize progress bar
  pb <- txtProgressBar(min = 0, max = total_iterations, style = 3)
  progress_counter <- 0

  # Loop over methods
  for (current_method in methods_to_run) {
    if (current_method == "constant") {
      # (Optional) Validate rho_values here if desired
      for (rho in rho_values) {
        # Construct covariance matrix
        sigma <- construct_cov_matrix(years, variances, rho)

        # Check if sigma is positive semi-definite
        if (!is_positive_semi_definite(sigma)) {
          # Log invalid parameter value
          invalid_params[[length(invalid_params) + 1]] <- list(
            method = "constant",
            parameter = rho
          )
          # Skip to next iteration
          progress_counter <- progress_counter + 1
          setTxtProgressBar(pb, progress_counter)
          next
        }

        # Call HonestDiD function
        delta_rm_results <- createSensitivityResults_relativeMagnitudes(
          betahat = betahat,
          sigma = sigma,
          numPrePeriods = numPrePeriods,
          numPostPeriods = numPostPeriods,
          Mbarvec = Mbarvec,
          ...
        )

        # Add method and parameter to results
        delta_rm_results$method <- "constant"
        delta_rm_results$parameter <- rho

        # Store results
        intervals_list[[length(intervals_list) + 1]] <- delta_rm_results

        # Update progress bar
        progress_counter <- progress_counter + 1
        setTxtProgressBar(pb, progress_counter)
      }
    } else if (current_method == "decay") {
      # (Optional) Validate decay_types and lambda_values here if desired
      for (decay_type in decay_types) {
        for (lambda in lambda_values) {
          # Construct covariance matrix
          sigma <- construct_cov_matrix_decay(years, variances, decay_type = decay_type, lambda = lambda)

          # Check if sigma is positive semi-definite
          if (!is_positive_semi_definite(sigma)) {
            # Log invalid parameter values
            invalid_params[[length(invalid_params) + 1]] <- list(
              method = paste0("decay_", decay_type),
              parameter = lambda
            )
            # Skip to next iteration
            progress_counter <- progress_counter + 1
            setTxtProgressBar(pb, progress_counter)
            next
          }

          # Call HonestDiD function
          delta_rm_results <- createSensitivityResults_relativeMagnitudes(
            betahat = betahat,
            sigma = sigma,
            numPrePeriods = numPrePeriods,
            numPostPeriods = numPostPeriods,
            Mbarvec = Mbarvec,
            ...
          )

          # Add method and parameter to results
          delta_rm_results$method <- paste0("decay_", decay_type)
          delta_rm_results$parameter <- lambda

          # Store results
          intervals_list[[length(intervals_list) + 1]] <- delta_rm_results

          # Update progress bar
          progress_counter <- progress_counter + 1
          setTxtProgressBar(pb, progress_counter)
        }
      }
    } else {
      stop("Unknown method specified.")
    }
  }

  # Close progress bar
  close(pb)

  # Combine results into a single data frame
  if (length(intervals_list) > 0) {
    combined_results <- do.call(rbind, intervals_list)
    # Calculate interval widths
    combined_results$interval_width <- combined_results$ub - combined_results$lb

    # Find widest interval
    widest_index <- which.max(combined_results$interval_width)
    widest_interval <- combined_results[widest_index, ]

    # Find narrowest interval
    narrowest_index <- which.min(combined_results$interval_width)
    narrowest_interval <- combined_results[narrowest_index, ]
  } else {
    warning("No valid intervals computed. All covariance matrices were not positive semi-definite.")
    combined_results <- data.frame()
    widest_interval <- data.frame()
    narrowest_interval <- data.frame()
  }

  # Inform the user about invalid parameter values
  if (length(invalid_params) > 0) {
    warning(sprintf(
      "Covariance matrices were not positive semi-definite for some parameter values. %d cases were skipped.",
      length(invalid_params)
    ))
  }

  # Create a custom object with class
  result <- list(
    widest_interval = widest_interval,
    narrowest_interval = narrowest_interval,
    all_intervals = combined_results
  )

  class(result) <- "sensitivity_intervals"

  return(result)
}

# Helper function to check if a matrix is positive semi-definite
is_positive_semi_definite <- function(mat, tol = 1e-8) {
  eigenvalues <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
  return(all(eigenvalues >= -tol))
}
