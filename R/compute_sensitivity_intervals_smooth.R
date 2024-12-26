#' Compute Sensitivity Intervals Using Smoothness Restrictions
#'
#' This function computes sensitivity intervals over different covariance structures using smoothness restrictions, leveraging the `HonestDiD` package.
#'
#' @param betahat Numeric vector of effect size estimates.
#' @param ci_lower Numeric vector of lower bounds of confidence intervals.
#' @param ci_upper Numeric vector of upper bounds of confidence intervals.
#' @param numPrePeriods Integer indicating the number of pre-treatment periods.
#' @param numPostPeriods Integer indicating the number of post-treatment periods.
#' @param method Character string specifying the method to use: "constant", "decay", or "all". Defaults to "constant".
#' @param rho_values Numeric vector of rho values to use for the "constant" method. Defaults to \code{c(0, 0.3, 0.5, 0.8)}.
#' @param decay_types Character vector specifying decay types for the "decay" method. Defaults to \code{c("exponential", "linear")}.
#' @param lambda_values Numeric vector of lambda values to use for the "decay" method. Defaults to \code{c(0.1, 0.2, 0.5, 1)}.
#' @param ci_level Numeric value specifying the confidence level. Defaults to \code{0.95}.
#' @param Mvec Numeric vector of smoothness parameters for smoothness restrictions. Defaults to \code{0.02}.
#' @param ... Additional arguments passed to \code{HonestDiD::createSensitivityResults}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{\code{widest_interval}}{The interval with the widest width.}
#'     \item{\code{narrowest_interval}}{The interval with the narrowest width.}
#'     \item{\code{all_intervals}}{A data frame containing all computed intervals.}
#'   }
#'
#' @importFrom stats sd
#' @importFrom HonestDiD createSensitivityResults
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
compute_sensitivity_intervals_smooth <- function(
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
    Mvec = 0.02,
    ...
) {
  # Define the time index internally
  years <- seq_along(betahat)

  # Input validation
  if (length(betahat) != length(ci_lower) || length(betahat) != length(ci_upper)) {
    stop("betahat, ci_lower, and ci_upper must all be the same length.")
  }

  if (!is.numeric(numPrePeriods) || !is.numeric(numPostPeriods)) {
    stop("numPrePeriods and numPostPeriods must be numeric.")
  }

  if (!is.numeric(ci_level) || ci_level <= 0 || ci_level >= 1) {
    stop("ci_level must be a numeric value between 0 and 1 (exclusive).")
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
      if (!is.numeric(rho_values) || any(rho_values < -1) || any(rho_values > 1)) {
        stop("rho_values must be a numeric vector with values between -1 and 1.")
      }

      for (rho in rho_values) {
        # Construct covariance matrix
        sigma <- construct_cov_matrix(years, variances, rho)

        # Ensure sigma dimensions match betahat
        T <- length(betahat)
        if (!all(dim(sigma) == c(T, T))) {
          stop("sigma must be a square matrix with dimensions equal to the length of betahat.")
        }

        # Call HonestDiD function
        delta_rm_results <- createSensitivityResults(
          betahat = betahat,
          sigma = sigma,
          numPrePeriods = numPrePeriods,
          numPostPeriods = numPostPeriods,
          Mvec = Mvec,
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
      if (!all(decay_types %in% c("exponential", "linear"))) {
        stop("decay_types must be either 'exponential' or 'linear'.")
      }

      if (!is.numeric(lambda_values) || any(lambda_values < 0)) {
        stop("lambda_values must be a numeric vector with non-negative values.")
      }

      for (decay_type in decay_types) {
        for (lambda in lambda_values) {
          # Construct covariance matrix
          sigma <- construct_cov_matrix_decay(
            years = years,
            variances = variances,
            decay_type = decay_type,
            lambda = lambda
          )

          # Ensure sigma dimensions match betahat
          T <- length(betahat)
          if (!all(dim(sigma) == c(T, T))) {
            stop("sigma must be a square matrix with dimensions equal to the length of betahat.")
          }

          # Call HonestDiD function
          delta_rm_results <- createSensitivityResults(
            betahat = betahat,
            sigma = sigma,
            numPrePeriods = numPrePeriods,
            numPostPeriods = numPostPeriods,
            Mvec = Mvec,
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

  # Optionally, inform the user about invalid parameter values
  if (length(invalid_params) > 0) {
    warning(sprintf(
      "Covariance matrices were not positive semi-definite for some parameter values. %d cases were skipped.",
      length(invalid_params)
    ))
    # Could also return invalid_params as part of the result if desired
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
