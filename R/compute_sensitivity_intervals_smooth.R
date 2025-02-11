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
#' @param scale Logical flag indicating whether to apply scaling to \code{betahat},
#'   \code{sigma}, and \code{Mvec} to help with numerical stability (default \code{TRUE}).
#' @param benchmark_aggregator Character in \code{c("none","smallest","largest","median")}.
#'   When not "none", this triggers a call to \code{compute_bound()} to obtain an \emph{upper}
#'   benchmark bound. The resulting numeric is multiplied into \code{Mvec} before passing to
#'   \code{createSensitivityResults()}. Defaults to \code{"none"}.
#' @param ... Additional arguments passed to \code{HonestDiD::createSensitivityResults}
#'   (e.g. \code{alpha} or \code{l_vec}).
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
    scale = TRUE,
    benchmark_aggregator = c("none", "smallest", "largest", "median"),
    ...
) {
  benchmark_aggregator <- match.arg(benchmark_aggregator)

  #---- 1. Input validation ----#
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

  #---- 2. Compute placeholder variances from confidence intervals ----#
  variances <- compute_variances_from_ci(
    effect_sizes = betahat,
    ci_lower     = ci_lower,
    ci_upper     = ci_upper,
    ci_level     = ci_level
  )

  #---- 3. [Optional] Compute an 'upper' benchmark bound & multiply Mvec ----#
  #    Only done if user sets benchmark_aggregator != "none"
  if (benchmark_aggregator != "none") {
    # The user wants an upper bound across the given rho_values.
    # We do not scale within compute_bound() because the function below will handle scaling.
    bound_val <- compute_bound(
      betahat       = betahat,
      ci_lower      = ci_lower,
      ci_upper      = ci_upper,
      numPrePeriods = numPrePeriods,
      rho_values    = rho_values,
      ci_level      = ci_level,
      aggregator    = benchmark_aggregator,  # "smallest","largest","median"
      bound_type    = "upper",
      scale         = FALSE
    )
    # Multiply Mvec by the newly found bound
    Mvec <- Mvec * bound_val
  }

  #---- 4. Helper function to run createSensitivityResults with optional scaling ----#
  run_sensitivity_with_optional_scaling <- function(betahat, sigma, Mvec, ...) {
    if (scale) {
      # Attempt scaling based on variance of the first post-treatment period
      var_post <- sigma[numPrePeriods + 1, numPrePeriods + 1]
      if (var_post <= 0) {
        # Not positive or numerically stable; skip or warn
        return(NULL)
      }

      scaleFactor <- 1 / sqrt(var_post)

      # Scale betahat, sigma, and Mvec
      betahat_scaled <- betahat * scaleFactor
      sigma_scaled   <- sigma * (scaleFactor^2)
      Mvec_scaled    <- Mvec  * scaleFactor

      # Safely run createSensitivityResults
      result <- tryCatch(
        {
          HonestDiD::createSensitivityResults(
            betahat       = betahat_scaled,
            sigma         = sigma_scaled,
            numPrePeriods = numPrePeriods,
            numPostPeriods= numPostPeriods,
            Mvec          = Mvec_scaled,
            ...
          )
        },
        error = function(e) {
          # If even with scaling it fails, return NULL
          return(NULL)
        }
      )

      # If successful, unscale the results (lb, ub, plus columns named M or Mbar)
      if (!is.null(result)) {
        result$lb <- result$lb / scaleFactor
        result$ub <- result$ub / scaleFactor

        if ("M" %in% colnames(result)) {
          result$M <- result$M / scaleFactor
        }
        if ("Mbar" %in% colnames(result)) {
          result$Mbar <- result$Mbar / scaleFactor
        }
      }
      return(result)

    } else {
      # Try no scaling; catch errors
      result <- tryCatch(
        {
          HonestDiD::createSensitivityResults(
            betahat       = betahat,
            sigma         = sigma,
            numPrePeriods = numPrePeriods,
            numPostPeriods= numPostPeriods,
            Mvec          = Mvec,
            ...
          )
        },
        error = function(e) {
          message("Computation failed (possibly a non-positive-definite covariance). ",
                  "Consider `scale = TRUE` or reviewing your inputs.")
          return(NULL)
        }
      )
      return(result)
    }
  }

  #---- 5. Methods to run: either "constant", "decay", or both ("all") ----#
  methods_to_run <- if (method == "all") c("constant", "decay") else method

  intervals_list <- list()
  invalid_params <- list()

  # Count total iterations for progress bar
  total_iterations <- 0
  if ("constant" %in% methods_to_run) {
    total_iterations <- total_iterations + length(rho_values)
  }
  if ("decay" %in% methods_to_run) {
    total_iterations <- total_iterations + length(decay_types) * length(lambda_values)
  }

  pb <- txtProgressBar(min = 0, max = total_iterations, style = 3)
  progress_counter <- 0

  #---- 6. Main loop over the chosen method(s) ----#
  for (current_method in methods_to_run) {
    if (current_method == "constant") {
      if (!is.numeric(rho_values) || any(rho_values < -1) || any(rho_values > 1)) {
        stop("rho_values must be numeric with values between -1 and 1.")
      }
      # Loop over rho
      for (rho in rho_values) {
        sigma <- construct_cov_matrix(variances = variances, rho = rho)
        T <- length(betahat)
        if (!all(dim(sigma) == c(T, T))) {
          stop("sigma must be a square matrix with dimensions equal to length(betahat).")
        }

        # Attempt sensitivity with optional scaling
        delta_rm_results <- run_sensitivity_with_optional_scaling(
          betahat = betahat,
          sigma   = sigma,
          Mvec    = Mvec,
          ...
        )

        if (!is.null(delta_rm_results)) {
          delta_rm_results$method    <- "constant"
          delta_rm_results$parameter <- rho
          intervals_list[[length(intervals_list) + 1]] <- delta_rm_results
        } else {
          invalid_params[[length(invalid_params) + 1]] <- list(method = "constant", param = rho)
        }

        progress_counter <- progress_counter + 1
        setTxtProgressBar(pb, progress_counter)
      }

    } else if (current_method == "decay") {
      # decay approach
      if (!all(decay_types %in% c("exponential", "linear"))) {
        stop("decay_types must be either 'exponential' or 'linear'.")
      }
      if (!is.numeric(lambda_values) || any(lambda_values < 0)) {
        stop("lambda_values must be non-negative.")
      }

      # Loop over decay_types and lambdas
      for (decay_type in decay_types) {
        for (lambda in lambda_values) {
          sigma <- construct_cov_matrix_decay(
            variances   = variances,
            decay_type  = decay_type,
            lambda      = lambda
          )
          T <- length(betahat)
          if (!all(dim(sigma) == c(T, T))) {
            stop("sigma must be a square matrix with dimensions equal to length(betahat).")
          }

          delta_rm_results <- run_sensitivity_with_optional_scaling(
            betahat = betahat,
            sigma   = sigma,
            Mvec    = Mvec,
            ...
          )

          if (!is.null(delta_rm_results)) {
            delta_rm_results$method    <- paste0("decay_", decay_type)
            delta_rm_results$parameter <- lambda
            intervals_list[[length(intervals_list) + 1]] <- delta_rm_results
          } else {
            invalid_params[[length(invalid_params) + 1]] <-
              list(method = paste0("decay_", decay_type), param = lambda)
          }

          progress_counter <- progress_counter + 1
          setTxtProgressBar(pb, progress_counter)
        }
      }

    } else {
      stop("Unknown method specified.")
    }
  }

  close(pb)

  #---- 7. Compile & finalize results ----#
  if (length(intervals_list) > 0) {
    combined_results <- do.call(rbind, intervals_list)
    combined_results$interval_width <- combined_results$ub - combined_results$lb

    # Widest interval
    widest_index <- which.max(combined_results$interval_width)
    widest_interval <- combined_results[widest_index, , drop = FALSE]

    # Narrowest interval
    narrowest_index <- which.min(combined_results$interval_width)
    narrowest_interval <- combined_results[narrowest_index, , drop = FALSE]
  } else {
    warning("No valid intervals computed. Possibly all covariance matrices failed or the solver did not converge.")
    combined_results  <- data.frame()
    widest_interval   <- data.frame()
    narrowest_interval<- data.frame()
  }

  # Possibly warn about invalid parameters
  if (length(invalid_params) > 0) {
    warning(sprintf(
      "Computation failed for %d parameter setting(s). This may indicate non-positive-definite covariance or solver issues.",
      length(invalid_params)
    ))
  }

  #---- 8. Return result object ----#
  result <- list(
    widest_interval     = widest_interval,
    narrowest_interval  = narrowest_interval,
    all_intervals       = combined_results
  )
  class(result) <- "sensitivity_intervals"
  return(result)
}


