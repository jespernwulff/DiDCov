#' Summary Method for Sensitivity Intervals
#'
#' Provides a concise summary of the results from `compute_sensitivity_intervals` or `compute_sensitivity_intervals_smooth`.
#'
#' If multiple values of Mbar (for `compute_sensitivity_intervals`) or M (for `compute_sensitivity_intervals_smooth`) are provided,
#' the summary will report the widest and narrowest intervals for each distinct Mbar/M value.
#'
#' @param object An object of class `sensitivity_intervals`.
#' @param ... Additional arguments (currently unused).
#' @return Prints a summary to the console.
#' @examples
#' # Define simple data
#' ub <- c(-0.002, 0.015)
#' lb <- c(-0.012, 0.0065)
#' beta <- c(-0.007, 0.011)
#'
#' # Compute the sensitivity intervals
#' bounds_paper <- compute_sensitivity_intervals(
#'   betahat = beta,
#'   ci_lower = lb,
#'   ci_upper = ub,
#'   numPrePeriods = 1,
#'   numPostPeriods = 1
#' )
#'
#' # Print a concise summary of the results
#' summary(bounds_paper)
#' @export
summary.sensitivity_intervals <- function(object, ...) {
  if (!inherits(object, "sensitivity_intervals")) {
    stop("The input object is not of class 'sensitivity_intervals'.")
  }

  # Determine whether we have an 'Mbar' or 'M' column to group by
  colnames_all <- colnames(object$all_intervals)
  param_name <- NULL
  if ("Mbar" %in% colnames_all) {
    param_name <- "Mbar"
  } else if ("M" %in% colnames_all) {
    param_name <- "M"
  }

  # Calculate overall summary statistics
  average_width <- mean(object$all_intervals$interval_width, na.rm = TRUE)
  sd_width <- sd(object$all_intervals$interval_width, na.rm = TRUE)

  cat("Sensitivity Interval Summary:\n")
  cat("----------------------------------------------------\n")

  # If we have multiple values of Mbar/M, print separate summaries per value
  if (!is.null(param_name)) {
    unique_vals <- unique(object$all_intervals[[param_name]])

    # If more than one Mbar/M value, print interval info per Mbar/M
    if (length(unique_vals) > 1) {
      for (val in unique_vals) {
        # Subset intervals for this Mbar/M value
        subset_intervals <- object$all_intervals[object$all_intervals[[param_name]] == val, ]

        # Find widest and narrowest intervals for this subset
        widest_index <- which.max(subset_intervals$interval_width)
        narrowest_index <- which.min(subset_intervals$interval_width)

        widest_interval_sub <- subset_intervals[widest_index, , drop = FALSE]
        narrowest_interval_sub <- subset_intervals[narrowest_index, , drop = FALSE]

        cat(sprintf("%s = %.3f:\n", param_name, val))
        cat("Widest Interval:\n")
        print(widest_interval_sub)
        cat("\nNarrowest Interval:\n")
        print(narrowest_interval_sub)
        cat("\n----------------------------------------------------\n")
      }
    } else {
      # Only one Mbar/M value, behave as original
      cat("Widest Interval:\n")
      print(object$widest_interval)
      cat("\nNarrowest Interval:\n")
      print(object$narrowest_interval)
      cat("\n")
    }
  } else {
    # No Mbar or M column, original behavior
    cat("Widest Interval:\n")
    print(object$widest_interval)
    cat("\nNarrowest Interval:\n")
    print(object$narrowest_interval)
    cat("\n")
  }

  # Print overall average and sd at the bottom
  avg <- round(average_width, 4)
  sdv <- round(sd_width, 4)
  cat(sprintf("Average Interval Width: %.4f (%.4f)\n", avg, sdv))
  cat("----------------------------------------------------\n")
}

