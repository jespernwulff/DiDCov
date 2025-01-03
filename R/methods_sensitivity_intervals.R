#' Summary Method for Sensitivity Intervals
#'
#' Provides a concise summary of the results from `compute_sensitivity_intervals`
#' or `compute_sensitivity_intervals_smooth`.
#'
#' By default, this method prints the widest intervals (one row per M or Mbar),
#' but the user can specify \code{type = "narrowest"} to see the narrowest intervals.
#'
#' @param object An object of class \code{sensitivity_intervals}.
#' @param type A string indicating which intervals to display.
#'   Must be either \code{"widest"} (default) or \code{"narrowest"}.
#' @param ... Additional arguments (currently unused).
#'
#' @return Prints a tibble to the console with columns:
#'   \code{lb}, \code{ub}, \code{method}, \code{Delta}, and either \code{M} or \code{Mbar}.
#'
#' @importFrom dplyr group_by filter ungroup rename select
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
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
#' # Print the widest intervals by default
#' summary(bounds_paper)
#'
#' # Print the narrowest intervals
#' summary(bounds_paper, type = "narrowest")
#' }
#'
#' @export
summary.sensitivity_intervals <- function(object,
                                          type = c("widest", "narrowest"),
                                          ...) {
  # Match the user's choice
  type <- match.arg(type)

  if (!inherits(object, "sensitivity_intervals")) {
    stop("The input object is not of class 'sensitivity_intervals'.")
  }

  # Identify whether M or Mbar is in all_intervals
  colnames_all <- colnames(object$all_intervals)
  param_name <- NULL
  if ("Mbar" %in% colnames_all) {
    param_name <- "Mbar"
  } else if ("M" %in% colnames_all) {
    param_name <- "M"
  }

  # Summary stats for printing at the end
  average_width <- mean(object$all_intervals$.data$interval_width, na.rm = TRUE)
  sd_width      <- sd(object$all_intervals$.data$interval_width, na.rm = TRUE)

  cat("Sensitivity Intervals Summary\n")
  cat("----------------------------------------\n")

  # If M or Mbar is found, group by it
  if (!is.null(param_name)) {
    intervals_summary <- dplyr::group_by(object$all_intervals, .data[[param_name]])
    intervals_summary <- dplyr::filter(
      intervals_summary,
      if (type == "widest") {
        .data$interval_width == max(.data$interval_width)
      } else {
        .data$interval_width == min(.data$interval_width)
      }
    )
    intervals_summary <- dplyr::ungroup(intervals_summary)

    # If grouping by Mbar, rename Mbar -> M; otherwise keep M
    if (param_name == "Mbar") {
      intervals_summary <- dplyr::rename(intervals_summary, M = .data$Mbar)
      intervals_summary <- dplyr::select(intervals_summary,
                                         .data$lb, .data$ub, .data$method,
                                         .data$Delta, .data$M)
    } else {
      intervals_summary <- dplyr::select(intervals_summary,
                                         .data$lb, .data$ub, .data$method,
                                         .data$Delta, .data$M)
    }

    if (type == "widest") {
      cat("WIDEST intervals (one per distinct", param_name, "):\n")
    } else {
      cat("NARROWEST intervals (one per distinct", param_name, "):\n")
    }
    print(intervals_summary)
    cat("\n")

  } else {
    # Fall back if no M or Mbar
    cat("Note: No M or Mbar column found. Falling back on original behavior.\n\n")

    if (type == "widest") {
      cat("Widest Interval:\n")
      print(object$widest_interval)
    } else {
      cat("Narrowest Interval:\n")
      print(object$narrowest_interval)
    }
    cat("\n")
  }

  # Print overall average and sd at the bottom
  avg <- round(average_width, 4)
  sdv <- round(sd_width, 4)
  cat(sprintf("Average Interval Width: %.4f (SD = %.4f)\n", avg, sdv))
  cat("----------------------------------------\n")
}

