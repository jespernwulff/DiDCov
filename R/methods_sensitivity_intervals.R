#' Summary Method for Sensitivity Intervals
#'
#' @param object An object of class \code{sensitivity_intervals}.
#' @param type Either "widest" (default) or "narrowest".
#' @param ... Currently unused.
#'
#' @importFrom dplyr group_by filter ungroup rename select
#' @importFrom rlang .data
#' @export
summary.sensitivity_intervals <- function(object,
                                          type = c("widest", "narrowest"),
                                          ...) {
  # Match user's choice
  type <- match.arg(type)

  if (!inherits(object, "sensitivity_intervals")) {
    stop("The input object is not of class 'sensitivity_intervals'.")
  }

  # Identify whether we have Mbar or M in the data
  colnames_all <- colnames(object$all_intervals)
  param_name <- NULL
  if ("Mbar" %in% colnames_all) {
    param_name <- "Mbar"
  } else if ("M" %in% colnames_all) {
    param_name <- "M"
  }

  # Calculate overall summary statistics in base R style
  average_width <- mean(object$all_intervals$interval_width, na.rm = TRUE)
  sd_width      <- sd(object$all_intervals$interval_width, na.rm = TRUE)

  cat("Sensitivity Intervals Summary\n")
  cat("----------------------------------------\n")

  if (!is.null(param_name)) {
    # Group by the chosen parameter and filter for widest/narrowest
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

    # If grouping by Mbar, rename() to M after the fact
    if (param_name == "Mbar") {
      intervals_summary <- dplyr::rename(intervals_summary, M = "Mbar")
      intervals_summary <- dplyr::select(intervals_summary,
                                         "lb", "ub", "method", "Delta", "M")
    } else {
      # param_name == "M"
      intervals_summary <- dplyr::select(intervals_summary,
                                         "lb", "ub", "method", "Delta", "M")
    }

    # Print
    if (type == "widest") {
      cat("WIDEST intervals (one per distinct", param_name, "):\n")
    } else {
      cat("NARROWEST intervals (one per distinct", param_name, "):\n")
    }
    print(intervals_summary)
    cat("\n")

  } else {
    # Fallback if no M or Mbar
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
