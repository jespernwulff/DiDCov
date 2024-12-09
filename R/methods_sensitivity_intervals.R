#' Summary Method for Sensitivity Intervals
#'
#' Provides a concise summary of the results from `compute_sensitivity_intervals`.
#'
#' @param object An object of class `sensitivity_intervals`.
#' @param ... Additional arguments (currently unused).
#' @return Prints a summary to the console.
#' @export
summary.sensitivity_intervals <- function(object, ...) {
  if (!inherits(object, "sensitivity_intervals")) {
    stop("The input object is not of class 'sensitivity_intervals'.")
  }

  cat("Sensitivity Interval Summary:\n")
  cat("----------------------------------------------------\n")
  cat("Widest Interval:\n")
  print(object$widest_interval)
  cat("\nNarrowest Interval:\n")
  print(object$narrowest_interval)
  cat("\nSummary Statistics for Interval Widths:\n")
  cat(sprintf("Average Interval Width: %.4f\n", object$average_width))
  cat(sprintf("Standard Deviation of Interval Widths: %.4f\n", object$sd_width))
  cat("----------------------------------------------------\n")
}
