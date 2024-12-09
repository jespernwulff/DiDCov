#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_vline scale_color_manual scale_y_continuous coord_cartesian labs theme_classic theme element_text margin
utils::globalVariables(c("y_pos", "lower", "upper", "interval_type"))

#' Plot Method for Sensitivity Intervals
#'
#' Creates a simple forest plot showing only the widest and narrowest intervals.
#' No points, no title, and just two intervals displayed in a clean, publication-ready style.
#'
#' @param x An object of class `sensitivity_intervals`.
#' @param ... Additional arguments (currently unused).
#' @return A ggplot object.
#' @export
plot.sensitivity_intervals <- function(x, ...) {
  if (!inherits(x, "sensitivity_intervals")) {
    stop("The input object is not of class 'sensitivity_intervals'.")
  }

  # Construct a simple data frame with widest and narrowest intervals
  plot_data <- data.frame(
    beta = c((x$widest_interval$lb + x$widest_interval$ub)/2,
             (x$narrowest_interval$lb + x$narrowest_interval$ub)/2),
    lower = c(x$widest_interval$lb, x$narrowest_interval$lb),
    upper = c(x$widest_interval$ub, x$narrowest_interval$ub),
    interval_type = factor(c("Widest Interval", "Narrowest Interval"),
                           levels = c("Widest Interval", "Narrowest Interval"))
  )

  # Assign y-axis positions: widest on top (y=2) and narrowest on bottom (y=1)
  plot_data$y_pos <- as.numeric(plot_data$interval_type)

  # Determine x-axis limits to center around zero
  max_abs <- max(abs(c(plot_data$lower, plot_data$upper)), na.rm = TRUE)
  x_limits <- c(-max_abs, max_abs)

  # Create the plot
  p <- ggplot(plot_data, aes(x = beta, y = y_pos)) +
    # Plot horizontal intervals only
    geom_errorbarh(aes(xmin = lower, xmax = upper, color = interval_type),
                   height = 0.3, linewidth = 0.8) +
    # Add a vertical line at zero
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    # Manual colors: two intervals, two colors
    scale_color_manual(values = c(
      "Widest Interval" = "#D55E00",   # Orange
      "Narrowest Interval" = "#0072B2" # Blue
    )) +
    # Adjust y-axis: label intervals, no axis title needed
    scale_y_continuous(
      breaks = c(1, 2),
      labels = c("Narrowest Interval", "Widest Interval"),
      name = NULL
    ) +
    # Center around zero with defined x-limits
    coord_cartesian(xlim = x_limits) +
    # Remove legend, no title
    labs(x = NULL, y = NULL, color = NULL) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )

  return(p)
}

