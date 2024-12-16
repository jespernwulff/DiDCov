#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_vline scale_color_manual scale_y_continuous coord_cartesian labs theme_classic theme element_text margin facet_wrap
utils::globalVariables(c("y_pos", "lower", "upper", "interval_type", "Mbar"))

#' Plot Method for Sensitivity Intervals
#'
#' Creates a simple forest plot showing only the widest and narrowest intervals computed using
#' `compute_sensitivity_intervals` or `compute_sensitivity_intervals_smooth`.
#'
#' If multiple values of `Mbar` are present in the results, this function will:
#'   - Identify the widest and narrowest intervals for each `Mbar` value.
#'   - Stack them vertically by using facets (one facet per `Mbar`), sharing the same x-axis scale.
#'
#' @param x An object of class `sensitivity_intervals`.
#' @param ... Additional arguments (currently unused).
#' @return A ggplot object.
#' @examples
#' # Define simple data example
#' ub <- c(-0.002, 0.015)
#' lb <- c(-0.012, 0.0065)
#' beta <- c(-0.007, 0.011)
#' years <- 1:2
#'
#' # Compute the sensitivity intervals
#' bounds_paper <- compute_sensitivity_intervals(
#'   betahat = beta,
#'   ci_lower = lb,
#'   ci_upper = ub,
#'   years = years,
#'   numPrePeriods = 1,
#'   numPostPeriods = 1
#' )
#'
#' # Create a forest-style plot of the widest and narrowest intervals
#' plot(bounds_paper)
#' @export
plot.sensitivity_intervals <- function(x, ...) {
  if (!inherits(x, "sensitivity_intervals")) {
    stop("The input object is not of class 'sensitivity_intervals'.")
  }

  intervals <- x$all_intervals

  # Check if Mbar column exists and if multiple values of Mbar are present
  has_mbar <- "Mbar" %in% colnames(intervals)
  if (has_mbar) {
    unique_mbar <- unique(intervals$Mbar)
  } else {
    unique_mbar <- NULL
  }

  if (has_mbar && length(unique_mbar) > 1) {
    # Multiple Mbar values: find widest and narrowest intervals for each Mbar value
    plot_data_list <- lapply(unique_mbar, function(mval) {
      sub_data <- intervals[intervals$Mbar == mval, ]
      # Find widest and narrowest by interval_width
      widest_idx <- which.max(sub_data$interval_width)
      narrowest_idx <- which.min(sub_data$interval_width)

      # Extract widest and narrowest intervals
      w <- sub_data[widest_idx, ]
      n <- sub_data[narrowest_idx, ]

      # Construct a data frame similar to original plot_data
      beta_vals <- c((w$lb + w$ub)/2, (n$lb + n$ub)/2)
      lower_vals <- c(w$lb, n$lb)
      upper_vals <- c(w$ub, n$ub)
      itype <- factor(c("Widest Interval", "Narrowest Interval"),
                      levels = c("Widest Interval", "Narrowest Interval"))

      pd <- data.frame(
        Mbar = mval,
        beta = beta_vals,
        lower = lower_vals,
        upper = upper_vals,
        interval_type = itype
      )
      pd$y_pos <- as.numeric(pd$interval_type)
      pd
    })

    plot_data <- do.call(rbind, plot_data_list)
    facet_needed <- TRUE

  } else {
    # Single Mbar value or no Mbar column: original behavior
    plot_data <- data.frame(
      beta = c((x$widest_interval$lb + x$widest_interval$ub)/2,
               (x$narrowest_interval$lb + x$narrowest_interval$ub)/2),
      lower = c(x$widest_interval$lb, x$narrowest_interval$lb),
      upper = c(x$widest_interval$ub, x$narrowest_interval$ub),
      interval_type = factor(c("Widest Interval", "Narrowest Interval"),
                             levels = c("Widest Interval", "Narrowest Interval"))
    )
    plot_data$y_pos <- as.numeric(plot_data$interval_type)
    facet_needed <- FALSE
  }

  # Determine x-axis limits to center around zero
  max_abs <- max(abs(c(plot_data$lower, plot_data$upper)), na.rm = TRUE)
  x_limits <- c(-max_abs, max_abs)

  p <- ggplot(plot_data, aes(x = beta, y = y_pos)) +
    # Plot horizontal intervals only
    geom_errorbarh(aes(xmin = lower, xmax = upper, color = interval_type),
                   height = 0.3, linewidth = 0.8) +
    # Add a vertical line at zero
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    # Manual colors for two intervals
    scale_color_manual(values = c(
      "Widest Interval" = "#D55E00",   # Orange
      "Narrowest Interval" = "#0072B2" # Blue
    )) +
    # Adjust y-axis: label intervals, no axis title needed
    scale_y_continuous(
      breaks = c(1, 2),
      labels = c("Widest Interval", "Narrowest Interval"),
      name = NULL
    ) +
    # Keep the same x-axis scale for all facets
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

  # If multiple Mbar values, stack the plots vertically with shared x-axis
  if (facet_needed) {
    # "scales = 'free_y'" keeps the same x-axis scale but allows different y scaling per facet.
    # "ncol = 1" stacks the facets vertically.
    p <- p + facet_wrap(~Mbar, ncol = 1, scales = "free_y")
  }

  return(p)
}


