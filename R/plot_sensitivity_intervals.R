#' @importFrom ggplot2 ggplot aes geom_errorbarh geom_vline scale_color_manual scale_y_continuous coord_cartesian labs theme_classic theme element_text margin facet_wrap
#' @importFrom stats as.formula
utils::globalVariables(c("y_pos", "lower", "upper", "interval_type", "Mbar", "M"))

#' Plot Method for Sensitivity Intervals
#'
#' Creates a forest-style plot:
#' - Without honest_data: Plots Narrowest (bottom) and Widest (top) intervals.
#' - With honest_data: Adds HonestDiD interval on top, resulting in Narrowest (1), Widest (2), HonestDiD (3).
#'
#' If multiple values of Mbar or M are present, it facets by that variable. If only one value (or none) is found, it produces a single plot.
#'
#' @param x An object of class `sensitivity_intervals`.
#' @param honest_data Optional. Data frame with columns `lb`, `ub`, and either `Mbar` or `M` matching the variable in `x$all_intervals`.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot object.
#' @export
plot.sensitivity_intervals <- function(x, honest_data = NULL, ...) {
  if (!inherits(x, "sensitivity_intervals")) {
    stop("The input object is not of class 'sensitivity_intervals'.")
  }

  intervals <- x$all_intervals

  # Determine which variable to facet by: prefer Mbar if present, otherwise M
  if ("Mbar" %in% colnames(intervals)) {
    facet_var <- "Mbar"
  } else if ("M" %in% colnames(intervals)) {
    facet_var <- "M"
  } else {
    facet_var <- NULL
  }

  # Helper function to build plot data for a subset of intervals corresponding to one Mbar/M value (or the whole set if no facet_var)
  make_plot_data <- function(sub_data, facet_value = NULL) {
    # Find widest and narrowest
    widest_idx <- which.max(sub_data$interval_width)
    narrowest_idx <- which.min(sub_data$interval_width)

    w <- sub_data[widest_idx, ]
    n <- sub_data[narrowest_idx, ]

    if (is.null(honest_data)) {
      # No HonestDiD: Just Narrowest (1) and Widest (2)
      interval_types <- c("Narrowest", "Widest")
      beta_vals <- c((n$lb + n$ub)/2, (w$lb + w$ub)/2)
      lower_vals <- c(n$lb, w$lb)
      upper_vals <- c(n$ub, w$ub)
    } else {
      # With HonestDiD: Narrowest (1), Widest (2), HonestDiD (3)
      # Need to find the matching HonestDiD interval
      if (!is.null(facet_var) && !is.null(facet_value)) {
        h_sub <- honest_data[honest_data[[facet_var]] == facet_value, , drop = FALSE]
      } else {
        # No facet_var or single scenario: use the first row of honest_data if it exists
        h_sub <- honest_data
      }

      if (nrow(h_sub) != 1) {
        stop("Expected exactly one matching HonestDiD interval.")
      }

      interval_types <- c("Narrowest", "Widest", "HonestDiD")
      beta_vals <- c((n$lb + n$ub)/2, (w$lb + w$ub)/2, (h_sub$lb + h_sub$ub)/2)
      lower_vals <- c(n$lb, w$lb, h_sub$lb)
      upper_vals <- c(n$ub, w$ub, h_sub$ub)
    }

    pd <- data.frame(
      beta = beta_vals,
      lower = lower_vals,
      upper = upper_vals,
      interval_type = factor(interval_types, levels = interval_types),
      stringsAsFactors = FALSE
    )
    pd$y_pos <- as.numeric(pd$interval_type)

    # If faceting, add facet column
    if (!is.null(facet_var) && !is.null(facet_value)) {
      pd[[facet_var]] <- facet_value
    }

    pd
  }

  # Build plot data for all values of the facet variable or single scenario
  if (!is.null(facet_var)) {
    unique_vals <- unique(intervals[[facet_var]])
    plot_data_list <- lapply(unique_vals, function(v) {
      sub_data <- intervals[intervals[[facet_var]] == v, , drop = FALSE]
      make_plot_data(sub_data, facet_value = v)
    })
    plot_data <- do.call(rbind, plot_data_list)
    facet_needed <- length(unique_vals) > 1
  } else {
    # No facet variable: single scenario
    plot_data <- make_plot_data(intervals)
    facet_needed <- FALSE
  }

  # Determine x-limits
  max_abs <- max(abs(c(plot_data$lower, plot_data$upper)), na.rm = TRUE)
  x_limits <- c(-max_abs, max_abs)

  # Colors and y-axis depend on honest_data presence
  if (is.null(honest_data)) {
    # Two intervals: Narrowest, Widest
    color_values <- c("Widest" = "#D55E00", "Narrowest" = "#0072B2")
    y_breaks <- c(1, 2)
    y_labels <- c("Narrowest", "Widest")
  } else {
    # Three intervals: Narrowest, Widest, HonestDiD
    color_values <- c("Widest" = "#D55E00", "Narrowest" = "#0072B2", "HonestDiD" = "#000000")
    y_breaks <- c(1, 2, 3)
    y_labels <- c("Narrowest", "Widest", "HonestDiD")
  }

  p <- ggplot(plot_data, aes(x = beta, y = y_pos)) +
    geom_errorbarh(aes(xmin = lower, xmax = upper, color = interval_type),
                   height = 0.3, linewidth = 0.8) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    scale_color_manual(values = color_values) +
    scale_y_continuous(breaks = y_breaks, labels = y_labels, name = NULL) +
    coord_cartesian(xlim = x_limits) +
    labs(x = NULL, y = NULL, color = NULL) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )

  if (facet_needed) {
    p <- p + facet_wrap(as.formula(paste0("~", facet_var)), ncol = 1, scales = "free_y")
  }

  return(p)
}




