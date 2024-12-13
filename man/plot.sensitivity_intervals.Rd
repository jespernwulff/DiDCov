% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_sensitivity_intervals.R
\name{plot.sensitivity_intervals}
\alias{plot.sensitivity_intervals}
\title{Plot Method for Sensitivity Intervals}
\usage{
\method{plot}{sensitivity_intervals}(x, honest_data = NULL, ...)
}
\arguments{
\item{x}{An object of class \code{sensitivity_intervals}.}

\item{honest_data}{Optional. A data frame containing HonestDiD intervals for each Mbar value.
Must have columns \code{lb}, \code{ub}, \code{Mbar}. If provided, an HonestDiD line (black) will be added.}

\item{...}{Additional arguments (currently unused).}
}
\value{
A ggplot object.
}
\description{
Creates a forest-style plot showing the widest and narrowest intervals computed using
\code{compute_sensitivity_intervals} or \code{compute_sensitivity_intervals_smooth}.
}
\details{
If multiple values of Mbar are present, it will facet the plot by Mbar.
If \code{honest_data} is provided, a third (HonestDiD) line is added. The order from bottom to top is:
Narrowest (bottom), Widest (middle), HonestDiD (top).
}
\examples{
# Simple example
ub <- c(-0.002, 0.015)
lb <- c(-0.012, 0.0065)
beta <- c(-0.007, 0.011)
years <- 1:2

bounds_paper <- compute_sensitivity_intervals(
  betahat = beta,
  ci_lower = lb,
  ci_upper = ub,
  years = years,
  numPrePeriods = 1,
  numPostPeriods = 1,
  Mbarvec = c(0.5, 1)
)

# Without honest_data:
plot(bounds_paper)

# With honest_data:
honest_data <- data.frame(
  lb = c(-0.001, 0.002),
  ub = c(0.009, 0.018),
  Mbar = c(0.5, 1)
)
plot(bounds_paper, honest_data = honest_data)

}
