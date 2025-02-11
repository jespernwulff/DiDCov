#' Compute DeltaSD Bound (Lower or Upper) Across Multiple Rho Values
#'
#' This function:
#' \enumerate{
#'   \item Approximates the variance of each estimate from \code{ci_lower}, \code{ci_upper}, and \code{ci_level}.
#'   \item For each \code{rho} in \code{rho_values}, constructs a covariance matrix assuming constant correlation \eqn{\rho}.
#'   \item Calls either \code{DeltaSD_lowerBound_Mpre} or \code{DeltaSD_upperBound_Mpre} from \pkg{HonestDiD}, as specified by \code{bound_type}.
#'   \item Aggregates the resulting single-number bounds (across all \code{rho}) by returning the \code{smallest}, \code{largest}, or \code{median}.
#' }
#' No internal scaling is performed.
#'
#' @param betahat Numeric vector of estimated effects (e.g., from an event-study DiD).
#' @param ci_lower Numeric vector of lower CI bounds, same length as \code{betahat}.
#' @param ci_upper Numeric vector of upper CI bounds, same length as \code{betahat}.
#' @param numPrePeriods Integer indicating the number of pre-treatment periods.
#' @param rho_values Numeric vector in \code{[-1, 1]} for constant correlation assumptions.
#' @param ci_level Numeric, the confidence level used for constructing variances from CIs (default 0.95).
#' @param aggregator Character in \code{c("smallest", "largest", "median")}.
#'   Determines how to combine the set of computed bounds. Defaults to \code{"smallest"}.
#' @param bound_type Character in \code{c("lower", "upper")}, specifying which function to call:
#'   \code{"lower"} uses \code{DeltaSD_lowerBound_Mpre}, \code{"upper"} uses \code{DeltaSD_upperBound_Mpre}.
#' @param ... Additional arguments passed to the underlying HonestDiD function
#'   (e.g., \code{numPostPeriods}, \code{M}, etc.).
#'
#' @return A single numeric value (the aggregated DeltaSD bound).
#'
#' @importFrom stats median
#' @importFrom HonestDiD DeltaSD_lowerBound_Mpre DeltaSD_upperBound_Mpre
#' @examples
#' # Example with 5 time points in total -> 3 pre-treatment + 2 post-treatment
#' betahat <- c(0.1, 0.2, 0.25, 0.3, 0.4)  # point estimates
#' ci_lower <- betahat - 0.05
#' ci_upper <- betahat + 0.05
#'
#' # Vector of possible correlation assumptions
#' rho_values <- c(-0.5, 0, 0.5)
#'
#' # Compute a lower bound, aggregating ("smallest") over these rho values
#' bound_val <- compute_bound(
#'   betahat       = betahat,
#'   ci_lower      = ci_lower,
#'   ci_upper      = ci_upper,
#'   numPrePeriods = 3,          # Must be >= 2 for HonestDiD's internal logic
#'   rho_values    = rho_values,
#'   ci_level      = 0.95,
#'   aggregator    = "smallest",
#'   bound_type    = "lower"
#'   # Optionally pass other args, e.g. numPostPeriods = 2
#' )
#'
#' print(bound_val)
#'
#' # You can switch to an upper bound or different aggregator:
#' # aggregator = "largest", bound_type = "upper", etc.
#' @export
compute_bound <- function(
    betahat,
    ci_lower,
    ci_upper,
    numPrePeriods,
    rho_values,
    ci_level = 0.95,
    aggregator = c("smallest", "largest", "median"),
    bound_type = c("lower", "upper"),
    ...
) {
  aggregator <- match.arg(aggregator)
  bound_type <- match.arg(bound_type)

  # Basic checks
  if (length(betahat) != length(ci_lower) ||
      length(betahat) != length(ci_upper)) {
    stop("betahat, ci_lower, and ci_upper must have the same length.")
  }
  if (!all(ci_lower <= ci_upper)) {
    stop("Each ci_lower must be <= the corresponding ci_upper.")
  }

  # 1. Compute approximate variances from the CIs
  variances <- compute_variances_from_ci(
    betahat,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    ci_level = ci_level
  )

  # 2. For each rho, build the covariance matrix & compute the bound
  bounds <- numeric(length(rho_values))
  for (i in seq_along(rho_values)) {
    rho <- rho_values[i]

    sigma_i <- construct_cov_matrix(variances, rho)

    # Depending on bound_type, call lowerBound or upperBound
    if (bound_type == "lower") {
      val <- HonestDiD::DeltaSD_lowerBound_Mpre(
        betahat       = betahat,
        sigma         = sigma_i,
        numPrePeriods = numPrePeriods,
        ...
      )
    } else {
      val <- HonestDiD::DeltaSD_upperBound_Mpre(
        betahat       = betahat,
        sigma         = sigma_i,
        numPrePeriods = numPrePeriods,
        ...
      )
    }
    bounds[i] <- val
  }

  # 3. Aggregate
  out <- switch(
    aggregator,
    "smallest" = min(bounds),
    "largest"  = max(bounds),
    "median"   = median(bounds)
  )
  return(out)
}
