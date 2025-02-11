#' Compute DeltaSD Bound (Lower or Upper) Across Multiple Rho Values, With Optional Scaling
#'
#' This function:
#' \enumerate{
#'   \item Approximates the variance of each estimate from \code{ci_lower}, \code{ci_upper}, and \code{ci_level}.
#'   \item For each \code{rho} in \code{rho_values}, constructs a covariance matrix assuming constant correlation \eqn{\rho}.
#'   \item Optionally scales \code{betahat} and \code{sigma} before calling
#'         \code{DeltaSD_lowerBound_Mpre} or \code{DeltaSD_upperBound_Mpre}.
#'   \item Aggregates the resulting single-number bounds (across all \code{rho}) by returning
#'         the \code{smallest}, \code{largest}, or \code{median}.
#' }
#'
#' @param betahat Numeric vector of estimated effects (e.g., from an event-study DiD).
#' @param ci_lower Numeric vector of lower CI bounds, same length as \code{betahat}.
#' @param ci_upper Numeric vector of upper CI bounds, same length as \code{betahat}.
#' @param numPrePeriods Integer indicating the number of pre-treatment periods.
#' @param rho_values Numeric vector in \code{[-1, 1]} for constant correlation assumptions.
#' @param ci_level Numeric, the confidence level used for constructing variances from CIs (default \code{0.95}).
#' @param aggregator Character in \code{c("smallest", "largest", "median")}.
#'   Determines how to combine the set of computed bounds. Defaults to \code{"smallest"}.
#' @param bound_type Character in \code{c("lower", "upper")}, specifying which function to call:
#'   \code{"lower"} uses \code{DeltaSD_lowerBound_Mpre}, \code{"upper"} uses \code{DeltaSD_upperBound_Mpre}.
#' @param scale Logical indicating whether to internally scale \code{betahat} and \code{sigma}.
#'   Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to the underlying HonestDiD function
#'   (e.g., \code{numPostPeriods}).
#'
#' @return A single numeric value (the aggregated DeltaSD bound). If \code{scale = TRUE},
#'   the bound is on scaled units. To convert back to the original scale, multiply the result
#'   by \eqn{\sqrt{\sigma_{\mathrm{post}}}}, where \eqn{\sigma_{\mathrm{post}}} is the variance
#'   of the first post-treatment period before scaling.
#'
#' @importFrom stats median
#' @importFrom HonestDiD DeltaSD_lowerBound_Mpre DeltaSD_upperBound_Mpre
#' @export
compute_bound <- function(
    betahat,
    ci_lower,
    ci_upper,
    numPrePeriods,
    rho_values =  c(0, 0.3, 0.5, 0.8),
    ci_level = 0.95,
    aggregator = c("smallest", "largest", "median"),
    bound_type = c("lower", "upper"),
    scale = FALSE,
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

    # If scale=TRUE, compute scaleFactor and adjust betahat & sigma
    if (scale) {
      var_post <- sigma_i[numPrePeriods + 1, numPrePeriods + 1]
      if (var_post <= 0) {
        stop("Cannot scale: the variance of the first post-treatment period is non-positive.")
      }

      scaleFactor    <- 1 / sqrt(var_post)
      betahat_scaled <- betahat * scaleFactor
      sigma_scaled   <- sigma_i * (scaleFactor^2)

      # Call HonestDiD with scaled arguments
      if (bound_type == "lower") {
        val <- HonestDiD::DeltaSD_lowerBound_Mpre(
          betahat       = betahat_scaled,
          sigma         = sigma_scaled,
          numPrePeriods = numPrePeriods,
          ...
        )
      } else {
        val <- HonestDiD::DeltaSD_upperBound_Mpre(
          betahat       = betahat_scaled,
          sigma         = sigma_scaled,
          numPrePeriods = numPrePeriods,
          ...
        )
      }
    } else {
      # No scaling
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
