#' Print results
#'
#' @description Print method for objects of class `est`, returned by [get_est()].
#' Builds and returns a data frame of the estimated causal effects per time
#' period, together with their 90% and 95% confidence intervals.
#'
#' @param x an object of class `est`, typically the output of [get_est()].
#' @param ... additional arguments. Currently ignored.
#'
#' @details Currently, observed densities (class: obs) and estimates (class: est) are supported by this function.
#'
#' @returns A data frame with one row per window, containing the point estimate
#' and the lower/upper bounds of the 90% and 95% confidence intervals.
#'
#' @seealso [get_est()]
#'
#' @export
print.est <- function(x, ...) {

  estimates <- x

  result <- data.frame(
    window = seq(1, length(estimates$windows)),
    point_estimate = estimates$est_causal,
    upper_95 = estimates$est_causal + 1.96 * sqrt(estimates$var_causal),
    lower_95 = estimates$est_causal - 1.96 * sqrt(estimates$var_causal),
    upper_90 = estimates$est_causal + 1.645 * sqrt(estimates$var_causal),
    lower_90 = estimates$est_causal - 1.645 * sqrt(estimates$var_causal)
  )

  return(result)

}
