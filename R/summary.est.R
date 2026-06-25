#' Summarize results
#'
#' @description Summary method for objects of class `est`, returned by [get_est()].
#' Summarizes the estimated causal effects per time period (with 90% and 95%
#' confidence intervals) together with the associated windows.
#'
#' @param object an object of class `est`, typically the output of [get_est()].
#' @param ... additional arguments. Currently ignored.
#'
#' @details Currently, observed densities (class: obs) and estimates (class: est) are supported by this function.
#'
#' @returns A named list with two elements: `result`, a data frame of the point
#' estimates and 90%/95% confidence intervals per window, and `windows`, the list
#' of windows used in estimation.
#'
#' @seealso [get_est()]
#'
#' @export
summary.est <- function(object, ...) {
  
  estimates <- object
  
  #1. Summary 1: Causal effects with 95% and 90% CIs
  result <- data.frame(
    window = seq(1, length(estimates$windows)),
    point_estimate = estimates$est_causal,
    upper_95 = estimates$est_causal + 1.96 * sqrt(estimates$var_causal),
    lower_95 = estimates$est_causal - 1.96 * sqrt(estimates$var_causal),
    upper_90 = estimates$est_causal + 1.645 * sqrt(estimates$var_causal),
    lower_90 = estimates$est_causal - 1.645 * sqrt(estimates$var_causal)
  )
  
  #2. Summary 2: All windows
  windows <- estimates$windows
  
  return(list(result = result, windows = windows))
  
}

