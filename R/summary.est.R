#' Summarize results 
#' 
#' @description `summary` functions take the output and summarize it.
#' 
#' @param object an output object
#' @param ... arguments passed on to the function
#' 
#' @details Currently, observed densities (class: obs) and estimates (class: est) are supported by this function.
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

