#' Summarize estimated results
#'
#' @description A function that summarizes estimated results
#'
#' @param estimates an object returned from `get_est()` function
#' 
#' @returns list of results (as a dataframe) and windows

sum_est <- function(estimates) {
  
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