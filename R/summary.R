#' Summarize results
#' 
#' @description `summary` functions take the output and summarize it.
#' 
#' @param x an output object
#' @inheritParams rlang::args_dots_empty
#' 
#' @details Currently, observed densities (class: obs) and estimates (class: est) are supported by this function.

#' @export
summary <- function (x) UseMethod("summary", x)

#' @importFrom stats summary
#' @export summary

summary <- summary

#' @importFrom spatstat.model summary
#' @export summary

summary <- summary

#' @rdname summary
#' @method summary obs
#' @export 
summary.obs <- function(x) {
  obs_density <- x
  data.frame(Variabe = c("Constant", obs_density$indep_var),
             Coefficient = obs_density$coef)
}

#' @rdname summary
#' @method summary est
#' @export
summary.est <- function(x) {
  
  estimates <- x
  
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

