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
summary.obs <- function(object, ...) {
  obs_density <- object
  data.frame(Variable = c("Constant", obs_density$indep_var),
             Coefficient = obs_density$coef)
}
