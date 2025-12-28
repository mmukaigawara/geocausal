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
  message("\n----- Observed Density Model Summary -----\n")
  message(sprintf("Dispersion parameter: %.4f\n", obs_density$dispersion))
  message(sprintf("Deviance: %.2f; Null deviance: %.2f\n",
                  obs_density$deviance, obs_density$null_deviance))
  message("------------------------------------------\n\n")
  return(obs_density$coef)
}
