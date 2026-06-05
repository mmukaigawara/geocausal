#' Summarize results
#'
#' @description Summary method for objects of class `obs`, returned by
#' [get_obs_dens()]. Prints a short summary of the fitted observed-density model
#' (dispersion parameter, deviance, and null deviance) and returns the estimated
#' model coefficients.
#'
#' @param object an object of class `obs`, typically the output of [get_obs_dens()].
#' @param ... additional arguments. Currently ignored.
#'
#' @details Currently, observed densities (class: obs) and estimates (class: est) are supported by this function.
#'
#' @returns The estimated model coefficients (`object$coef`), returned alongside
#' the printed model summary.
#'
#' @seealso [get_obs_dens()]
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
