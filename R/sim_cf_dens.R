#' Simulate counterfactual densities
#'
#' @description
#' `sim_cf_dens()` takes a list of power densities
#' and returns simulated counterfactual densities.
#'
#' @param expected_number the expected number of observations
#' @param base_dens the baseline density (im object)
#' @param power_sim_results the results obtained by `simulate_power_density()`
#' @param window owin object
#'
#' @returns list of counterfactual densities, power as numerics, and expected number as a numeric
#'
#' @details `sim_cf_dens()` combines a baseline density with each of the power
#' densities produced by `sim_power_dens()`. For every simulated set of
#' priorities, the baseline density is multiplied by the corresponding power
#' density, normalized over `window`, and rescaled to the supplied
#' `expected_number` of events. The result is a `cflist` object holding the list
#' of counterfactual densities together with the associated powers and expected
#' number.
#'
#' @references
#' Papadogeorgou, G., Imai, K., Lyall, J. and Li, F. (2022). Causal inference with spatio-temporal data: estimating the effects of airstrikes on insurgent violence in Iraq. \emph{Journal of the Royal Statistical Society Series B}, 84(5), 1969--1999. \doi{10.1111/rssb.12548}
#'
#' Mukaigawara, M., Imai, K., Lyall, J. and Papadogeorgou, G. (2025). Spatiotemporal causal inference with arbitrary spillover and carryover effects. arXiv preprint. \doi{10.48550/arXiv.2504.03464}
#'
#' @seealso [sim_power_dens()], [get_distexp()]
#'
#' @family density estimation functions

sim_cf_dens <- function(expected_number,
                        base_dens,
                        power_sim_results,
                        window) {

  power_densities <- power_sim_results$power_density_list
  powers <- power_sim_results$priorities_manip

  # Obtaining a list of counterfactual densities based on simluations
  counterfactual_density_list <- lapply(1:length(power_densities),
                                        function(x) { #Suppress warnings for incompatibility of images
                                          suppressWarnings( product_power_baseline <- base_dens * power_densities[[x]] )
                                          counterfactual_density <- product_power_baseline/
                                            integral(product_power_baseline, W = window) * expected_number
                                          return(counterfactual_density)
                                        }
  )

  clist <- list(cf_density_list = counterfactual_density_list,
                powers = powers,
                expected_number = expected_number)

  class(clist) <- c("cflist", "list")
  return(clist)

}
