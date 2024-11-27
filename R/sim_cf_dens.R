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
