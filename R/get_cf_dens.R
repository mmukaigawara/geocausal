#' Get counterfactual densities
#'
#' @description `get_cf_dens` takes the target (expected) number, baseline density,
#' and power density, and generates a hyperframe with counterfactual densities.
#'
#' @param expected_number the expected number of observations.
#' @param base_dens baseline density (im object)
#' @param power_dens power density (im object)
#' @param window owin object
#'
#' @returns an im object of a counterfactual density
#'
#' @details There are two ways of generating counterfactual densities.
#' First, users can keep the locations of observations as they are and change the expected number of observations.
#' In this case, users do not have to set `power_dens` and simply modify `expected_number`.
#' Alternatively, users can shift the locations as well. In this case, `power_dens` must be specified.
#' To obtain power densities, refer to [get_power_dens()].

get_cf_dens <- function(expected_number,
                        base_dens,
                        power_dens = NA,
                        window) {

  if (is.na(power_dens[1])) { # counterfactual_type = intensity only -> multiply by the expectation

    cf_density <- expected_number * base_dens

  } else { # counterfactual_type = location as well

    product_power_baseline <- base_dens * power_dens
    cf_density <- product_power_baseline/
      integral(product_power_baseline, W = window) * expected_number

  }

  return(cf_density)

}
