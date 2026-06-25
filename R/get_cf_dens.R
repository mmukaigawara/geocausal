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
#'
#' @references
#' Papadogeorgou, G., Imai, K., Lyall, J. and Li, F. (2022). Causal inference with spatio-temporal data: estimating the effects of airstrikes on insurgent violence in Iraq. \emph{Journal of the Royal Statistical Society Series B}, 84(5), 1969--1999. \doi{10.1111/rssb.12548}
#'
#' Mukaigawara, M., Imai, K., Lyall, J. and Papadogeorgou, G. (2025). Spatiotemporal causal inference with arbitrary spillover and carryover effects. arXiv preprint. \doi{10.48550/arXiv.2504.03464}
#'
#' @seealso [get_obs_dens()], [get_est()]
#'
#' @family density estimation functions
#'
#' @examples
#' \donttest{
#' # Baseline density from out-of-sample (2007--2008) insurgency data
#' base <- get_base_dens(window = iraq_window, ndim = 64,
#'                       out_data = insurgencies,
#'                       out_coordinates = c("longitude", "latitude"))
#'
#' # Counterfactual scenario: two airstrikes per day on average,
#' # distributed proportionally to the baseline density
#' cf <- get_cf_dens(expected_number = 2, base_dens = base,
#'                   window = iraq_window)
#' }

get_cf_dens <- function(expected_number,
                        base_dens,
                        power_dens = NA,
                        window) {

  if (inherits(power_dens, "logical")) { # identify NA; counterfactual_type = intensity only -> multiply by the expectation

    cf_density <- expected_number * base_dens

  } else { # counterfactual_type = location as well

    product_power_baseline <- base_dens * power_dens
    cf_density <- product_power_baseline/
      integral(product_power_baseline, W = window) * expected_number

  }

  return(cf_density)

}
