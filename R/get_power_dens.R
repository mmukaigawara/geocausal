#' Get power densities
#'
#' @description
#' `get_power_dens()` takes the target densities and their priorities
#' and returns a power density.
#'
#' @param target_dens list of target densities
#' @param priorities vector of priorities for each of target densities
#' @param window owin object
#'
#' @returns an im object of power densities
#'
#' @details `get_power_dens()` raises each target density to the power given by
#' the corresponding entry of `priorities`, multiplies the results together, and
#' normalizes the product to integrate to one over `window`. The resulting power
#' density is used to shift the locations of counterfactual events relative to
#' the baseline density.
#'
#' @references
#' Papadogeorgou, G., Imai, K., Lyall, J. and Li, F. (2022). Causal inference with spatio-temporal data: estimating the effects of airstrikes on insurgent violence in Iraq. \emph{Journal of the Royal Statistical Society Series B}, 84(5), 1969--1999. \doi{10.1111/rssb.12548}
#'
#' Mukaigawara, M., Imai, K., Lyall, J. and Papadogeorgou, G. (2025). Spatiotemporal causal inference with arbitrary spillover and carryover effects. arXiv preprint. \doi{10.48550/arXiv.2504.03464}
#'
#' @seealso [get_cf_dens()], [sim_power_dens()]
#'
#' @family density estimation functions
#'
#' @examples
#' \donttest{
#' # A target density: proportional to the distance from Baghdad
#' dist_baghdad <- get_dist_focus(window = iraq_window,
#'                                lon = 44.366, lat = 33.315, ndim = 64)
#' power <- get_power_dens(target_dens = list(dist_baghdad / sum(dist_baghdad)),
#'                         priorities = 1, window = iraq_window)
#' }

get_power_dens <- function(target_dens,
                           priorities,
                           window) {

  # Obtaining each target density ^ alpha
  target_alpha <- lapply(1:length(target_dens), function(x) target_dens[[x]] ^ priorities[x])

  power_density <- target_alpha[[1]]

  if (length(target_dens) > 1){

    for (ii in 2:length(target_dens)){ suppressWarnings( #Suppress warnings
      power_density <- power_density * target_alpha[[ii]]
      ) }
  }

  power_density <- power_density/spatstat.univar::integral(power_density, domain = window)

  return(power_density)

}
