#' Simulate power densities
#'
#' @description A function that takes the target densities and their priorities
#' and returns a power density image over a range of parameters
#'
#' @param target_dens list of target densities. This should always be a list, even if there is only one target density.
#' @param dens_manip a target density for which we manipulate the value of priorities
#' @param priorities numeric. `priorities` specifies the priority for the target density that we do not manipulate.
#' @param priorities_manip vector of priorities for the density that we manipulate.
#' @param window owin object
#'
#' @returns list of densities and priorities
#'
#' @details `sim_power_dens()` generates a sequence of power densities by holding
#' the priorities of the fixed target densities at `priorities` while varying the
#' priority of `dens_manip` across the values in `priorities_manip`. For each such
#' value, the product of the powered target densities is normalized over `window`.
#' The result is a `powerlist` object containing the list of power densities and
#' the manipulated priorities, and is typically passed to `sim_cf_dens()`.
#'
#' @references
#' Papadogeorgou, G., Imai, K., Lyall, J. and Li, F. (2022). Causal inference with spatio-temporal data: estimating the effects of airstrikes on insurgent violence in Iraq. \emph{Journal of the Royal Statistical Society Series B}, 84(5), 1969--1999. \doi{10.1111/rssb.12548}
#'
#' Mukaigawara, M., Imai, K., Lyall, J. and Papadogeorgou, G. (2025). Spatiotemporal causal inference with arbitrary spillover and carryover effects. arXiv preprint. \doi{10.48550/arXiv.2504.03464}
#'
#' @family density estimation functions

sim_power_dens <- function(target_dens, #This must be a list element
                           dens_manip,
                           priorities,
                           priorities_manip,
                           window) {

  # Obtaining each target density ^ alpha
  target_alpha <- lapply(1:length(target_dens), function(x) target_dens[[x]] ^ priorities[x])

  power_density <- target_alpha[[1]]

  if (length(target_dens) > 1){

    for (ii in 2:length(target_dens)){ power_density <- power_density * target_alpha[[ii]] }

  }

  power_density_list <- list()

  for (jj in 1:length(priorities_manip)){
    temp <- power_density * dens_manip ^ priorities_manip[jj]
    temp <- temp/spatstat.univar::integral(temp, domain = window)
    power_density_list[[jj]] <- temp
  }

  power_density_list <- lapply(1:length(priorities_manip),
                               function(x) {
                                 temp <- power_density * dens_manip ^ priorities_manip[x]
                                 temp <- temp/spatstat.univar::integral(temp, domain = window)
                                 return(temp)
                               }
  )

  plist <- list(power_density_list = power_density_list,
                priorities_manip = priorities_manip)

  class(plist) <- c("powerlist", "list")
  return(plist)

}
