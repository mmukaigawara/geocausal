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
