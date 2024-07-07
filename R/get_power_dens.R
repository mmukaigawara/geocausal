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
#' @returns list of an im object and a ggplot object of power densities


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
