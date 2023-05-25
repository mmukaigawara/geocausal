#' Function: get_power_density
#'
#' A function that takes the target densities and their priorities
#' and returns a power density image
#'
#' @param target_densities A list of target densities
#' @param priorities A vector of priorities for each of target densities
#' @param window An owin object


get_power_density <- function(target_densities,
                              priorities,
                              window) {
  
  power_density <- 1
  
  for (ii in 1:length(target_densities)){
    power_density <- power_density * target_densities[[ii]] ^ priorities[ii]
  }
  
  power_density <- power_density/integral(power_density, domain = iraq_window)
  
  return(power_density)
  
}