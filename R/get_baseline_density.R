#' Function: get_baseline_density
#'
#' A function that takes a dataframe and returns the baseline densities
#' using Scott's rule of thumb
#'
#' @param data A dataframe
#' @param coordinates A vector of column names for longitudes and latitudes (in this order)
#' @param indep_var A vector of names of independent variables
#' @param window The window object of interest
#' @param return_ppp If TRUE, the function returns both the density and point processes. By default, FALSE

get_baseline_density <- function(data, 
                                 coordinates = c("longitude", "latitude"),
                                 window,
                                 return_ppp = FALSE){
  
  # Convert data to ppp
  coordinates_data <- airstr_base[, coordinates]
  baseline_ppp <- spatstat.geom::as.ppp(coordinates_data, W = window)
  
  # Apply Scott's rule of thumb
  scott_bandwidth <- spatstat.explore::bw.scott(baseline_ppp)
  
  baseline_density <- stats::density(baseline_ppp, scott_bandwidth) #Kernel density estimation
  baseline_density <- baseline_density / integral(baseline_density) #Divide by integral of the density
  
  
  if (return_ppp == TRUE) {
    return(list(baseline_density, baseline_ppp))
  } else {
    return(baseline_density)
  }
  
}