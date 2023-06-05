#' Function: get_dist_focus
#'
#' A function that generates a distance map from focus locations
#'
#' @param focus_locations A dataframe of coordinates of focus locations
#' @param window An owin object

get_dist_focus <- function(focus_locations,
                           window) {
  
  # Convert to ppp
  focus_locations_ppp <- spatstat.geom::as.ppp(X = focus_locations, W = window)
  
  # Get a distance map
  focus_locations_distance <- spatstat.geom::distmap(focus_locations_ppp, dimyx = 256)
  spatstat.geom::Window(focus_locations_distance) <- window #Add window
  
  # Take the exponential of -distance and divide it by the integral
  neg_exp_distance <- exp(-focus_locations_distance)
  distance_from_focus <- neg_exp_distance/integral(neg_exp_distance)

  return(distance_from_focus)
  
}