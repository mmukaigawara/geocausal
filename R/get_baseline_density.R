#' Function: get_baseline_density
#'
#' A function that takes a dataframe and returns the baseline densities
#' using Scott's rule of thumb
#'
#' @param data A dataframe
#' @param coordinates A vector of column names for longitudes and latitudes (in this order)
#' @param indep_var A vector of names of independent variables
#' @param window The window object of interest

get_baseline_density <- function(data, 
                                 coordinates = c("longitude", "latitude"),
                                 window,
                                 grayscale = FALSE){
  
  # Convert data to ppp
  coordinates_data <- airstr_base[, coordinates]
  baseline_ppp <- spatstat.geom::as.ppp(coordinates_data, W = window)
  
  # Apply Scott's rule of thumb
  scott_bandwidth <- spatstat.explore::bw.scott(baseline_ppp)
  
  baseline_density <- stats::density(baseline_ppp, scott_bandwidth, dimyx = 256) #Kernel density estimation
  baseline_density <- baseline_density / integral(baseline_density) #Divide by integral of the density

  # Figure - density
  sf_density <- stars::st_as_stars(baseline_density)
  sf_density <- sf::st_as_sf(sf_density) %>% sf::st_set_crs(32650)
  
  if(grayscale) {
    baseline_dens <- ggplot() +
      ggplot2::geom_sf(data = sf_density, aes(fill = v), col = NA) +
      ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") + 
      ggplot2::geom_path(data = fortify(as.data.frame(window)), aes(x = x, y = y)) + 
      ggthemes::theme_map() +
      ggplot2::ggtitle(paste0("Baseline Density\n(The expected number of treatment per time period = ", 1, ")" )) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  } else {
    baseline_dens <- ggplot() +
      ggplot2::geom_sf(data = sf_density, aes(fill = v), col = NA) +
      ggplot2::scale_fill_viridis_c(option = "plasma") + 
      ggplot2::geom_path(data = fortify(as.data.frame(window)), aes(x = x, y = y)) + 
      ggthemes::theme_map() +
      ggplot2::ggtitle(paste0("Baseline Density\n(The expected number of treatment per time period = ", 1, ")" )) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  }

  # Figure - ppp
  sf_points <- data.frame(lat = baseline_ppp$y,
                          lon = baseline_ppp$x) %>%
    sf::st_as_sf(coords = c("lon", "lat")) %>%
    sf::st_set_crs(32650)
  
  baseline_ppp <- ggplot() +
    ggplot2::geom_sf(data = sf_points, size = 0.5, col = "black") +
    ggplot2::geom_path(data = fortify(as.data.frame(window)), aes(x = x, y = y)) + 
    ggthemes::theme_map() +
    ggplot2::ggtitle("Observed treatment locations") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  return(list(density = baseline_density, density_plot = baseline_dens, point_plot = baseline_ppp))

}