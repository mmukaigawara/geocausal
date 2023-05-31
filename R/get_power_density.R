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
  
  power_density <- power_density/integral(power_density, domain = window)
  
  # Figure - density
  sf_density <- stars::st_as_stars(power_density)
  sf_density <- sf::st_as_sf(sf_density) %>% sf::st_set_crs(32650)
  
  power_dens <- ggplot() +
    ggplot2::geom_sf(data = sf_density, aes(fill = v), col = NA) +
    ggplot2::scale_fill_viridis_c(option = "plasma") + 
    ggplot2::geom_path(data = fortify(as.data.frame(window)), aes(x = x, y = y)) + 
    ggthemes::theme_map() +
    ggplot2::ggtitle(paste0("Power density")) +
    labs(fill = "Density") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  return(list(density = power_density, density_plot = power_dens))
  
}