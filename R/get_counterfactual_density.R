#' Function: get_counterfactual_density
#'
#' A function that takes the target number, baseline density, and power density,
#' and generates a hyperframe with point patterns
#'
#' @param expected_number The expected number of observations.
#' @param baseline_density The baseline density (an im object)
#' @param power_density The power density (an im object)
#' @param window An owin object

get_counterfactual_density <- function(expected_number,
                                       baseline_density,
                                       power_density = NA,
                                       window,
                                       grayscale = FALSE) {
  
  if (is.na(power_density[1])) { # counterfactual_type = intensity only -> multiply by the expectation
    
    counterfactual_density <- expected_number * baseline_density
    
  } else { # counterfactual_type = location as well
    
    product_power_baseline <- baseline_density * power_density
    counterfactual_density <- product_power_baseline/
      integral(product_power_baseline, W = window) * expected_number
    
  }
  
  # Figure
  sf_density <- stars::st_as_stars(counterfactual_density)
  sf_density <- sf::st_as_sf(sf_density) %>% sf::st_set_crs(32650)
  
  if (grayscale) {
    
    counterfactual_dens <- ggplot() +
      ggplot2::geom_sf(data = sf_density, aes(fill = v), col = NA) +
      ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") + 
      ggplot2::geom_path(data = ggplot2::fortify(as.data.frame(window)), aes(x = x, y = y), color = "white") + 
      ggthemes::theme_map() +
      ggplot2::ggtitle("Counterfactual Density",
                       subtitle = paste0("The expected number of treatment events\nover the entire region per time period = ", expected_number)) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
    } else {
    
    counterfactual_dens <- ggplot() +
      ggplot2::geom_sf(data = sf_density, aes(fill = v), col = NA) +
      ggplot2::scale_fill_viridis_c(option = "plasma") + 
      ggplot2::geom_path(data = fortify(as.data.frame(window)), aes(x = x, y = y), color = "white") + 
      ggthemes::theme_map() +
      ggplot2::ggtitle("Counterfactual Density",
                       subtitle = paste0("The expected number of treatment events\nover the entire region per time period = ", expected_number)) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
    }
  
  return(list(density = counterfactual_density, plot = counterfactual_dens))
  
}
