#' Function: get_counterfactual_density
#'
#' @description A function that takes the target number, baseline density, and power density,
#' and generates a hyperframe with point patterns
#'
#' @param expected_number The expected number of observations.
#' @param baseline_density The baseline density (an im object)
#' @param power_density The power density (an im object)
#' @param window An owin object
#' @param grayscale Whether to grayscale the plot or not
#' 
#' @returns A list of density (an im object) and a ggplot object

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
  
  # ggplot figure
  
  ## Convert power density to a data frame
  cd_df <- as.data.frame(counterfactual_density)
  
  ## Pivot the data frame to a long format
  cd_df_long <- tidyr::pivot_longer(cd_df, cols = starts_with("V"), names_to = "variable", values_to = "value")
  
  if (grayscale) {
    
    counterfactual_dens <- ggplot() +
      ggplot2::geom_tile(data = cd_df_long, aes(x = x, y = y, fill = value)) +
      ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") +
      ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") + 
      ggthemes::theme_map() +
      ggplot2::ggtitle("Counterfactual Density",
                       subtitle = paste0("The expected number of treatment events\nover the entire region per time period = ", expected_number)) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
    } else {
    
    counterfactual_dens <- ggplot() +
      ggplot2::geom_tile(data = cd_df_long, aes(x = x, y = y, fill = value)) +
      ggplot2::scale_fill_viridis_c(option = "plasma") +
      ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") + 
      ggthemes::theme_map() +
      ggplot2::ggtitle("Counterfactual Density",
                       subtitle = paste0("The expected number of treatment events\nover the entire region per time period = ", expected_number)) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
    }
  
  return(list(density = counterfactual_density, plot = counterfactual_dens))
  
}
