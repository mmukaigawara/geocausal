#' Function: get_power_density
#'
#' @description A function that takes the target densities and their priorities
#' and returns a power density image
#'
#' @param target_densities A list of target densities
#' @param priorities A vector of priorities for each of target densities
#' @param window An owin object
#' @param grayscale Whether to use grayscale
#' 
#' @returns A list of an im object and a ggplot object of power densities


get_power_density <- function(target_densities,
                              priorities,
                              window,
                              grayscale = FALSE) {
  
  # Obtaining each target density ^ alpha
  target_alpha <- lapply(1:length(target_densities), function(x) target_densities[[x]] ^ priorities[x])

  power_density <- target_alpha[[1]]
  
  if (length(target_densities) > 1){
    
    for (ii in 2:length(target_densities)){ suppressWarnings( #Suppress warnings
      power_density <- power_density * target_alpha[[ii]]
      ) }
  } 

  power_density <- power_density/spatstat.geom::integral(power_density, domain = window)
  
  # ggplot figure
  
  ## Convert power density to a data frame
  pd_df <- as.data.frame(power_density)

  ## Pivot the data frame to a long format
  pd_df_long <- tidyr::pivot_longer(pd_df, cols = starts_with("V"), names_to = "variable", values_to = "value")

  ## Plot the image using ggplot2
  if(grayscale) {
    
    power_dens <- ggplot() +
      ggplot2::geom_tile(data = pd_df_long, aes(x = x, y = y, fill = value)) +
      ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") +
      ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") + 
      ggthemes::theme_map() +
      ggplot2::ggtitle(paste0("Power Density")) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
  } else {
    
    power_dens <- ggplot() +
      ggplot2::geom_tile(data = pd_df_long, aes(x = x, y = y, fill = value)) +
      ggplot2::scale_fill_viridis_c(option = "plasma") + 
      ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") + 
      ggthemes::theme_map() +
      ggplot2::ggtitle(paste0("Power Density")) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
  }

  return(list(density = power_density, plot = power_dens))
  
}