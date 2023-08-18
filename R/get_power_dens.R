#' Get power densities
#'
#' @description 
#' `get_power_dens()` takes the target densities and their priorities
#' and returns a power density.
#'
#' @param target_dens list of target densities
#' @param priorities vector of priorities for each of target densities
#' @param window owin object
#' @param grayscale logical. `grayscale` specifies whether to convert plot to grayscale (by default, FALSE).
#' 
#' @returns list of an im object and a ggplot object of power densities
#' 
#' @examples
#' # Density 1: Distance from Mosul
#' dist_from_mosul <- get_dist_focus(window = iraq_window,
#'                                   longitude = c(43.158),
#'                                   latitude = c(36.349),
#'                                   resolution = 0.5,
#'                                   grayscale = FALSE,
#'                                   mile = FALSE,
#'                                   preprocess = FALSE)
#' 
#' # Density 2: Distance from Baghdad
#' dist_from_baghd <- get_dist_focus(window = iraq_window,
#'                                   longitude = c(44.366),
#'                                   latitude = c(33.315),
#'                                   resolution = 0.5,
#'                                   grayscale = FALSE,
#'                                   mile = FALSE,
#'                                   preprocess = FALSE)
#'                                   
#' # Power density
#' get_power_dens(target_dens = list(dist_from_mosul[[1]], dist_from_baghd[[1]]),
#'                priorities = c(3, 2), 
#'                window = iraq_window,
#'                grayscale = FALSE)


get_power_dens <- function(target_dens,
                           priorities,
                           window,
                           grayscale = FALSE) {
  
  # Obtaining each target density ^ alpha
  target_alpha <- lapply(1:length(target_dens), function(x) target_dens[[x]] ^ priorities[x])

  power_density <- target_alpha[[1]]
  
  if (length(target_dens) > 1){
    
    for (ii in 2:length(target_dens)){ suppressWarnings( #Suppress warnings
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