#' Get baseline densities
#'
#' @description `get_base_dens()` takes a dataframe and 
#' returns the baseline densities using Scott's rule of thumb.
#'
#' @param data dataframe
#' @param coordinates vector of column names of longitudes and latitudes (in this order)
#' @param window owin object
#' @param grayscale logical. `grayscale` specifies whether to convert plot to grayscale (by default, FALSE).
#' 
#' @returns list of the following:
#'     * `density`: im object of baseline density
#'     * `density_plot`: ggplot object of baseline density
#'     * `point_plot`: ggplot object of raw data
#'
#' @examples
#' get_base_dens(data = airstrikes_base, 
#'               coordinates = c("longitude", "latitude"),
#'               window = iraq_window,
#'               grayscale = FALSE)

get_base_dens <- function(data, 
                          coordinates = c("longitude", "latitude"),
                          window,
                          grayscale = FALSE){
  
  # Convert data to ppp
  coordinates_data <- data[, coordinates]
  baseline_ppp <- spatstat.geom::as.ppp(coordinates_data, W = window)
  
  # Apply Scott's rule of thumb
  scott_bandwidth <- spatstat.explore::bw.scott(baseline_ppp)
  
  baseline_density <- stats::density(baseline_ppp, scott_bandwidth, dimyx = 256) #Kernel density estimation
  baseline_density <- baseline_density / spatstat.geom::integral(baseline_density) #Divide by integral of the density
  
  # ggplot figure - density
  
  ## Convert power density to a data frame
  pd_df <- as.data.frame(baseline_density)
  
  ## Pivot the data frame to a long format
  pd_df_long <- tidyr::pivot_longer(pd_df, cols = starts_with("V"), names_to = "variable", values_to = "value")
  
  ## Plot the image using ggplot2
  if(grayscale) {
    
    baseline_dens <- ggplot() +
      ggplot2::geom_tile(data = pd_df_long, aes(x = x, y = y, fill = value)) +
      ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") +
      ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") + 
      ggthemes::theme_map() +
      ggplot2::ggtitle("Baseline Density",
                       subtitle = paste0("The expected number of treatment events\nover the entire region per time period = ", 1)) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
  } else {
    
    baseline_dens <- ggplot() +
      ggplot2::geom_tile(data = pd_df_long, aes(x = x, y = y, fill = value)) +
      ggplot2::scale_fill_viridis_c(option = "plasma") + 
      ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") + 
      ggthemes::theme_map() +
      ggplot2::ggtitle("Baseline Density",
                       subtitle = paste0("The expected number of treatment events\nover the entire region per time period = ", 1)) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
  }
  
  # ggplot figure - points
  
  points <- as.data.frame(baseline_ppp)
  
  baseline_ppp <- ggplot() +
    ggplot2::geom_point(data = points, aes(x = x, y = y), size = 0.5, col = "black") +
    ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "black") + 
    ggthemes::theme_map() +
    ggplot2::ggtitle("Observed Treatment Events") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  return(list(density = baseline_density, density_plot = baseline_dens, point_plot = baseline_ppp))

}