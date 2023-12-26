#' Plot densities
#' 
#' @param x input
#' @param ... arguments passed on to the function
#' @param main title
#' @param grayscale whether to use grayscale. By default, FALSE.
#' 
#' @export
plot.dens <- function(x, ...,  main = "Density", grayscale = "FALSE") {
  
  ## Convert the density image to a data frame
  pd_df <- as.data.frame(x)
  
  ## Pivot the data frame to a long format
  pd_df_long <- tidyr::pivot_longer(pd_df, cols = starts_with("V"), names_to = "variable", values_to = "value")
  
  ## Extract owin
  window <- spatstat.geom::Window(x)
  
  ## Plot the image using ggplot2
  if(grayscale) {
    
    plot_dens <- ggplot() +
      ggplot2::geom_tile(data = pd_df_long, aes(x = x, y = y, fill = value)) +
      ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") +
      ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") + 
      ggthemes::theme_map() +
      ggplot2::ggtitle(main) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
  } else {
    
    plot_dens <- ggplot() +
      ggplot2::geom_tile(data = pd_df_long, aes(x = x, y = y, fill = value)) +
      ggplot2::scale_fill_viridis_c(option = "plasma") + 
      ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") + 
      ggthemes::theme_map() +
      ggplot2::ggtitle(main) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))
    
  }
  
  return(plot_dens)
  
}