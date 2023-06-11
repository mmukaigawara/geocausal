#' Function: simulate_power_density
#'
#' A function that takes the target densities and their priorities
#' and returns a power density image over a range of parameters
#'
#' @param target_densities A list of target densities
#' @param density_to_manipulate One target density for which we manipulate the value of alpha
#' @param priorities A vector of priorities for each of target densities
#' @param priorities_for_manipulation A vector of priority values
#' @param window An owin object
#' @param grayscale By default, FALSE

simulate_power_density <- function(target_densities, #This must be a list element
                                   density_to_manipulate,
                                   priorities,
                                   priorities_for_manipulation,
                                   window,
                                   grayscale = FALSE) {
  
  # Obtaining each target density ^ alpha
  target_alpha <- lapply(1:length(target_densities), function(x) target_densities[[x]] ^ priorities[x])
  
  power_density <- target_alpha[[1]]
  
  if (length(target_densities) > 1){
    
    for (ii in 2:length(target_densities)){ power_density <- power_density * target_alpha[[ii]] }
    
  } 

  power_density_list <- list()
  
  for (jj in 1:length(priorities_for_manipulation)){
    temp <- power_density * density_to_manipulate ^ priorities_for_manipulation[jj]
    temp <- temp/spatstat.geom::integral(temp, domain = window)
    power_density_list[[jj]] <- temp
  }
  
  power_density_list <- lapply(1:length(priorities_for_manipulation),
                               function(x) {
                                 temp <- power_density * density_to_manipulate ^ priorities_for_manipulation[x]
                                 temp <- temp/spatstat.geom::integral(temp, domain = window)
                                 return(temp)
                               }
                               )
  
  # Figure - density
  sf_density_list <- lapply(1:length(power_density_list),
                            function(x) {
                              sf_density <- stars::st_as_stars(power_density_list[[x]])
                              sf_density <- sf::st_as_sf(sf_density) %>% sf::st_set_crs(32650)
                              return(sf_density)
                            }
                            )

  ## Max value to obtain the zlim
  max_val <- max(unlist(lapply(1:length(sf_density_list),
                               function(x) max(sf_density_list[[x]]$v, na.rm = TRUE))))
  
  if (grayscale) {
    
    plot_list <- lapply(1:length(sf_density_list),
                        function(a) {
                          power_dens <- ggplot() +
                            ggplot2::geom_sf(data = sf_density_list[[a]], aes(fill = v), col = NA) +
                            ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") + 
                            ggplot2::geom_path(data = fortify(as.data.frame(window)), aes(x = x, y = y)) + 
                            ggthemes::theme_map() +
                            ggplot2::ggtitle(latex2exp::TeX(paste0("$\\alpha_{focus} = ", priorities_for_manipulation[a], "$"))) +
                            labs(fill = "Density") +
                            theme(plot.title = element_text(hjust = 0.5))
                          
                          return(power_dens)
                        })
    
  } else {
    
    plot_list <- lapply(1:length(sf_density_list),
                        function(a) {
                          power_dens <- ggplot() +
                            ggplot2::geom_sf(data = sf_density_list[[a]], aes(fill = v), col = NA) +
                            ggplot2::scale_fill_viridis_c(option = "plasma", limits = c(NA, max_val)) + 
                            ggplot2::geom_path(data = fortify(as.data.frame(window)), aes(x = x, y = y)) + 
                            ggthemes::theme_map() +
                            ggplot2::ggtitle(latex2exp::TeX(paste0("$\\alpha_{focus} = ", priorities_for_manipulation[a], "$"))) +
                            labs(fill = "Density") +
                            theme(plot.title = element_text(hjust = 0.5))
                          
                          return(power_dens)
                        })
    
  }
  
  plot <- ggpubr::ggarrange(plotlist = plot_list, common.legend = TRUE, legend = "bottom")
  plot <- ggpubr::annotate_figure(plot, top = ggpubr::text_grob("Simulated Power Densities", face = "bold"))
  
  return(list(densities = power_density_list,
              plot = plot,
              priorities = priorities_for_manipulation))

}