#' Simulate power densities
#'
#' @description A function that takes the target densities and their priorities
#' and returns a power density image over a range of parameters
#'
#' @param target_dens list of target densities. This should always be a list, even if there is only one target density.
#' @param dens_manip a target density for which we manipulate the value of priorities
#' @param priorities numeric. `priorities` specifies the priority for the target density that we do not manipulate.
#' @param priorities_manip vector of priorities for the density that we manipulate.
#' @param window owin object
#' @param grayscale logical. `grayscale` specifies whether to convert plot to grayscale (by default, FALSE).
#' 
#' @returns list of densities, plot, and priorities
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
#' # Simulation
#' sim_power_dens(target_dens = list(dist_from_baghd$distance_im),
#'                dens_manip = dist_from_mosul$distance_im,
#'                priorities = 1,
#'                priorities_manip = c(1, 2, 5, 10, 15, 50),
#'                window = iraq_window,
#'                grayscale = FALSE)

sim_power_dens <- function(target_dens, #This must be a list element
                           dens_manip,
                           priorities,
                           priorities_manip,
                           window,
                           grayscale = FALSE) {
  
  # Obtaining each target density ^ alpha
  target_alpha <- lapply(1:length(target_dens), function(x) target_dens[[x]] ^ priorities[x])
  
  power_density <- target_alpha[[1]]
  
  if (length(target_dens) > 1){
    
    for (ii in 2:length(target_dens)){ power_density <- power_density * target_alpha[[ii]] }
    
  } 

  power_density_list <- list()
  
  for (jj in 1:length(priorities_manip)){
    temp <- power_density * dens_manip ^ priorities_manip[jj]
    temp <- temp/spatstat.geom::integral(temp, domain = window)
    power_density_list[[jj]] <- temp
  }
  
  power_density_list <- lapply(1:length(priorities_manip),
                               function(x) {
                                 temp <- power_density * dens_manip ^ priorities_manip[x]
                                 temp <- temp/spatstat.geom::integral(temp, domain = window)
                                 return(temp)
                               }
                               )
  
  # Figure - density
  sf_density_list <- lapply(1:length(power_density_list),
                            function(x) {
                              sf_density <- as.data.frame(power_density_list[[x]])
                              sf_density <- tidyr::pivot_longer(sf_density, cols = starts_with("V"), 
                                                                names_to = "variable", values_to = "value")
                              return(sf_density)
                            }
                            )

  ## Max value to obtain the zlim
  max_val <- max(unlist(lapply(1:length(sf_density_list),
                               function(x) max(sf_density_list[[x]]$value, na.rm = TRUE))))
  
  if (grayscale) {
    
    plot_list <- lapply(1:length(sf_density_list),
                        function(a) {
                          power_dens <- ggplot() +
                            ggplot2::geom_tile(data = sf_density_list[[a]], aes(x = x, y = y, fill = value)) +
                            ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") + 
                            ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") + 
                            ggthemes::theme_map() +
                            ggplot2::ggtitle(latex2exp::TeX(paste0("$\\alpha_{focus} = ", priorities_manip[a], "$"))) +
                            labs(fill = "Density") +
                            theme(plot.title = element_text(hjust = 0.5))
                          
                          return(power_dens)
                        })
    
  } else {
    
    plot_list <- lapply(1:length(sf_density_list),
                        function(a) {
                          power_dens <- ggplot() +
                            ggplot2::geom_tile(data = sf_density_list[[a]], aes(x = x, y = y, fill = value)) +
                            ggplot2::scale_fill_viridis_c(option = "plasma", limits = c(NA, max_val)) + 
                            ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") + 
                            ggthemes::theme_map() +
                            ggplot2::ggtitle(latex2exp::TeX(paste0("$\\alpha_{focus} = ", priorities_manip[a], "$"))) +
                            labs(fill = "Density") +
                            theme(plot.title = element_text(hjust = 0.5))
                          
                          return(power_dens)
                        })
    
  }
  
  plot <- ggpubr::ggarrange(plotlist = plot_list, common.legend = TRUE, legend = "bottom")
  plot <- ggpubr::annotate_figure(plot, top = ggpubr::text_grob("Simulated Power Densities", face = "bold"))
  
  return(list(densities = power_density_list,
              plot = plot,
              priorities = priorities_manip))

}