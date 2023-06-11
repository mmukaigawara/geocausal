#' Function: simulate_counterfactual_density
#'
#' A function that takes a list of power densities
#' and returns simulated counterfactual densities
#'
#' @param expected_number The expected number of observations
#' @param baseline_density The baseline density (an im object)
#' @param power_simulation_results The results obtained by simulate_power_density function
#' @param window An owin object
#' @param gray_scale By default, FALSE

simulate_counterfactual_density <- function(expected_number,
                                            baseline_density,
                                            power_simulation_results,
                                            window,
                                            grayscale = FALSE) {
  
  power_densities <- power_simulation_results$densities
  powers <- power_simulation_results$priorities
  
  # Obtaining a list of counterfactual densities based on simluations
  counterfactual_density_list <- lapply(1:length(power_densities),
                                        function(x) {
                                          product_power_baseline <- baseline_density * power_densities[[x]]
                                          counterfactual_density <- product_power_baseline/
                                            integral(product_power_baseline, W = window) * expected_number
                                          return(counterfactual_density)
                                          }
                                        )
  
  # Figure - density
  sf_density_list <- lapply(1:length(counterfactual_density_list),
                            function(x) {
                              sf_density <- stars::st_as_stars(counterfactual_density_list[[x]])
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
                          counterfactual_dens <- ggplot() +
                            ggplot2::geom_sf(data = sf_density_list[[a]], aes(fill = v), col = NA) +
                            ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") + 
                            ggplot2::geom_path(data = fortify(as.data.frame(window)), aes(x = x, y = y)) + 
                            ggthemes::theme_map() +
                            ggplot2::ggtitle(latex2exp::TeX(paste0("$\\alpha_{focus} = ", powers[a], "$"))) +
                            labs(fill = "Density") +
                            theme(plot.title = element_text(hjust = 0.5))
                          
                          return(counterfactual_dens)
                        })
    
  } else {

    plot_list <- lapply(1:length(sf_density_list),
                        function(a) {
                          counterfactual_dens <- ggplot() +
                            ggplot2::geom_sf(data = sf_density_list[[a]], aes(fill = v), col = NA) +
                            ggplot2::scale_fill_viridis_c(option = "plasma", limits = c(NA, max_val)) + 
                            ggplot2::geom_path(data = fortify(as.data.frame(window)), aes(x = x, y = y)) + 
                            ggthemes::theme_map() +
                            ggplot2::ggtitle(latex2exp::TeX(paste0("$\\alpha_{focus} = ", powers[a], "$"))) +
                            labs(fill = "Density") +
                            theme(plot.title = element_text(hjust = 0.5))
                          
                          return(counterfactual_dens)
                        })

  }
  
  plot <- ggpubr::ggarrange(plotlist = plot_list, common.legend = TRUE, legend = "bottom")
  titletext <- paste0("Simulated Counterfactual Densities")
  labtext <- paste0("Note: The expected number of treatment events\nover the entire region per time period = ", expected_number)
  plot <- ggpubr::annotate_figure(plot, fig.lab = labtext, fig.lab.pos = "bottom.right", fig.lab.size = 7)
  plot <- ggpubr::annotate_figure(plot, top = ggpubr::text_grob(titletext, face = "bold"))
  
  return(list(densities = counterfactual_density_list,
              plot = plot,
              priorities = power_simulation_results$priorities))
  
}
