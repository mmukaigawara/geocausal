#' Simulate counterfactual densities
#'
#' @description
#' `sim_cf_dens()` takes a list of power densities
#' and returns simulated counterfactual densities.
#'
#' @param expected_number the expected number of observations
#' @param base_dens the baseline density (im object)
#' @param power_sim_results the results obtained by `simulate_power_density()`
#' @param window owin object
#' @param grayscale logical. `grayscale` specifies whether to convert plot to grayscale (by default, FALSE).
#' @param color the color scale. By default, "white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", and "#771F59FF".
#'
#' @returns list of counterfactual densities, a ggplot, and priorities

sim_cf_dens <- function(expected_number,
                        base_dens,
                        power_sim_results,
                        window,
                        grayscale = FALSE,
                        color = c("white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", "#771F59FF")) {

  power_densities <- power_sim_results$densities
  powers <- power_sim_results$priorities

  # Obtaining a list of counterfactual densities based on simluations
  counterfactual_density_list <- lapply(1:length(power_densities),
                                        function(x) { #Suppress warnings for incompatibility of images
                                          suppressWarnings( product_power_baseline <- base_dens * power_densities[[x]] )
                                          counterfactual_density <- product_power_baseline/
                                            spatstat.univar::integral(product_power_baseline, W = window) * expected_number
                                          return(counterfactual_density)
                                          }
                                        )

  # Figure - density
  sf_density_list <- lapply(1:length(counterfactual_density_list),
                            function(x) {
                              sf_density <- as.data.frame(counterfactual_density_list[[x]])
                              sf_density <- tidyr::pivot_longer(sf_density, cols = starts_with("V"),
                                                                names_to = "variable", values_to = "value")
                              return(sf_density)
                            }
                            )

  ## Max value to obtain the zlim
  max_val <- max(unlist(lapply(1:length(sf_density_list),
                               function(x) max(sf_density_list[[x]]$value, na.rm = TRUE))))

  ## Conversion of the window object
  window_sp <- conv_owin_into_sf(spatstat.geom::Window(counterfactual_density_list[[1]]))
  polygon_df <- window_sp[[2]] #Convert owin to DF

  if (grayscale) {

    plot_list <- lapply(1:length(sf_density_list),
                        function(a) {

                          counterfactual_dens <- ggplot() +
                            tidyterra::geom_spatraster(data = terra::rast(counterfactual_density_list[[a]])) +
                            ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude),
                                                  fill = NA, color = "darkgrey", linewidth = 0.2) +
                            #ggplot2::geom_tile(data = sf_density_list[[a]], aes(x = x, y = y, fill = value)) +
                            #ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") +
                            ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") +
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
                            tidyterra::geom_spatraster(data = terra::rast(counterfactual_density_list[[a]])) +
                            ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude),
                                                  fill = NA, color = "darkgrey", linewidth = 0.2) +
                            #ggplot2::geom_tile(data = sf_density_list[[a]], aes(x = x, y = y, fill = value)) +
                            #ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") +
                            ggplot2::scale_fill_gradientn(colors = color, na.value = NA) +
                            #ggplot2::scale_fill_viridis_c(option = "plasma", limits = c(NA, max_val)) +
                            ggthemes::theme_map() +
                            ggplot2::ggtitle(latex2exp::TeX(paste0("$\\alpha_{focus} = ", powers[a], "$"))) +
                            labs(fill = "Density") +
                            theme(plot.title = element_text(hjust = 0.5))

                          return(counterfactual_dens)
                        })

  }

  plot <- ggpubr::ggarrange(plotlist = plot_list, common.legend = TRUE, legend = "bottom")
  titletext <- paste0("Simulated counterfactual densities")
  labtext <- paste0("Note: The expected number of treatment events\nover the entire region per time period = ", expected_number)
  plot <- ggpubr::annotate_figure(plot, fig.lab = labtext, fig.lab.pos = "bottom.right", fig.lab.size = 7)
  plot <- ggpubr::annotate_figure(plot, top = ggpubr::text_grob(titletext, face = "bold"))

  return(list(densities = counterfactual_density_list,
              plot = plot,
              priorities = power_sim_results$priorities))

}
