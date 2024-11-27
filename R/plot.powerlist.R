#' Plot simulated power densities
#'
#' @description A function that takes the simulated power densities and their priorities
#' and returns a power density image over a range of parameters
#'
#' @param x input (should be the output of the `sim_power_dens()` function)
#' @param ... arguments passed on to the function
#' @param grayscale logical. `grayscale` specifies whether to convert plot to grayscale (by default, FALSE).
#' @param color the color scale. By default, "white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", and "#771F59FF".
#'
#' @returns list of densities, plot, and priorities

plot.powerlist <- function(x, ...,
                           color = c("white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", "#771F59FF"),
                           grayscale = FALSE) {

  power_density_list <- x[[1]] # First element = simulated power densities
  priorities_manip <- x[[2]] # Second element = priorities

  if(grayscale) {
    color = c("white", "#D4D4D4", "#B4B4B4", "#909090", "#636363") } # Grayscale colors

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

  ## Conversion of the window object
  window_sp <- conv_owin_into_sf(spatstat.geom::Window(power_density_list[[1]]))
  polygon_df <- window_sp[[2]] #Convert owin to DF

  plot_list <- lapply(1:length(sf_density_list),
                        function(a) {

                          power_dens <- ggplot() +
                            tidyterra::geom_spatraster(data = terra::rast(power_density_list[[a]])) +
                            ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude),
                                                  fill = NA, color = "darkgrey", linewidth = 0.2) +
                            #ggplot2::geom_tile(data = sf_density_list[[a]], aes(x = x, y = y, fill = value)) +
                            #ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") +
                            ggplot2::scale_fill_gradientn(colors = color, na.value = NA) +
                            #ggplot2::scale_fill_viridis_c(option = "plasma", limits = c(NA, max_val)) +
                            ggthemes::theme_map() +
                            ggplot2::ggtitle(latex2exp::TeX(paste0("$\\alpha_{focus} = ", priorities_manip[a], "$"))) +
                            labs(fill = "Density") +
                            theme(plot.title = element_text(hjust = 0.5))

                          return(power_dens)
                          })

  plot <- ggpubr::ggarrange(plotlist = plot_list, common.legend = TRUE, legend = "bottom")
  plot <- ggpubr::annotate_figure(plot, top = ggpubr::text_grob("Simulated power densities", face = "bold"))

  return(plot)

}
