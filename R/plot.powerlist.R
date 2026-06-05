#' Plot simulated power densities
#'
#' @description Plot method for objects of class `powerlist`, returned by
#' [sim_power_dens()]. Takes the simulated power densities and their priorities
#' and returns a faceted power density image over the range of parameters.
#'
#' @param x an object of class `powerlist`, typically the output of [sim_power_dens()].
#' @param ... additional arguments. Currently ignored.
#' @param grayscale logical. `grayscale` specifies whether to convert plot to grayscale (by default, FALSE).
#' @param color the color scale. By default, "white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", and "#771F59FF".
#'
#' @returns A `ggplot` object (a faceted arrangement of power density images).
#'
#' @seealso [sim_power_dens()]
#'
#' @export

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
                            ggplot2::scale_fill_gradientn(colors = color, na.value = NA) +
                            ggthemes::theme_map() +
                            ggplot2::ggtitle(bquote(alpha[focus] == .(powers[a]))) +
                            labs(fill = "Density") +
                            theme(plot.title = element_text(hjust = 0.5))

                          return(power_dens)
                          })

  plot <- ggpubr::ggarrange(plotlist = plot_list, common.legend = TRUE, legend = "bottom")
  plot <- ggpubr::annotate_figure(plot, top = ggpubr::text_grob("Simulated power densities", face = "bold"))

  return(plot)

}
