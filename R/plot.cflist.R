#' Plot simulated counterfactual densities
#'
#' @description A function that takes the simulated counterfactual densities and their priorities
#' and returns a counterfactual density image over a range of parameters
#'
#' @param x input (should be the output of the `sim_power_dens()` function)
#' @param ... arguments passed on to the function
#' @param grayscale logical. `grayscale` specifies whether to convert plot to grayscale (by default, FALSE).
#' @param color the color scale. By default, "white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", and "#771F59FF".
#'
#' @returns ggplot object

plot.cflist <- function(x, ...,
                        color = c("white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", "#771F59FF"),
                        grayscale = FALSE) {

  counterfactual_density_list <- x$cf_density_list
  powers <- x$powers
  expected_number <- x$expected_number

  if(grayscale) {
    color = c("white", "#D4D4D4", "#B4B4B4", "#909090", "#636363") } # Grayscale colors

  # Figure - density
  sf_density_list <- lapply(1:length(counterfactual_density_list),
                            function(i) {
                              sf_density <- as.data.frame(counterfactual_density_list[[i]])
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

  plot_list <- lapply(1:length(sf_density_list),
                      function(a) {

                        suppressWarnings( # Suppress warnings for inherent NAs
                          counterfactual_dens <- ggplot() +
                          tidyterra::geom_spatraster(data = terra::rast(counterfactual_density_list[[a]])) +
                          ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude),
                                                fill = NA, color = "darkgrey", linewidth = 0.2) +
                          ggplot2::scale_fill_gradientn(colors = color, na.value = NA) +
                          ggthemes::theme_map() +
                          ggplot2::ggtitle(latex2exp::TeX(paste0("$\\alpha_{focus} = ", powers[a], "$"))) +
                          labs(fill = "Density") +
                          theme(plot.title = element_text(hjust = 0.5))
                          )

                        return(counterfactual_dens)
                      })

  plot <- ggpubr::ggarrange(plotlist = plot_list, common.legend = TRUE, legend = "bottom")
  titletext <- paste0("Simulated counterfactual densities")
  labtext <- paste0("Note: The expected number of treatment events\nover the entire region per time period = ", expected_number)
  plot <- ggpubr::annotate_figure(plot, fig.lab = labtext, fig.lab.pos = "bottom.right", fig.lab.size = 7)
  plot <- ggpubr::annotate_figure(plot, top = ggpubr::text_grob(titletext, face = "bold"))

  return(plot)

}
