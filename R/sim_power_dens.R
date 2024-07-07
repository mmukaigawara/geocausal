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
#' @param color the color scale. By default, "white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", and "#771F59FF".
#'
#' @returns list of densities, plot, and priorities

sim_power_dens <- function(target_dens, #This must be a list element
                           dens_manip,
                           priorities,
                           priorities_manip,
                           window,
                           color = c("white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", "#771F59FF"),
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
    temp <- temp/spatstat.univar::integral(temp, domain = window)
    power_density_list[[jj]] <- temp
  }

  power_density_list <- lapply(1:length(priorities_manip),
                               function(x) {
                                 temp <- power_density * dens_manip ^ priorities_manip[x]
                                 temp <- temp/spatstat.univar::integral(temp, domain = window)
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

  ## Conversion of the window object
  window_sp <- conv_owin_into_sf(spatstat.geom::Window(power_density_list[[1]]))
  polygon_df <- window_sp[[2]] #Convert owin to DF

  if (grayscale) {

    plot_list <- lapply(1:length(sf_density_list),
                        function(a) {

                          power_dens <- ggplot() +
                            tidyterra::geom_spatraster(data = terra::rast(power_density_list[[a]])) +
                            ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude),
                                                  fill = NA, color = "darkgrey", linewidth = 0.2) +
                            #ggplot2::geom_tile(data = sf_density_list[[a]], aes(x = x, y = y, fill = value)) +
                            #ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") +
                            ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") +
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

  }

  plot <- ggpubr::ggarrange(plotlist = plot_list, common.legend = TRUE, legend = "bottom")
  plot <- ggpubr::annotate_figure(plot, top = ggpubr::text_grob("Simulated power densities", face = "bold"))

  return(list(densities = power_density_list,
              plot = plot,
              priorities = priorities_manip))

}
