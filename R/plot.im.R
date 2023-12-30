#' Plot im
#'
#' @param x input
#' @param ... arguments passed on to the function
#' @param main title
#' @param scalename the name of the scale (for images only)
#' @param lim limits of the scale. By default, NA. To set limits manually, provide a vector or max and min
#' @param grayscale whether to use grayscale. By default, FALSE.
#' @param color the color scale. By default, "white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", and "#771F59FF".
#'
#' @export
plot.im <- function(x, ...,  main = "Image object",
                    scalename = "Density", grayscale = "FALSE",
                    color = c("white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", "#771F59FF"), lim = NA) {

  ## Extract owin
  window_sp <- conv_owin_into_sf(spatstat.geom::Window(x))
  polygon_df <- window_sp[[2]] #Convert owin to DF

  ## Plot the image using ggplot2
  plot_dens <- ggplot() +
    tidyterra::geom_spatraster(data = terra::rast(x)) +
    ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude),
                          fill = NA, color = "darkgrey", linewidth = 0.2) +
    ggthemes::theme_map() +
    ggplot2::ggtitle(main) +
    labs(fill = scalename) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "bottom")

  if (grayscale) {

    if (is.na(lim)[1]) {

      plot_dens <- plot_dens +
        ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys")

    } else {

      plot_dens <- plot_dens +
        ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys", limits = lim)

    }

  } else {

    if (is.na(lim)[1]) {

      plot_dens <- plot_dens +
        ggplot2::scale_fill_gradientn(colors = color, na.value = NA, name = scalename)

    } else {

      plot_dens <- plot_dens +
        ggplot2::scale_fill_gradientn(colors = color, na.value = NA, limits = lim, name = scalename)

    }

  }

  return(plot_dens)

}
