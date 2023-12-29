#' Plot im
#'
#' @param x input
#' @param ... arguments passed on to the function
#' @param main title
#' @param window an owin object
#' @param scalename the name of the scale (for images only)
#' @param lim limits of the scale. By default, NA. To set limits manually, provide a vector or max and min
#' @param grayscale whether to use grayscale. By default, FALSE.
#'
#' @export
plot.im <- function(x, ...,  main = "Image object", window, scalename = "Density", grayscale = "FALSE", lim = NA) {

  ## Convert the density image to a data frame
  pd_df <- as.data.frame(x)

  ## Pivot the data frame to a long format
  pd_df_long <- tidyr::pivot_longer(pd_df, cols = starts_with("V"), names_to = "variable", values_to = "value")

  ## Extract owin
  #window <- spatstat.geom::Window(x)

  ## Plot the image using ggplot2
  plot_dens <- ggplot() +
    ggplot2::geom_tile(data = pd_df_long, aes(x = x, y = y, fill = value)) +
    ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") +
    ggthemes::theme_map() +
    ggplot2::ggtitle(main) +
    labs(fill = scalename) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))

  if (grayscale) {

    if (is.na(lim)[1]) {

      plot_dens <- plot_dens +
        ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys")

    } else {

      plot_dens <- plot_dens +
        ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys", limits = lim)

    }


  } else {

    if (is.na(lim)) {

      plot_dens <- plot_dens +
        ggplot2::scale_fill_viridis_c(option = "plasma")

    } else {

      plot_dens <- plot_dens +
        ggplot2::scale_fill_viridis_c(option = "plasma", limits = lim)

    }

  }

  return(plot_dens)

}
