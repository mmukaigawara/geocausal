#' Plot im objects (list)
#'
#' @param x input
#' @param ... arguments passed on to the function
#' @param main title
#' @param lim limits of the scale. By default, NA. To set limits manually, provide a vector or max and min
#' @param scalename the name of the scale
#' @param color the color scale. By default, "white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", and "#771F59FF".
#' @param grayscale grayscale or not. By default, FALSE.
#' @param ncol the number of columns (if plotitng multiple images at once)
#' @param nrow the number of rows (if plotting multiple images at once)
#'
#' @export
plot.imlist <- function(x, ..., main = "image", lim = NA,
                        scalename = "Density", color = c("white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", "#771F59FF"),
                        grayscale = FALSE, ncol = NA, nrow = NA) {

  im_temp <- x
  time_vis <- names(im_temp)
  num_time_period <- length(im_temp)

  if(grayscale) {
    color = c("white", "#D4D4D4", "#B4B4B4", "#909090", "#636363") } # Grayscale colors

  # Convert window to df -----
  window <- spatstat.geom::Window(im_temp[1][[1]])
  window_sp <- conv_owin_into_sf(window)
  polygon_df <- window_sp[[2]]

  # Visualization -----

  if (num_time_period == 1) {

    # Case 1: One time period x One outcome column -----

    gg <- ggplot2::ggplot() + #Plot smoothed outcome
      tidyterra::geom_spatraster(data = terra::rast(im_temp[[1]])) +
      ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude),
                            fill = NA, color = "darkgrey", linewidth = 0.2) +
      ggthemes::theme_map() +
      ggplot2::ggtitle(paste0(main, "\n(Time Period ", time_vis, ")")) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                     legend.position = "bottom")

    if (is.na(lim)[1]) {
      gg <- gg + ggplot2::scale_fill_gradientn(colors = color, na.value = NA, name = scalename)
    } else {
      gg <- gg + ggplot2::scale_fill_gradientn(colors = color, na.value = NA, name = scalename, limits = lim)
    }

  } else if (num_time_period > 1) {

    # Case 2: Multiple time periods x One outcome column -----

    if (is.na(lim)[1]) { # Get the max and min of v of image objects
      lim <- c(min(sapply(x, function(elem) range(elem$v, na.rm = TRUE))),
               max(sapply(x, function(elem) range(elem$v, na.rm = TRUE))))
    }


    ggplots <- lapply(seq_along(im_temp), function(i) {

      ggplot2::ggplot() + #Plot smoothed outcome
        tidyterra::geom_spatraster(data = terra::rast(im_temp[[i]])) +
        ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude),
                              fill = NA, color = "darkgrey", linewidth = 0.2) +
        ggthemes::theme_map() +
        ggplot2::ggtitle(paste0("Time Period ", i, "")) +
        ggplot2::theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                       legend.position = "bottom") +
        ggplot2::scale_fill_gradientn(colors = color, na.value = NA, name = scalename, limits = lim)

    })

    if (is.na(nrow) | is.na(ncol)) { # If not specified
      ncol = 2
      nrow = ceiling(length(ggplots) / 2)
    }

    gg <- ggpubr::ggarrange(plotlist = ggplots, ncol = ncol, nrow = nrow,
                            common.legend = TRUE, legend = "bottom")

    gg <- ggpubr::annotate_figure(
      ggpubr::ggarrange(gg, legend, ncol = 1, heights = c(10, 1)),
      top = ggpubr::text_grob(main, face = "bold", hjust = 0.5))

  }

  return(gg)

}
