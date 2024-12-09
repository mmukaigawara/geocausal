#' Plot lists
#'
#' @param x input
#' @param ... arguments passed on to the function
#' @param main title
#' @param frame the element number of the list object (by default, 1)
#' @param combined logical. `combined` specifies whether to combine all the point processes to one plot.
#' @param lim limits of the scale. By default, NA. To set limits manually, provide a vector or max and min
#' @param scalename the name of the scale
#' @param color the color scale. By default, "white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", and "#771F59FF".
#' @param grayscale grayscale or not. By default, FALSE.
#' @param ncol the number of columns (if plotitng multiple images at once)
#' @param nrow the number of rows (if plotting multiple images at once)
#' @param transf a function to transform the pixel values (by default, NULL)
#' This argument applies only to the case when users specify one column with multiple time periods.
#' By default = TRUE
#'
#' @export
plot.list <- function(x, ..., main = "list", lim = NA,
                      transf = NULL, frame = 1, combined = TRUE,
                      scalename = "Density", color = c("white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", "#771F59FF"),
                      grayscale = FALSE, ncol = NA, nrow = NA) {


  if(inherits(x[[1]]) == "im") {

    num_time_period <- length(frame)

    if (num_time_period == 1) {

      # Convert window to df -----
      im_temp <- x[[frame]]
      window <- spatstat.geom::Window(im_temp)
      window_sp <- conv_owin_into_sf(window)
      polygon_df <- window_sp[[2]]

      if (!is.null(transf)) {
        if (is.function(transf)) {
          im_temp <- transf(im_temp)
        } else {
          stop("The 'transf' parameter must be a valid function.")
        }
      }

    } else {

      # Convert window to df -----
      im_temp <- x[frame]
      window <- spatstat.geom::Window(im_temp[[1]])
      window_sp <- conv_owin_into_sf(window)
      polygon_df <- window_sp[[2]]

      if (!is.null(transf)) {
        if (is.function(transf)) {
          for (i in 1:length(im_temp)) { im_temp[[i]] <- transf(im_temp[[i]]) }
        } else {
          stop("The 'transf' parameter must be a valid function.")
        }
      }

    }

    time_vis <- frame

    if(grayscale) {
      color = c("white", "#D4D4D4", "#B4B4B4", "#909090", "#636363") } # Grayscale colors

    # Visualization -----

    if (num_time_period == 1) {

      # Case 1: One time period x One outcome column -----

      gg <- ggplot2::ggplot() + #Plot smoothed outcome
        tidyterra::geom_spatraster(data = terra::rast(im_temp)) +
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
        ggpubr::ggarrange(gg, ncol = 1, heights = c(10, 1)),
        top = ggpubr::text_grob(main, face = "bold", hjust = 0.5))

    }

    return(gg)

  } else {


    ppp_temp <- x[frame]
    time_vis <- frame
    num_time_period <- length(frame)

    # Convert window to df -----
    window <- spatstat.geom::Window(ppp_temp[1][[1]])
    window_sp <- conv_owin_into_sf(window)
    polygon_df <- window_sp[[2]]

    # Visualization -----

    if (num_time_period == 1) {

      # Case 1: One time period x One outcome column -----

      dat <- ppp_temp[[1]] #Convert points to df
      df <- data.frame(longitude = dat$x, latitude = dat$y)

      gg <- ggplot2::ggplot(data = df, aes(x = longitude, y = latitude)) +
        ggplot2::geom_point(shape = 1, size = 1.5) +
        ggplot2::coord_quickmap() +
        ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "black") +
        ggthemes::theme_map() +
        ggplot2::ggtitle(paste0(main, "\n(Time Period ", time_vis, ")")) +
        ggplot2::theme(plot.title = element_text(hjust = 0.5, face = "bold"))

    } else if (num_time_period > 1) {

      # Case 2: Multiple time periods x One outcome column -----

      ## If combined (everything in one plot)
      if (combined) {

        df_list <- lapply(time_vis, function(x) {
          dat <- ppp_temp[which(time_vis == x)][[1]] #Convert points to df
          df <- data.frame(longitude = dat$x, latitude = dat$y)
        })

        df <- do.call(rbind, df_list)

        gg <- ggplot2::ggplot(data = df, aes(x = longitude, y = latitude)) +
          ggplot2::geom_point(shape = 1, size = 1.5) +
          ggplot2::coord_quickmap() +
          ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "black") +
          ggthemes::theme_map() +
          ggplot2::ggtitle(paste0(main, "\n(Time Periods ", time_vis[1], " - ", time_vis[length(time_vis)], ")")) +
          ggplot2::theme(plot.title = element_text(hjust = 0.5, face = "bold"))

      } else {

        ## If not combined

        df_list <- lapply(time_vis, function(i) {
          dat <- ppp_temp[which(time_vis == i)][[1]] #Convert points to df
          df <- data.frame(longitude = dat$x, latitude = dat$y, time_period = rep(i, length(dat$x)))

          if (length(dat$x) != 0) {
            df <- df
          } else {
            df <- data.frame(longitude = NA, latitude = NA, time_period = i)

          }

        })

        df <- do.call(rbind, df_list)

        gg <- ggplot2::ggplot(data = df, aes(x = longitude, y = latitude)) +
          ggplot2::geom_point(shape = 1, size = 1.5) +
          ggplot2::coord_quickmap() +
          ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "black") +
          ggthemes::theme_map() +
          ggplot2::ggtitle(paste0(main, "\n(Time Periods ", time_vis[1], " - ", time_vis[length(time_vis)], ")")) +
          ggplot2::theme(strip.text = element_text(hjust = 0.5, face = "bold"),
                         strip.background = element_rect(fill = "white", color = "white"),
                         plot.title = element_text(hjust = 0.5, face = "bold")) +
          ggplot2::facet_wrap(vars(time_period))

      }

    }

    return(gg)

  }

}
