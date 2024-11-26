#' Plot point patterns
#'
#' @param x input
#' @param ... arguments passed on to the function
#' @param main title
#' @param combined logical. `combined` specifies whether to combine all the point processes to one plot.
#' This argument applies only to the case when users specify one column with multiple time periods.
#' By default = TRUE
#'
#' @export
plot.ppplist <- function(x, ..., main = "ppp", combined = TRUE) {

  ppp_temp <- x
  time_vis <- names(ppp_temp)
  num_time_period <- length(ppp_temp)

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
