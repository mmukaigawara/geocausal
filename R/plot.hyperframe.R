#' Plot estimates
#'
#' @param x input
#' @param ... arguments passed on to the function
#' @param col the name/s of a column of interest.
#' @param main title
#' To specify multiple columns, users should list column names as a character vector.
#' @param time_col The name of the column of time variable. By default, `"time"`. Note that the time variable must be integers.
#' @param range vector that specifies the range of tiem variable (e.g., `c("2007-01-01", "2007-01-31")`)
#' @param lim limits of the scale. By default, NA. To set limits manually, provide a vector or max and min
#' @param combined logical. `combined` specifies whether to combine all the point processes to one plot.
#' This argument applies only to the case when users specify one column with multiple time periods.
#' By default = TRUE
#' @param scalename the name of the scale (for images only)
#' @param color the color scale. By default, "white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", and "#771F59FF".
#'
#' @export
plot.hyperframe <- function(x, ..., col, time_col = "time", range, lim = NA, main = "Image object",
                            scalename = NA, color = c("white", "#F8DAC5FF", "#F4825AFF", "#D2204CFF", "#771F59FF"),
                            combined = TRUE) {

  # Clean the hyperframe -----
  hfr_temp <- x

  if (length(range) == 1) { all_rows <- range } else
  { all_rows <- seq(min(range), max(range), by = 1)} #Sequence of all rows

  time_id <- which(names(hfr_temp) == time_col)
  names(hfr_temp)[time_id] <- "time" #Rename the time column

  row_id <- which(hfr_temp$time %in% all_rows) #Obtain the time period row IDs
  outcome_id <- which(names(hfr_temp) %in% col) #Obtain the outcome column IDs

  hfr_cleaned <- hfr_temp[row_id, c(time_id, outcome_id)] #Return necessary portions of hfr

  # Visualization (preparation) -----
  num_time_period <- nrow(hfr_cleaned)
  num_outcome_columns <- ncol(hfr_cleaned) - 1 #Subtract 1 d/t time column

  # Convert window to df
  window <- spatstat.geom::Window(hfr_cleaned[1, which(colnames(hfr_cleaned) == col[1])][[1]])
  window_sp <- conv_owin_into_sf(window)
  polygon_df <- window_sp[[2]]

  # Function for grayscaling (for density plot only)
  convert_to_grayscale <- function(ggplot_object, grayscale) {

    if (grayscale) {
      ggplot_obj <- ggplot_object +
        ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") +
        ggplot2::ggtitle(main)
    } else {
      ggplot_obj <- ggplot_object +
        ggplot2::scale_fill_gradientn(colors = color) +
        ggplot2::ggtitle(main)
    }

    return(ggplot_obj)

  }

  # Visualization (four patterns wrt time periods and outcome columns) -----

  if (num_time_period == 1 && num_outcome_columns == 1) {

    # Case 1: One time period x One outcome column -----

    outcome_name <- colnames(hfr_cleaned)[-1] #Names of outcomes
    time_vis <- hfr_cleaned$time[1]$time #Time period to visualize

    if (!inherits(hfr_cleaned[1, 2][[1]], "im")) { # If ppp

      dat <- hfr_cleaned[1, 2][[1]] #Convert points to df
      df <- data.frame(longitude = dat$x, latitude = dat$y)

      gg <- ggplot2::ggplot(data = df, aes(x = longitude, y = latitude)) +
        ggplot2::geom_point(shape = 1, size = 1.5) +
        ggplot2::coord_quickmap() +
        ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "black") +
        ggthemes::theme_map() +
        ggplot2::ggtitle(paste0(main, "\n(Time Period ", time_vis, ")")) +
        ggplot2::theme(plot.title = element_text(hjust = 0.5, face = "bold"))

    } else { # If im

      window_sp <- conv_owin_into_sf(spatstat.geom::Window(hfr_cleaned[1, 2][[1]]))
      polygon_df <- window_sp[[2]] #Convert owin to DF

      gg <- ggplot2::ggplot() + #Plot smoothed outcome
        tidyterra::geom_spatraster(data = terra::rast(hfr_cleaned[1, 2][[1]])) +
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

    }

  } else if (num_time_period == 1 && num_outcome_columns > 1) {

    # Case 2: One time period x Multiple outcome columns: PPP only

    outcome_name <- colnames(hfr_cleaned)[-1] #Names of outcomes
    time_vis <- hfr_cleaned$time[1]$time #Time period to visualize

    df_list <- lapply(outcome_name, function(i) {
      dat <- hfr_cleaned[1, which(outcome_name == i) + 1][[1]] #Convert points to df
      df <- data.frame(longitude = dat$x, latitude = dat$y, subtype = i)

      if (length(dat$x) != 0) {
        df <- df
      } else {
        df <- data.frame(longitude = NA, latitude = NA, subtype = i)
      }

    })

    df <- do.call(rbind, df_list)

    gg <- ggplot2::ggplot(data = df, aes(x = longitude, y = latitude)) +
      ggplot2::geom_point(shape = 1, size = 1.5) +
      ggplot2::coord_quickmap() +
      ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "black") +
      ggthemes::theme_map() +
      ggplot2::ggtitle(paste0(paste0(main, collapse = ", "), "\n(Time period ", time_vis, ")")) +
      ggplot2::theme(strip.text = element_text(hjust = 0.5, face = "bold"),
                     strip.background = element_rect(fill = "white", color = "white"),
                     plot.title = element_text(hjust = 0.5, face = "bold")) +
      ggplot2::facet_wrap(vars(subtype))

    ## If Image -> TBD

  } else if (num_time_period > 1 && num_outcome_columns == 1) {

    # Case 3: Multiple time periods x One outcome column: PPP only

    ## If ppp
    outcome_name <- colnames(hfr_cleaned)[-1] #Names of outcomes
    time_vis <- hfr_cleaned$time #Time period to visualize

    ### If combined (everything in one plot)
    if (combined) {

      df_list <- lapply(time_vis, function(x) {
        dat <- hfr_cleaned[which(time_vis == x), 2][[1]] #Convert points to df
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

      ### If not combined

      df_list <- lapply(time_vis, function(i) {
        dat <- hfr_cleaned[which(time_vis == i), 2][[1]] #Convert points to df
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

  } else if (num_time_period > 1 && num_outcome_columns > 1) {

    # Case 4: Mutliple time periods x Multiple outcome columns: PPP only, automatically combine

    outcome_name <- colnames(hfr_cleaned)[-1] #Names of outcomes
    time_vis <- hfr_cleaned$time #Time period to visualize

    i <- outcome_name[1]

    df_list <- lapply(outcome_name, function(i) {
      dat <- hfr_cleaned[, which(outcome_name == i) + 1][[1]] #Convert points to df

      df_list_internal <- lapply(time_vis, function(j) {
        df_internal <- data.frame(longitude = dat[[which(time_vis == j)]]$x,
                                  latitude = dat[[which(time_vis == j)]]$y,
                                  subtype = rep(i, length(dat[[which(time_vis == j)]]$x)))

      })

      df <- do.call(rbind, df_list_internal)

    })

    df <- do.call(rbind, df_list)

    gg <- ggplot2::ggplot(data = df, aes(x = longitude, y = latitude)) +
      ggplot2::geom_point(shape = 1, size = 1.5) +
      ggplot2::coord_quickmap() +
      ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "black") +
      ggthemes::theme_map() +
      ggplot2::ggtitle(paste0(paste0(main, collapse = ", "),
                              "\n(Time Periods ", time_vis[1], " - ", time_vis[length(time_vis)], ")")) +
      ggplot2::theme(strip.text = element_text(hjust = 0.5, face = "bold"),
                     strip.background = element_rect(fill = "white", color = "white"),
                     plot.title = element_text(hjust = 0.5, face = "bold")) +
      ggplot2::facet_wrap(vars(subtype))

  }

  return(gg)

}
