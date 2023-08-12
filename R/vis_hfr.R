#' Visualize hyperframes
#'
#' @description
#' `vis_hfr()` takes a hyperframe and visualizes columns that users specify.
#' `vis_hfr()` is used mainly for the visualization of the output of [get_hfr()] function.
#'
#' @param hfr hyperframe
#' @param subtype_column The name/s of a column of interest.
#' To specify multiple columns, users should list column names as a character vector.
#' @param time_column The name of the column of time variable. By default, `"time"`. Note that the time variable must be integers.
#' @param range vector that specifies the range of tiem variable (e.g., `c("2007-01-01", "2007-01-31")`)
#' @param combined logical. `combined` specifies whether to combine all the point processes to one plot.
#' This argument applies only to the case when users specify one column with multiple time periods.
#' By default = TRUE
#'
#' @returns ggplot object that displays ppp objects of interest
#'
#' @examples
#' # Data
#' dat <- data.frame(time = c(1, 1, 2, 2),
#'                   longitude = c(43.9, 44.5, 44.1, 44.0),
#'                   latitude = c(33.6, 32.7, 33.6, 33.5),
#'                   type = rep(c("treat", "out"), 2))
#'
#' # Hyperframe
#' dat_hfr <- get_hfr(data = dat,
#'                    subtype_column = "type",
#'                    window = iraq_window,
#'                    time_column = "time",
#'                    time_range = c(1, 2),
#'                    coordinates = c("longitude", "latitude"),
#'                    combined = FALSE)
#'
#' # Visualization
#' vis_hfr(hfr = dat_hfr,
#'         subtype_column = c("treat"),
#'         time_column = "time",
#'         range = c(1:2),
#'         combined = TRUE)

vis_hfr <- function(hfr,
                    subtype_column,
                    time_column = "time",
                    range,
                    combined = TRUE) {

  # Clean the hyperframe -----
  hfr_temp <- hfr

  if (length(range) == 1) { all_rows <- range } else
  { all_rows <- seq(min(range), max(range), by = 1)} #Sequence of all rows

  time_id <- which(names(hfr_temp) == time_column)
  names(hfr_temp)[time_id] <- "time" #Rename the time column

  row_id <- which(hfr_temp$time %in% all_rows) #Obtain the time period row IDs
  outcome_id <- which(names(hfr_temp) %in% subtype_column) #Obtain the outcome column IDs

  hfr_cleaned <- hfr_temp[row_id, c(time_id, outcome_id)] #Return necessary portions of hfr

  # Visualization (preparation) -----
  num_time_period <- nrow(hfr_cleaned)
  num_outcome_columns <- ncol(hfr_cleaned) - 1 #Subtract 1 d/t time column

  # Convert window to df
  window <- spatstat.geom::Window(hfr_cleaned[1, which(colnames(hfr_cleaned) == subtype_column[1])][[1]])
  window_sp <- conv_owin_into_sf(window)
  polygon_df <- window_sp[[2]]

  # Function for grayscaling (for density plot only)
  convert_to_grayscale <- function(ggplot_object, grayscale) {

    if (grayscale) {
      ggplot_obj <- ggplot_object +
        ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys")
    } else {
      ggplot_obj <- ggplot_object +
        ggplot2::scale_fill_viridis_c(option = "plasma")
    }

    return(ggplot_obj)

  }

  # Visualization (four patterns wrt time periods and outcome columns) -----

  if (num_time_period == 1 && num_outcome_columns == 1) {

    # Case 1: One time period x One outcome column

    ## If ppp
    outcome_name <- colnames(hfr_cleaned)[-1] #Names of outcomes
    time_vis <- hfr_cleaned$time[1]$time #Time period to visualize
    dat <- hfr_cleaned[1, 2][[1]] #Convert points to df
    df <- data.frame(longitude = dat$x, latitude = dat$y)

    gg <- ggplot2::ggplot(data = df, aes(x = longitude, y = latitude)) +
      ggplot2::geom_point(shape = 1, size = 1.5) +
      ggplot2::coord_quickmap() +
      ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "black") +
      ggthemes::theme_map() +
      ggplot2::ggtitle(paste0(outcome_name, "\n(Time Period ", time_vis, ")")) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5, face = "bold"))

    ## If image -> TBD

  } else if (num_time_period == 1 && num_outcome_columns > 1) {

    # Case 2: One time period x Multiple outcome columns

    ## If ppp
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
      ggplot2::ggtitle(paste0(paste0(outcome_name, collapse = ", "), "\n(Time period ", time_vis, ")")) +
      ggplot2::theme(strip.text = element_text(hjust = 0.5, face = "bold"),
                     strip.background = element_rect(fill = "white", color = "white"),
                     plot.title = element_text(hjust = 0.5, face = "bold")) +
      ggplot2::facet_wrap(vars(subtype))

    ## If Image -> TBD

  } else if (num_time_period > 1 && num_outcome_columns == 1) {

    # Case 3: Multiple time periods x One outcome column

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
        ggplot2::ggtitle(paste0(outcome_name, "\n(Time Periods ", time_vis[1], " - ", time_vis[length(time_vis)], ")")) +
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
        ggplot2::ggtitle(paste0(outcome_name, "\n(Time Periods ", time_vis[1], " - ", time_vis[length(time_vis)], ")")) +
        ggplot2::theme(strip.text = element_text(hjust = 0.5, face = "bold"),
                       strip.background = element_rect(fill = "white", color = "white"),
                       plot.title = element_text(hjust = 0.5, face = "bold")) +
        ggplot2::facet_wrap(vars(time_period))

    }

    ## If Image -> TBD

  } else if (num_time_period > 1 && num_outcome_columns > 1) {

    # Case 4: Mutliple time periods x Multiple outcome columns

    ## If ppp
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
      ggplot2::ggtitle(paste0(paste0(outcome_name, collapse = ", "),
                              "\n(Time Periods ", time_vis[1], " - ", time_vis[length(time_vis)], ")")) +
      ggplot2::theme(strip.text = element_text(hjust = 0.5, face = "bold"),
                     strip.background = element_rect(fill = "white", color = "white"),
                     plot.title = element_text(hjust = 0.5, face = "bold")) +
      ggplot2::facet_wrap(vars(subtype))

    ## If Image -> TBD

  }

  # If the object is smoothed outcomes (ie, pixel images) ----------
  #if (class(hfr_temp$Outcome)[1] == "imlist") {
  # If the object is point processes (ie, ppp) ----------
  #} else if (class(hfr_temp$Outcome)[1] == "ppplist")

  return(gg)

}
