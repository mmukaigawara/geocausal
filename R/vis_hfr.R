#' Function: vis_hfr
#'
#' A function that generates a plot of point processes
#'
#' @param hfr A hyperframe to convert
#' @param subtype_column A column of interest
#' @param time_range The range of dates (e.g., c("2007-02-23", "2007-02-28"))
#' @param combined Whether to combine all the point processes to one plot. By default = TRUE
#' @param marks For a combined plot, whether to use the same marks. By default = FALSE
#' @param scale_max Choose the maximum of the color scale (for visualizing images; by default = 100)

vis_hfr <- function(hfr,
                    subtype_column,
                    time_column = "time",
                    range,
                    combined = TRUE,
                    marks = FALSE,
                    scale_max = 100) {

  # Clean the data ----------
  hfr_temp <- hfr

  if (is.na(time_column) == FALSE) {

    time_id <- which(names(hfr_temp) == time_column)
    names(hfr_temp)[time_id] <- "time"
    min_row <- which(hfr_temp$time == range[1])
    max_row <- which(hfr_temp$time == range[2])

  } else {
    # If no time is specified, then simply use row IDs
    min_row <- range[1]
    max_row <- range[2]

  }

  targetcol_id <- which(names(hfr_temp) == subtype_column)
  names(hfr_temp)[targetcol_id] <- "targetcol"

  all_rows <- seq(min_row, max_row, by = 1)

  hfr_temp <- hfr_temp[all_rows, ]

  # If the object is smoothed outcomes (ie, pixel images) ----------
  if (class(hfr_temp$targetcol)[1] == "imlist") {

    if (combined == FALSE) {

      plot_out <- plot(hfr_temp[, "targetcol"],
                       main = paste0(subtype_column, " from ", range[1], " to ", range[2]),
                       zlim = c(0, scale_max))

    } else if (combined == TRUE) {

      hfr_temp_selected <- hfr_temp[1, "targetcol"]

      # First add up smoothed outcomes (add v of all density.ppp outputs)
      smoothed_base <- hfr_temp$targetcol[[1]]$v

      for (ii in 2 : length(all_rows)) {
        smoothed_base <- smoothed_base + hfr_temp$targetcol[[ii]]$v
      }

      # Then use it for visualization
      hfr_temp_selected$targetcol[[1]]$v <- smoothed_base

      plot_out <- plot(hfr_temp_selected[, "targetcol"],
                       main = paste0(subtype_column, " from ", range[1], " to ", range[2]),
                       zlim = c(0, scale_max))

    }

  # If the object is point processes (ie, ppp) ----------

  } else if (class(hfr_temp$targetcol)[1] == "ppplist")

  if (combined == FALSE) {

    plot_out <- plot(hfr_temp[, "targetcol"],
                     main = paste0(subtype_column, " from ", range[1], " to ", range[2]))

  } else if (combined == TRUE) {

    out <- do.call(eval(parse(text = "spatstat.geom::superimpose")),
                   hfr_temp$targetcol)

    if (marks == TRUE) {
      out$marks <- NULL # If marks = TRUE, use the same shape for all dates
      }

    plot_out <- plot(out, main = paste0(subtype_column, " from ",
                                        range[1], " to ", range[2]))

  }

  return(plot_out)

}
