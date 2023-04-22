#' Function: vis_hfr
#'
#' A function that generates a plot of point processes
#'
#' @param hfr Hyperframe to convert
#' @param subtype_column A column of interest
#' @param time_range The range of dates
#' @param combined Whether to combine all the point processes to one plot. By default = TRUE
#' @param marks For a combined plot, whether to use the same marks. By default = TRUE

vis_hfr <- function(hfr = treatment_hfr,
                    subtype_column = "Airstrike",
                    time_column = "time",
                    time_range = c("2007-02-23", "2007-02-28"),
                    combined = TRUE,
                    marks = TRUE) {

  hfr_temp <- hfr

  time_id <- which(names(hfr_temp) == time_column)
  targetcol_id <- which(names(hfr_temp) == subtype_column)

  names(hfr_temp)[time_id] <- "time"
  names(hfr_temp)[targetcol_id] <- "targetcol"

  min_row <- which(hfr_temp$time == time_range[1])
  max_row <- which(hfr_temp$time == time_range[2])
  all_rows <- seq(min_row, max_row, by = 1)

  hfr_temp <- hfr_temp[all_rows, ]

  if (combined == FALSE) {

    plot_out <- lapply(hfr_temp[, "targetcol"], plot,
                       main = paste0(subtype_column, " from ",
                                     time_range[1], " to ", time_range[2]))

  } else if (combined == TRUE) {

    out <- do.call(eval(parse(text = "spatstat.geom::superimpose")),
                   hfr_temp$targetcol)

    if (marks == TRUE) {
      out$marks <- NULL # If marks = TRUE, use the same shape for all dates
      }

    plot_out <- plot(out, main = paste0(subtype_column, " from ",
                                        time_range[1], " to ", time_range[2]))

  }

  return(plot_out)

}
