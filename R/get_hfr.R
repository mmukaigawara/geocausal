#' Create a hyperframe
#'
#' @description
#' `get_hfr()` takes a dataframe with time and location variables
#' and generates a hyperframe with point patterns.
#' `get_hfr()` is usually the first function that users employ in order to
#' perform spatiotemporal causal inference analytic methods.
#'
#' @param data dataframe. The dataframe must have time and location variables.
#' Location variables should be standard coordinates (i.e., longitudes and latitudes).
#' @param subtype_column the name of the column for subtypes of events of interest
#' @param window owin object (for more information, refer to `spatstat.geom::owin()`).
#' Basically, an owin object specifies the geographical boundaries of areas of interest.
#' @param time_column the name of the column for time variable. Note that the time variable must be integers.
#' @param time_range numeric vector. `time_range` specifies the range of the time variable
#' (i.e., min and max of the time variable).
#' The current version assumes that the unit of this time variable is dates.
#' @param coordinates character vector. `coordinates` specifies the names of columns for locations.
#' By default, `c("longitude", "latitude")` in this order. Note that the coordinates must be in decimal degree formats.
#' @param combined logical. `combined` tells whether to generate output for all subtypes of events combined.
#' By default, `TRUE`, which means that a column of ppp objects with all subtypes combined is generated in the output.
#'
#' @importFrom data.table .BY .SD
#'
#' @returns A hyperframe is generated with rows representing time and columns representing the following:
#'     * The first column: time variable
#'     * The middle columns: ppp objects (see `spatstat.geom::ppp()`) generated for each subtype of events of interest
#'     * The last column (if `combined = TRUE`): ppp objects with all subtypes combined. This column is named as `all_combined`.
#' @examples
#' # Data
#' dat <- data.frame(time = c(1, 1, 2, 2),
#'                   longitude = c(43.9, 44.5, 44.1, 44.0),
#'                   latitude = c(33.6, 32.7, 33.6, 33.5),
#'                   type = rep(c("treat", "out"), 2))
#'
#' # Hyperframe
#' get_hfr(data = dat,
#'         subtype_column = "type",
#'         window = iraq_window,
#'         time_column = "time",
#'         time_range = c(1, 2),
#'         coordinates = c("longitude", "latitude"),
#'         combined = FALSE)

get_hfr <- function(data, subtype_column,
                    window,
                    time_column,
                    time_range,
                    coordinates = c("longitude", "latitude"),
                    combined = TRUE) {

  # Getting the range of dates -----------
  all_time <- seq(time_range[1], time_range[2], by = 1)

  # Cleaning the data ----------
  data <- data %>%
    dplyr::select(time_column, subtype_column, coordinates[1], coordinates[2])
  colnames(data) <- c("time", "type", "longitude", "latitude")
  data.table::setDT(data)

  if (combined){
    data_c <- data
    data_c[, "type"] <- "all_combined"
    data <- rbind(data, data_c)
  }

  # Converting data to ppp ----------
  message("Converting the data to ppp objects...\n")

  ## Creating empty point process for imputation
  empty <- data.table::data.table(latitude = double(), longitude = double())
  empty_ppp <- spatstat.geom::as.ppp(cbind(x = empty$longitude, y = empty$latitude), W = window)

  ## Converting data to point process
  as.list(
    data[, .(list(spatstat.geom::as.ppp(cbind(x = .SD$longitude, y = .SD$latitude),
                                        W = window))),
         by = list(time, type)]
  ) -> x_ppp

  message("Converting the data to a hyperframe...\n")

  ## Identifying missing dates
  all_time_type <- expand.grid(time = all_time,
                               type = as.character(unique(data[, type])),
                               KEEP.OUT.ATTRS = FALSE)
  all_time_type$type <- as.character(all_time_type$type)
  data.table::setDT(all_time_type)

  obs_time_type <- data.table::data.table(time = x_ppp$time, type = x_ppp$type)

  missing_time_type <- all_time_type[!obs_time_type, on = c("time", "type")]

  ## Combining observed and missing data
  x_list <- c(x_ppp$V1, rep(list(empty_ppp), length(missing_time_type[[1]])))
  x_index <- rbind(obs_time_type, missing_time_type)
  data_types <- unique(data[, type])

  lapply(1:length(data_types),
         function(ii){
           x_list[which(x_index$type == data_types[ii])]
         }) -> x_list

  x_hyperframe <- spatstat.geom::hyperframe(time = all_time)

  for(jj in 1:length(data_types)){
    x_hyperframe[, data_types[jj]] <-
      x_list[[jj]][order(subset(x_index, type == data_types[[jj]])$time)]
  }

  # Finalizing the hyperframe ----------
  message("Generating a hyperframe of point processes...\n")

  return(x_hyperframe)

}

