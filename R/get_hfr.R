#' Create a hyperframe
#'
#' @description
#' `get_hfr()` takes a dataframe with time and location variables
#' and generates a hyperframe with point patterns.
#'
#' @param data dataframe. The dataframe must have time and location variables.
#' @param col the name of the column for types of events of interest
#' @param window owin object (for more information, refer to `spatstat.geom::owin()`).
#' Must be a projected window (e.g., created via \code{get_window} with a \code{target_crs}
#' specified). This function automatically detects the projection from the window and projects
#' the event data accordingly.
#' @param time_col the name of the column for time variable. Note that the time variable must be integers.
#' @param time_range numeric vector. `time_range` specifies the range of the time variable
#' (i.e., min and max of the time variable).
#' The current version assumes that the unit of this time variable is dates.
#' @param coordinates character vector. `coordinates` specifies the names of columns for locations.
#' By default, `c("longitude", "latitude")` in this order. Note that the coordinates must be in decimal degree formats.
#' @param input_crs the CRS of the input \code{coordinates}. Defaults to \code{4326}
#' (WGS84 decimal degrees). The function will transform these to match the \code{window} projection
#' @param combine logical. `combine` tells whether to generate output for all subtypes of events combined.
#' By default, `TRUE`, which means that a column of ppp objects with all subtypes combined is generated in the output.
#'
#' @importFrom data.table .BY .SD
#'
#' @returns A hyperframe is generated with rows representing time and columns representing the following:
#'     * The first column: time variable
#'     * The middle columns: ppp objects (see `spatstat.geom::ppp()`) generated for each subtype of events of interest
#'     * The last column (if `combine = TRUE`): ppp objects with all subtypes combined. This column is named as `all_combined`.
#' @examples
#' # Data
#' dat <- data.frame(time = c(1, 1, 2, 2),
#'                   longitude = c(43.9, 44.5, 44.1, 44.0),
#'                   latitude = c(33.6, 32.7, 33.6, 33.5),
#'                   type = rep(c("treat", "out"), 2))
#'
#' # Hyperframe
#' get_hfr(data = dat,
#'         col = "type",
#'         window = iraq_window,
#'         time_col = "time",
#'         time_range = c(1, 2),
#'         coordinates = c("longitude", "latitude"),
#'         combine = FALSE)

get_hfr <- function(data, col,
                    window,
                    time_col,
                    time_range,
                    coordinates = c("longitude", "latitude"),
                    combine = TRUE,
                    input_crs = 4326) {

  # 1. Automatic CRS Detection from Window
  detected_crs <- attr(window, "crs")
  if (is.null(detected_crs)) {
    stop("The window object has no CRS. Please ensure get_window() was used with a target_crs.")
  }

  # 2. Project Input Data to match Window
  # Convert the raw dataframe to an sf object, project it, and extract coordinates back
  data_sf <- sf::st_as_sf(data, coords = coordinates, crs = input_crs)
  data_proj <- sf::st_transform(data_sf, detected_crs)
  coords_proj <- sf::st_coordinates(data_proj)

  # Update the data with projected coordinates
  data$longitude <- coords_proj[, 1]
  data$latitude <- coords_proj[, 2]

  # 3. Getting the range of dates
  all_time <- seq(time_range[1], time_range[2], by = 1)

  # 4. Cleaning the data
  data <- data %>%
    dplyr::select(dplyr::all_of(time_col), dplyr::all_of(col), longitude, latitude)
  colnames(data) <- c("time", "type", "longitude", "latitude")
  data.table::setDT(data)

  if (combine){
    data_c <- data.table::copy(data)
    data_c[, "type"] <- "all_combined"
    data <- rbind(data, data_c)
  }

  # 5. Converting data to ppp
  message("Converting the data to ppp objects...\n")

  ## Creating empty point process for imputation
  empty_ppp <- spatstat.geom::ppp(numeric(0), numeric(0), window = window, check = FALSE)

  ## Converting data to point process
  # Using the projected longitude/latitude now matches the window's scale (meters)
  x_ppp <- data[, .(V1 = list(spatstat.geom::as.ppp(cbind(x = .SD$longitude, y = .SD$latitude),
                                                    W = window))),
                by = list(time, type)]

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
  x_list <- c(x_ppp$V1, rep(list(empty_ppp), nrow(missing_time_type)))
  x_index <- rbind(obs_time_type, missing_time_type)
  data_types <- unique(data[, type])

  final_list <- lapply(data_types, function(tp) {
    idx <- which(x_index$type == tp)
    # Order by time within each type
    x_list[idx][order(x_index$time[idx])]
  })

  x_hyperframe <- spatstat.geom::hyperframe(time = all_time)

  for(jj in 1:length(data_types)){
    x_hyperframe[, as.character(data_types[jj])] <- final_list[[jj]]
  }

  message("Generating a hyperframe of point processes...\n")
  return(x_hyperframe)

}

