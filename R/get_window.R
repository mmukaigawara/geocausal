#' Generate a window
#'
#' @description `get_window()` takes a directory that hosts a shapefile
#' and returns an owin object.
#'
#' @param load_path path to the shp file
#' @param target_crs target CRS (if users want to specify it; as an EPSG code)
#'
#' @returns owin object

get_window <- function(load_path, target_crs = NULL) {

  # 1. Reading the data
  temp <- sf::st_read(load_path, quiet = TRUE)

  # 2. Handling Projection
  if (!is.null(target_crs)) {

    temp <- sf::st_transform(temp, crs = target_crs)

  } else if (sf::st_is_longlat(temp)) {

    suggested <- crsuggest::suggest_crs(temp, units = "m")

    if (nrow(suggested) > 0) {
      auto_crs <- as.numeric(suggested$crs_code[1])
      message(paste0("Projecting to suggested Cartesian CRS: EPSG:", auto_crs))
      temp <- sf::st_transform(temp, crs = auto_crs)
    } else {
      stop("No suggested Cartesian CRS was identified. Please provide a 'target_crs' for projection.")
    }
  }

  # 3. Converting the data
  #temp_union <- sf::st_union(temp)
  temp_matrix <- sf::st_coordinates(temp)

  # Checking whether data is clockwise
  clockwise <- function(xx) {

    x.coords <- c(xx[[1]], xx[[1]][1])
    y.coords <- c(xx[[2]], xx[[2]][1])

    double.area <- sum(sapply(2:length(x.coords), function(ii) {
      (x.coords[ii] - x.coords[ii-1])*(y.coords[ii] + y.coords[ii-1])
    }))

    double.area > 0
  }

  # temp_matrix[, 1] and [, 2] are now in meters/Cartesian units
  clockwise_check <- clockwise(data.frame(x = temp_matrix[, 1],
                                          y = temp_matrix[, 2]))

  # Generating a window
  if (clockwise_check) {
    temp_window <- spatstat.geom::owin(poly = data.frame(x = rev(temp_matrix[, 1]),
                                                         y = rev(temp_matrix[, 2])))
  } else {
    temp_window <- spatstat.geom::owin(poly = data.frame(x = temp_matrix[, 1],
                                                         y = temp_matrix[, 2]))
  }

  attr(temp_window, "crs") <- sf::st_crs(temp) # Add crs information

  return(temp_window)

}
