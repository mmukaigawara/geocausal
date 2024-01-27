#' Generate a window
#'
#' @description `get_window()` takes a directory that hosts a shapefile
#' and returns an owin object.
#'
#' @param load_path path to the shp file
#'
#' @returns owin object

get_window <- function(load_path) {

  # Reading the data
  temp <- sf::st_read(file.path(load_path))

  # Converting the data
  #temp <- sf::st_union(sf::st_as_sf(temp)) #union only required if we need to combine shapefiles at lower admin levels
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

  clockwise_check <- clockwise(data.frame(x = temp_matrix[, 1],
                                          y = temp_matrix[, 2]))

  # Generating a window
  if (clockwise_check) {
    temp_window <- spatstat.geom::owin(poly = data.frame(x = rev(temp_matrix[, 1]), y = rev(temp_matrix[, 2])))
  } else {
    temp_window <- spatstat.geom::owin(poly = data.frame(x = temp_matrix[, 1], y = temp_matrix[, 2]))
  }

  return(temp_window)

}
