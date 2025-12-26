#' Get elevation data
#'
#' @description
#' `get_elev()` takes a directory that hosts shapefile and returns an owin object of altitudes.
#'
#' @param load_path path to the shp file (note: a folder)
#' @param ... other parameters passed to `elevatr::get_elev_raster()`. The resolution argument z must be specified.
#'
#' @returns an im object (unit: meters)

get_elev <- function(load_path, ...) {

  # 1. Prepare data
  temp <- sf::st_read(load_path, quiet = TRUE)

  # 2. Get elevation data
  elevation_data <- elevatr::get_elev_raster(locations = temp, clip = "locations", ...)

  # 3. Convert to image object
  rs <- terra::res(elevation_data)

  # Use extent directly to avoid maptools dependency issues
  ext <- elevation_data@extent
  orig <- c(ext[1], ext[3]) + 0.5 * rs

  # dm[1] is columns (width), dm[2] is rows (height)
  dm <- dim(elevation_data)[2:1]

  xx <- unname(orig[1] + (0:(dm[1] - 1)) * rs[1])
  yy <- unname(orig[2] + (0:(dm[2] - 1)) * rs[2])

  val <- terra::values(elevation_data)

  if(is.factor(elevation_data)){
    lev <- levels(elevation_data)[[1]]
    factor.col <- length(lev)
    val <- factor(val, levels = lev$ID, labels = lev[[factor.col]])
  }

  # spatstat 'im' expects mat[row, col] where nrow = length(yy) and ncol = length(xx)
  # We set the dimensions to match the physical raster layout [rows, cols]
  val_mat <- matrix(val, nrow = dm[2], ncol = dm[1], byrow = TRUE)

  # Flip the matrix vertically because Raster starts at top-left, spatstat starts at bottom-left
  val_mat <- val_mat[dm[2]:1, ]

  # Create the image
  elevation_im <- spatstat.geom::im(val_mat, xcol = xx, yrow = yy)

  return(elevation_im)
}
