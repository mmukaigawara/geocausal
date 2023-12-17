#' Get elevation data
#'
#' @description 
#' `get_elevation()` takes a directory that hosts shapefile and returns an owin object of altitudes.
#'
#' @param load_path path to the shp file (note: a folder)
#' @param ... other parameters passed to `elevatr::get_elev_raster()`. The resolution argument z must be specified.
#' 
#' @returns an im object (unit: meters)

get_elev <- function(load_path, ...) {
  
  # Prepare data
  temp <- sf::st_read(file.path(load_path)) #Download the file from geoBoundaries as needed
  temp_combined <- sf::st_union(sf::st_as_sf(temp))
  
  # Get elevation data using elevatr (z indicates resolutions)
  elevation_data <- elevatr::get_elev_raster(locations = temp, clip = "locations", ...)
  # Convert it to a dataframe
  elevation_data_df <- terra::as.data.frame(elevation_data, xy = TRUE)
  colnames(elevation_data_df)[3] <- "z"
  elevation_data_df <- elevation_data_df[complete.cases(elevation_data_df), ]
  # Convert it to an image object (based on maptools' as.im.RasterLayer function with modifications)
  rs <- terra::res(elevation_data) #resolution
  orig <- elevation_data@extent[c(1, 3)] + 0.5 * rs
  dm <- dim(elevation_data)[2:1]
  xx <- unname(orig[1] + cumsum(c(0, rep(rs[1], dm[1]-1))))
  yy <- unname(orig[2] + cumsum(c(0, rep(rs[2], dm[2]-1))))
  val <- terra::values(elevation_data)
  if(is.factor(elevation_data)){
    lev <- levels(elevation_data)[[1]]
    factor.col <- length(lev)
    val <- factor(val, levels = lev$ID, labels = lev[[factor.col]])
  }
  dim(val) <- dm #Assign dimensions
  val <- spatstat.geom::transmat(val, from = list(x="-i", y="j"), to = "spatstat")
  elevation_im <- spatstat.geom::im(val, xcol=xx, yrow=yy)
  
  # ggplot
  #gg <- ggplot() +
  #  ggplot2::geom_raster(data = elevation_data_df, aes(x = x, y = y, fill = z)) +
  #  ggplot2::geom_sf(data = temp_combined, color = "white", fill = NA) +
  #  ggplot2::coord_sf() + ggplot2::scale_fill_viridis_c(option = "plasma") + ggthemes::theme_map() +
  #  labs(title = "Elevation", x = "Longitude", y = "Latitude", fill = "Elevation (meters)") +
  #  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Return a list of RasterLayer, im object, and a ggplot object
  #return(list(rasterlayer = elevation_data,
  #            im = elevation_im,
  #            #df = elevation_data_df,
  #            plot = gg))
  return(elevation_im)
  
}