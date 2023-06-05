#' Function: get_elevation
#'
#' A function that takes a directory that hosts shp file
#' and returns an owin object
#'
#' @param load_path A path to the shp file (note: a folder)

get_elevation <- function(load_path, ...) {
  
  temp <- sf::st_read(file.path(load_path)) #Download the file from geoBoundaries as needed
  temp_combined <- sf::st_union(sf::st_as_sf(temp))
  
  # Get elevation data using elevatr (z indicates resolutions)
  elevation_data <- elevatr::get_elev_raster(locations = temp, z = 5, clip = "locations")
  # Convert it to a dataframe
  elevation_data_df <- raster::as.data.frame(elevation_data, xy = TRUE)
  colnames(elevation_data_df)[3] <- "z"
  elevation_data_df <- elevation_data_df[complete.cases(elevation_data_df), ]
  # Convert it to an image object
  elevation_im <- maptools_as.im.RasterLayer(elevation_data)

  # ggplot
  gg <- ggplot() +
    ggplot2::geom_raster(data = elevation_data_df, aes(x = x, y = y, fill = z)) +
    ggplot2::geom_sf(data = temp_combined, color = "white", fill = NA) +
    coord_sf() + scale_fill_viridis_c(option = "plasma") + theme_map() +
    labs(title = "Elevation", x = "Longitude", y = "Latitude", fill = "Elevation (meters)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Return a list of RasterLayer, im object, and a ggplot object
  return(list(rasterlayer = elevation_data,
              im = elevation_im,
              #df = elevation_data_df,
              plot = gg))
  
}