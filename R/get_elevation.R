#' Get elevation data
#'
#' @description 
#' `get_elevation()` takes a directory that hosts shapefile and returns an owin object of altitudes.
#'
#' @param load_path path to the shp file (note: a folder)
#' @param ... other parameters passed to `elevatr::get_elev_raster()`
#' 
#' @returns list of a raster layer, an im object, and a ggplot object of altitudes (in meters).

get_elevation <- function(load_path, ...) {
  
  # maptools function
  maptools_as.im.RasterLayer <- function(from, factor.col.name = NULL) {
    
    rs <- raster::res(from)
    orig <- sp::bbox(from)[, 1] + 0.5 * rs
    dm <- dim(from)[2:1]
    xx <- unname(orig[1] + cumsum(c(0, rep(rs[1], dm[1]-1))))
    yy <- unname(orig[2] + cumsum(c(0, rep(rs[2], dm[2]-1))))
    val <- raster::values(from)
    if(is.factor(from)){
      lev <- levels(from)[[1]]
      if(!is.null(factor.col.name)){
        if(factor.col.name %in% colnames(lev)){
          factor.col <- which(colnames(lev) == factor.col.name)
        } else {
          stop("'factor.col.name' is not a column name of the raster 'from'")
        }
      }else{
        factor.col <- length(lev)
      }
      val <- factor(val, levels = lev$ID, labels = lev[[factor.col]])
    }
    ## Assign dimensions to `val` as a matrix in raster layout:
    dim(val) <- dm
    ## Transform to spatstat format
    val <- spatstat.geom::transmat(val, from = list(x="-i", y="j"), to = "spatstat")
    im <- spatstat.geom::im(val, xcol=xx, yrow=yy)
    return(im)
  }
  
  # Prepare data
  temp <- sf::st_read(file.path(load_path)) #Download the file from geoBoundaries as needed
  temp_combined <- sf::st_union(sf::st_as_sf(temp))
  
  # Get elevation data using elevatr (z indicates resolutions)
  elevation_data <- elevatr::get_elev_raster(locations = temp, clip = "locations", ...)
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
    ggplot2::coord_sf() + ggplot2::scale_fill_viridis_c(option = "plasma") + ggthemes::theme_map() +
    labs(title = "Elevation", x = "Longitude", y = "Latitude", fill = "Elevation (meters)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Return a list of RasterLayer, im object, and a ggplot object
  return(list(rasterlayer = elevation_data,
              im = elevation_im,
              #df = elevation_data_df,
              plot = gg))
  
}