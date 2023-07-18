#' Convert windows into sf objects
#'
#' @description `conv_owin_into_sf` takes an owin object and converts it to sf-related objects. 
#' This function is mostly an internal function of other functions.
#' 
#' @param window owin object
#' 
#' @returns list of polygon, dataframe, sfc_POLYGON, sf, and SpatialPolygonsDataFrame objects

conv_owin_into_sf <- function(window) {
  
  # Convert owin to polygonal object
  window_polygon <- spatstat.geom::as.polygonal(window)
  
  # Extract coordinates
  coords_list <- lapply(window_polygon$bdry, function(bdry) {
    coords <- cbind(bdry$x, bdry$y)
    ## Check if the polygon is closed, if not, add the first point to the end
    if (!all(coords[1, ] == coords[nrow(coords), ])) {
      coords <- rbind(coords, coords[1, ])
    }
    coords
  })
  
  # Conversion
  
  ## Class: POLYGON
  polygon <- sf::st_polygon(list(coords_list[[1]]))
  
  ## Class: dataframe with coordinates
  polygon_df <- as.data.frame(polygon[1][[1]])
  colnames(polygon_df) <- c("longitude", "latitude")
  polygon_df$polygon_id <- 1
  
  ## Class: sfc_POLYGON
  polygon_sfc <- sf::st_sfc(polygon)
  
  ## Class: sf
  polygon_sf <- sf::st_sf(polygon_sfc)

  ## Class: SpatialPolygonsDataFrame
  polygon_spdf <- sf::as_Spatial(polygon_sf)
  
  return(list(polygon, polygon_df, polygon_sfc, polygon_sf, polygon_spdf))
  
}