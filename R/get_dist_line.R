#' Function: get_dist_line
#'
#' A function that generates a distance map from lines (e.g., roads and rivers)
#'
#' @param path_to_shapefile Path to shapefile
#' @param line_data A sfc_MULTILINESTRING file (if available; if not the function creates it from a shapefile)
#' @param window An owin object
#' @param grayscale
#' @param mile Whether to return the output in miles instead of kilometers

get_dist_line <- function(window, path_to_shapefile, line_data = NULL,
                          grayscale, mile, resolution, ...){
  
  # Convert owin into sp objects  
  window_sp <- convert_owin_into_sf(window)
  polygon <- window_sp[[1]]
  polygon_df <- window_sp[[2]]
  polygon_sfc <- window_sp[[3]]
  polygon_sf <- window_sp[[4]]
  polygon_spdf <- window_sp[[5]]
  
  # Create "sfc_MULTILINESTRING" if not available
  if(is.null(line_data)) {
    # Read and re-coordinate the shapefile for lines
    shp <- sf::st_read(path_to_shapefile)
    roads <- sf::st_geometry(shp) #Geometries of lines (e.g., roads)
    
  } else {
    # If it is available, then load it
    roads <- line_data

  }
  
  # Create a raster based on the polygon's extent
  r <- raster::raster(res = 0.5)
  raster::extent(r) <- raster::extent(polygon_sf)
  
  # Define the extent of the raster
  r <- raster::raster(raster::extent(polygon_spdf), resolution = resolution) #Modify resolution as needed
  
  # Rasterize the polygon
  r <- raster::rasterize(polygon_spdf, r, field = 1)
  
  # Mask the raster with the polygon
  r <- raster::mask(r, polygon_spdf)
  
  # Convert raster to SpatialPixels
  rast_points <- raster::rasterToPoints(r)
  rast_points <- rast_points[, c(1:2)]

  # Calculate distance for each pixel and take the minimum
  # Do the same for all the points of interest

  lines_dists_list <- furrr::future_map(1:length(roads), function(j) {
    
    # Distance from a point
    line_dists <- furrr::future_map_dbl(1:nrow(rast_points), function(i) {
      suppressWarnings(as.numeric(geosphere::dist2Line(rast_points[i, ], roads[[j]][[1]])[, 1]))
    })
    
    line_dists <- unlist(line_dists) #Vector
    return(line_dists)
    
  }, .progress = TRUE)
  
  # Take the minimum
  line_dists <- do.call(pmin, lines_dists_list)
  
  # Create a df to store the results
  
  if (mile) {
    
    dist_df <- data.frame(longitude = sp::coordinates(rast_points)[, 1],
                          latitude = sp::coordinates(rast_points)[, 2],
                          distance = line_dists * 0.621371/1000) #miles
  } else {
    
    dist_df <- data.frame(longitude = sp::coordinates(rast_points)[, 1],
                          latitude = sp::coordinates(rast_points)[, 2],
                          distance = line_dists/1000) #km
    
  }
  
  # Generate a plot
  
  if (mile) { #miles
    
    if (grayscale) {
      
      gg <- ggplot(data = dist_df, aes(x = longitude, y = latitude, fill = distance)) +
        ggplot2::geom_tile() +
        ggplot2::coord_quickmap() +
        ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "white") +
        ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") + 
        ggthemes::theme_map() +
        ggplot2::ggtitle("Distance from Lines") + labs(fill = "Distance (mile)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
    } else {
      
      gg <- ggplot(data = dist_df, aes(x = longitude, y = latitude, fill = distance)) +
        ggplot2::geom_tile() +
        ggplot2::coord_quickmap() +
        ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "white") +
        ggplot2::scale_fill_viridis_c(option = "plasma") + 
        ggthemes::theme_map() +
        ggplot2::ggtitle("Distance from Lines") + labs(fill = "Distance (mile)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
    }
    
  } else { #kilometers
    
    if (grayscale) {
      
      gg <- ggplot(data = dist_df, aes(x = longitude, y = latitude, fill = distance)) +
        ggplot2::geom_tile() +
        ggplot2::coord_quickmap() +
        ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "white") +
        ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") + 
        ggthemes::theme_map() +
        ggplot2::ggtitle("Distance from Lines") + labs(fill = "Distance (km)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
    } else {
      
      gg <- ggplot(data = dist_df, aes(x = longitude, y = latitude, fill = distance)) +
        ggplot2::geom_tile() +
        ggplot2::coord_quickmap() +
        ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "white") +
        ggplot2::scale_fill_viridis_c(option = "plasma") + 
        ggthemes::theme_map() +
        ggplot2::ggtitle("Distance from Lines") + labs(fill = "Distance (km)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
    }
    
  }
  
  # Generate an image object
  distance_im <- spatstat.geom::as.im(dist_df, W = window)
  
  # Return a list of output
  return(list(distance_im = distance_im, plot = gg))
  
}

