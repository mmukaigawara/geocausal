#' Function: get_dist_focus
#'
#' @description A function that generates a distance map from focus locations
#'
#' @param longitude A vector of longitude and latitude of a point
#' @param latitude A vector of longitude and latitude of a point
#' @param window An owin object
#' @param resolution The resolution of raster objects
#' @param grayscale Whether to use grayscale
#' @param mile Whether to return the output in miles instead of kilometers
#' @param preprocess Whether to first pick the potentially closest point (better to set TRUE if there are many points)
#'
#' @returns A list of an im object and a corresponding ggplot object

get_dist_focus <- function(window, longitude, latitude, resolution,
                           grayscale, mile, preprocess = FALSE, ...){

  # Convert owin into sp objects  
  window_sp <- convert_owin_into_sf(window)
  polygon <- window_sp[[1]]
  polygon_df <- window_sp[[2]]
  polygon_sfc <- window_sp[[3]]
  polygon_sf <- window_sp[[4]]
  polygon_spdf <- window_sp[[5]]
  
  # Create sf objects
  point_df <- data.frame(longitude = longitude, latitude = latitude)
  num_points <- nrow(point_df)
  point_sf <- sf::st_as_sf(point_df, coords = c("longitude", "latitude"))
  
  # Create a raster based on the polygon's extent
  r <- raster::raster(res = 0.1)
  raster::extent(r) <- raster::extent(polygon_sf)
  
  # Define the extent of the raster
  r <- raster::raster(raster::extent(polygon_spdf), resolution = resolution)

  # Rasterize the polygon
  r <- raster::rasterize(polygon_spdf, r, field = 1)
  
  # Mask the raster with the polygon
  r <- raster::mask(r, polygon_spdf)
  
  # Convert raster to SpatialPixels
  rast_points <- raster::rasterToPoints(r)
  rast_points <- rast_points[, c(1:2)]
  
  # Calculate distance for each pixel
  
  if (preprocess) {
    
    # With preprocessing
    # First, pick a potential line with minimum distance for each point
    
    ## Convert rast points to sf objects
    rast_points_sf <- furrr::future_map(1:nrow(rast_points), function(k) {
      sf::st_point(rast_points[k, ])
    })
    
    rast_points_sf <- sf::st_sfc(rast_points_sf)
    rast_points_sf <- rast_points_sf %>% sf::st_set_crs(sf::st_crs(point_sf))
    
    ## Identify line ID with minimum distance for each point
    point_id_min <- sf::st_nearest_feature(rast_points_sf,
                                           point_sf)
    
    points_covered <- sort(unique(point_id_min)) # Not every point is a candidate
    
    # Second, calculate distance from these lines for each point
    progressr::with_progress({
      
      p <- progressr::progressor(steps = length(points_covered))
      
      dists_list <- furrr::future_map(1:length(points_covered), function(l, p) {
        
        p() #For progress bar
        
        # Identify the points with line i as the line with minimum distance
        rast_point_id <- which(point_id_min == points_covered[l])
        
        # Distance from a line
        suppressWarnings( #Suppress warnings
          
          dist <- as.numeric(geosphere::distVincentyEllipsoid(rast_points[rast_point_id, ], 
                                                              point_df[points_covered[l], ]))
          
        )
        
        return(cbind(rast_point_id, dist))
        
      }, p = p)
      
    })
    
    dists <- do.call(rbind, dists_list)
    dists <- dists[order(dists[, 1]), ] #Sort the distance
    
    # Create a dataframe
    
    if (mile) {
      
      dist_df <- data.frame(longitude = sp::coordinates(rast_points)[, 1],
                            latitude = sp::coordinates(rast_points)[, 2],
                            distance = dists[, 2] * 0.621371/1000) #miles
    } else {
      
      dist_df <- data.frame(longitude = sp::coordinates(rast_points)[, 1],
                            latitude = sp::coordinates(rast_points)[, 2],
                            distance = dists[, 2]/1000) #km
      
    }
    
  } else {
    
    # Calculate distance for each pixel and take the minimum
    # Do the same for all the points of interest

    progressr::with_progress({
      
      p <- progressr::progressor(steps = length(num_points))
      
      point_dists_list <- furrr::future_map(1:num_points, function(j, p) {
        
        # Distance from a point
        geosphere::distVincentyEllipsoid(rast_points, point_df[j, ])
        
      }, p = p)
      
    })
    
    # Take the minimum
    
    point_dists <- do.call(pmin, point_dists_list)
    
    # Create a dataframe
    
    if (mile) {
      
      dist_df <- data.frame(longitude = sp::coordinates(rast_points)[, 1],
                            latitude = sp::coordinates(rast_points)[, 2],
                            distance = point_dists * 0.621371/1000) #miles
    } else {
      
      dist_df <- data.frame(longitude = sp::coordinates(rast_points)[, 1],
                            latitude = sp::coordinates(rast_points)[, 2],
                            distance = point_dists/1000) #km
      
    }
    
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
        ggplot2::ggtitle("Distance from the Focus") + labs(fill = "Distance (mile)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
    } else {
      
      gg <- ggplot(data = dist_df, aes(x = longitude, y = latitude, fill = distance)) +
        ggplot2::geom_tile() +
        ggplot2::coord_quickmap() +
        ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "white") +
        ggplot2::scale_fill_viridis_c(option = "plasma") + 
        ggthemes::theme_map() +
        ggplot2::ggtitle("Distance from the Focus") + labs(fill = "Distance (mile)") +
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
        ggplot2::ggtitle("Distance from the Focus") + labs(fill = "Distance (km)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
    } else {
      
      gg <- ggplot(data = dist_df, aes(x = longitude, y = latitude, fill = distance)) +
        ggplot2::geom_tile() +
        ggplot2::coord_quickmap() +
        ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), fill = NA, color = "white") +
        ggplot2::scale_fill_viridis_c(option = "plasma") + 
        ggthemes::theme_map() +
        ggplot2::ggtitle("Distance from the Focus") + labs(fill = "Distance (km)") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
    }
    
  }
  
  # Generate an image object
  distance_im <- spatstat.geom::as.im(dist_df, W = window)
  
  # Return a list of output
  return(list(distance_im = distance_im, plot = gg))
  
}
