#' Function: get_dist_focus
#'
#' A function that generates a distance map from focus locations
#'
#' @param longitude A vector of longitude and latitude of a point
#' @param latitude A vector of longitude and latitude of a point
#' @param window An owin object
#' @param grayscale
#' @param mile Whether to return the output in miles instead of kilometers

get_dist_focus <- function(window, longitude, latitude, resolution,
                           grayscale, mile){
  
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
  
  # Calculate distance for each pixel and take the minimum
  # Do the same for all the points of interest
  
  point_dists_list <- furrr::future_map(1:num_points, function(j) {

    # Distance from a point
    point_dists <- furrr::future_map_dbl(1:nrow(rast_points), function(i) {
      geosphere::distVincentySphere(rast_points[i, ], point_df[j, ])
    }) #More accurate function is distVincentyEllipsoid but it takes time
    
    return(point_dists)
    
  })
  
  # Take the minimum
  point_dists <- do.call(pmin, point_dists_list)
  
  # Create a df to store the results
  
  if (mile) {
    
    dist_df <- data.frame(longitude = sp::coordinates(rast_points)[, 1],
                          latitude = sp::coordinates(rast_points)[, 2],
                          distance = point_dists * 0.621371/1000) #miles
  } else {
    
    dist_df <- data.frame(longitude = sp::coordinates(rast_points)[, 1],
                          latitude = sp::coordinates(rast_points)[, 2],
                          distance = point_dists/1000) #km
    
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

# Old function
#get_dist_focus <- function(focus_locations,
#                           window) {
  
  # Convert to ppp
#  focus_locations_ppp <- spatstat.geom::as.ppp(X = focus_locations, W = window)
  
  # Get a distance map
#  focus_locations_distance <- spatstat.geom::distmap(focus_locations_ppp, dimyx = 256)
#  spatstat.geom::Window(focus_locations_distance) <- window #Add window
  
  # Take the exponential of -distance and divide it by the integral
#  neg_exp_distance <- exp(-focus_locations_distance)
#  distance_from_focus <- neg_exp_distance/integral(neg_exp_distance)
  
#  return(distance_from_focus)
  
#}
