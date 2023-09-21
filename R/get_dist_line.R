#' Get distance maps from lines and polygons
#'
#' @description
#' `get_dist_line()` generates a distance map from lines and polygons.
#'
#' @param path_to_shapefile path to shapefile
#' @param line_data sfc_MULTILINESTRING file (If available. If not, `get_dist_line()` creates it from a shapefile.)
#' @param window owin object
#' @param grayscale logical. `grayscale` specifies whether to convert plot to grayscale (by default, FALSE).
#' @param resolution resolution of raster objects
#' @param mile logical. `mile` specifies whether to return the output in miles instead of kilometers (by default,  FALSE).
#' @param preprocess logical. `preprocess` specifies whether to first pick the potentially closest point.
#' It is recommended to set `preprocess = TRUE` if users need to obtain distances from many points.
#'
#' @returns A list of im and ggplot object

get_dist_line <- function(window, path_to_shapefile, line_data = NULL,
                          grayscale, mile = FALSE, resolution, preprocess = TRUE){

  # Convert owin into sp objects -----
  window_sp <- conv_owin_into_sf(window)
  polygon <- window_sp[[1]]
  polygon_df <- window_sp[[2]]
  polygon_sfc <- window_sp[[3]]
  polygon_sf <- window_sp[[4]]
  polygon_spdf <- window_sp[[5]]

  # Create "sfc_MULTILINESTRING" if not available -----
  if(is.null(line_data)) {
    # Read and re-coordinate the shapefile for lines
    shp <- sf::st_read(path_to_shapefile)
    roads <- sf::st_geometry(shp) #Geometries of lines (e.g., roads)

  } else {
    # If it is available, then load it
    roads <- line_data

  }

  # Create a raster based on the polygon's extent
  r <- terra::rast(res = 0.1)
  v <- terra::vect(polygon_sf)  # Vect object in terra
  terra::ext(r) <- terra::ext(v)  # Set the extent of r to match the extent of v
  
  # Define the extent of the raster
  v <- terra::vect(polygon_spdf)
  r <- terra::rast(terra::ext(v), res = resolution)
  
  # Rasterize the polygon
  r <- terra::rasterize(v, r, field = 1)
  
  # Mask the raster with the polygon
  r <- terra::mask(r, v)
  
  # Convert raster to SpatialPixels
  rast_points <- terra::as.points(r)
  rast_points <- terra::crds(rast_points)
  
  # Calculate distance for each pixel -----

  message("Calculating distance...\n")

  if (preprocess) {

    # If preprocess = TRUE
    # First, pick a potential line with minimum distance for each point

    ## Convert rast points to sf objects
    rast_points_sf <- furrr::future_map(1:nrow(rast_points), function(k) {
      sf::st_point(rast_points[k, ])
    })

    rast_points_sf <- sf::st_sfc(rast_points_sf)
    rast_points_sf <- rast_points_sf %>% sf::st_set_crs(sf::st_crs(roads))

    ## Identify line ID with minimum distance for each point
    line_id_min <- sf::st_nearest_feature(rast_points_sf,
                                          sf::st_sfc(roads))

    lines_covered <- sort(unique(line_id_min)) # Not every line is a candidate

    # Second, calculate distance from these lines for each point
    progressr::with_progress({

      p <- progressr::progressor(steps = length(lines_covered))

      dists_list <- furrr::future_map(1:length(lines_covered), function(l, p) {

        p() #For progress bar

        # Identify the points with line i as the line with minimum distance
        rast_point_id <- which(line_id_min == lines_covered[l])

        # Distance from a line
        suppressWarnings( #Suppress warnings

          dist <- as.numeric(geosphere::dist2Line(rast_points[rast_point_id, ],
                                                  roads[[lines_covered[l]]][[1]],
                                                  distfun = geosphere::distVincentyEllipsoid)[, 1])

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

    # If preprocess = FALSE

    progressr::with_progress({

      p <- progressr::progressor(steps = length(roads))

      dists_list <- furrr::future_map(1:length(roads), function(l, p) {

        p() #For progress bar

        # Distance from a line
        suppressWarnings( #Suppress warnings

          dist <- as.numeric(geosphere::dist2Line(rast_points,
                                                  roads[[l]][[1]],
                                                  distfun = geosphere::distVincentyEllipsoid)[, 1])

        )

        return(dist)

      }, p = p)

    })

    # Take the minimum
    line_dists <- do.call(pmin, dists_list)

    # Create a dataframe
    if (mile) {

      dist_df <- data.frame(longitude = sp::coordinates(rast_points)[, 1],
                            latitude = sp::coordinates(rast_points)[, 2],
                            distance = line_dists * 0.621371/1000) #miles
    } else {

      dist_df <- data.frame(longitude = sp::coordinates(rast_points)[, 1],
                            latitude = sp::coordinates(rast_points)[, 2],
                            distance = line_dists/1000) #km

    }

  }

  # Generate a plot -----

  message("Generating a plot...\n")

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

  # Generate an image object -----
  distance_im <- spatstat.geom::as.im(dist_df, W = window)

  # Return a list of output -----
  return(list(distance_im = distance_im, plot = gg))

}

