#' Get distance maps
#'
#' @description
#' `get_dist_focus()` generates a distance map from focus locations.
#'
#' @param lon vector of longitudes
#' @param lat vector of latitudes
#' @param window owin object
#' @param resolution resolution of raster objects (in meters; by default, 1000)
#' @param mile logical. `mile` specifies whether to return the output in miles instead of kilometers (by default, FALSE).
#' @param preprocess logical. `preprocess` specifies whether to first pick the potentially closest point.
#' It is recommended to set `preprocess = TRUE` if users need to obtain distances from many points.
#' @param input_crs the CRS of the focus points (defaults to 4326). These points are internally projected
#' to match the window CRS to ensure isotropic distance calculations.
#'
#' @returns an im object
#'
#' @details
#' `get_dist_focus()` depends on `geosphere::distVincentyEllipsoid()`.
#' Since it calculates accurate distances considering the ellipsoid, the process sometimes
#' becomes computationally demanding, namely when we need to obtain distances from many points.
#' In that case, users can set `preprocess = TRUE`. With this option, `get_dist_focus()` calculates
#' distances from points by first identifying the closest point using `sf::st_nearest_feature()` with approximations.
#' This process is more efficient than computing distances from all the points
#' with `geosphere::distVincentyEllipsoid()` and then obtaining the minimum of all the distances.
#' By default, `get_dist_focus()` returns distances in kilometers unless users set `mile =  TRUE`.

get_dist_focus <- function(window, lon, lat, resolution = 1000,
                           mile = FALSE, preprocess = FALSE,
                           input_crs = 4326) {

  # Convert owin into sp objects
  window_sp <- conv_owin_into_sf(window)
  polygon_sf <- window_sp[[4]]
  polygon_spdf <- window_sp[[5]]

  detected_crs <- attr(window, "crs") # Get CRS from the owin object

  if (is.na(detected_crs)) {
    stop("The window object has no CRS. Please ensure get_window() was used with a target_crs.")
  }

  # Create sf objects
  point_df <- data.frame(longitude = lon, latitude = lat)
  num_points <- nrow(point_df)
  point_sf <- sf::st_as_sf(point_df, coords = c("longitude", "latitude"),
                           crs = input_crs)
  point_sf_proj <- sf::st_transform(point_sf, detected_crs)
  point_coords_proj <- sf::st_coordinates(point_sf_proj)

  # 3. Align Raster/Terra Objects
  v <- terra::vect(polygon_sf)
  # Ensure the raster 'r' inherits the exact CRS string from the vector 'v'
  r <- terra::rast(terra::ext(v), res = resolution, crs = terra::crs(v))

  # Rasterize and Mask
  r <- terra::rasterize(v, r, field = 1)
  r <- terra::mask(r, v)

  # Convert raster to projected coordinates
  rast_points <- terra::crds(terra::as.points(r))

  # Calculate distance for each pixel
  if (preprocess) {
    rast_points_sf <- sf::st_as_sf(as.data.frame(rast_points),
                                   coords = c("x", "y"), crs = detected_crs)

    point_id_min <- sf::st_nearest_feature(rast_points_sf, point_sf_proj)
    points_covered <- sort(unique(point_id_min))

    progressr::with_progress({
      p <- progressr::progressor(steps = length(points_covered))

      dists_list <- furrr::future_map(1:length(points_covered), function(l) {
        p()
        rast_point_id <- which(point_id_min == points_covered[l])

        p1 <- rast_points[rast_point_id, , drop = FALSE]
        p2 <- point_coords_proj[points_covered[l], , drop = FALSE]

        dist <- sqrt((p1[,1] - p2[,1])^2 + (p1[,2] - p2[,2])^2)
        return(cbind(rast_point_id, dist))
      })
    })

    dists_mat <- do.call(rbind, dists_list)
    final_dists <- dists_mat[order(dists_mat[, 1]), 2]

  } else {
    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(point_coords_proj))

      point_dists_list <- furrr::future_map(1:nrow(point_coords_proj), function(j) {
        p()
        p2 <- point_coords_proj[j, ]
        sqrt((rast_points[,1] - p2[1])^2 + (rast_points[,2] - p2[2])^2)
      })
    })
    final_dists <- do.call(pmin, point_dists_list)
  }

  if (mile) {
    dist_val <- final_dists * 0.000621371 # meters to miles
  } else {
    dist_val <- final_dists / 1000        # meters to km
  }

  dist_df <- data.frame(longitude = rast_points[, 1],
                        latitude = rast_points[, 2],
                        distance = dist_val)

  return(spatstat.geom::as.im(dist_df, W = window))

}

