#' Get distance maps from lines and polygons
#'
#' @description
#' `get_dist_line()` generates a distance map from lines and polygons.
#'
#' @param path_to_shapefile path to shapefile
#' @param line_data sfc_MULTILINESTRING file (If available. If not, `get_dist_line()` creates it from a shapefile.)
#' @param window owin object
#' @param resolution resolution of raster objects (distance map) (in km; by default, 1)
#' @param mile logical. `mile` specifies whether to return the output in miles instead of kilometers (by default,  FALSE).
#' @param preprocess logical. `preprocess` specifies whether to first pick the potentially closest point.
#' It is recommended to set `preprocess = TRUE` if users need to obtain distances from many points.
#' @param unit_scale set to the same value as the parameter in `get_window()` function.
#' This parameter converts the coordinate values so that they alingn with the unit (km) of the owin object
#'
#' @details
#' The function ensures spatial integrity by automatically projecting the
#' \code{line_data} or shapefile to match the Coordinate Reference System (CRS)
#' of the \code{window}.
#'
#' @returns an im object

get_dist_line <- function(window, path_to_shapefile = NULL, line_data = NULL,
                          mile = FALSE, resolution = 1, preprocess = TRUE,
                          unit_scale = 1000) {

  window_sp <- conv_owin_into_sf(window)
  polygon_sf <- window_sp[[4]]
  detected_crs <- attr(window, "crs")

  if (is.null(detected_crs)) {
    stop("The window object has no CRS. Please ensure get_window() was used with a target_crs.")
  }

  # 2. Handle Line Data and Project it
  if (is.null(line_data)) {
    roads <- sf::st_read(path_to_shapefile, quiet = TRUE)
  } else {
    roads <- line_data
  }

  # Ensure roads match the window's projection
  roads <- sf::st_transform(roads, detected_crs)

  # 3. Create Raster
  v <- terra::vect(polygon_sf)
  r <- terra::rast(terra::ext(v), res = resolution, crs = terra::crs(v))

  r <- terra::rasterize(v, r, field = 1, background = NA)

  rast_points <- terra::crds(terra::as.points(r))
  rast_points_sf <- sf::st_as_sf(as.data.frame(rast_points * unit_scale),
                                 coords = c("x", "y"), crs = detected_crs)

  # 4. Calculate Distance
  message("Calculating distance to lines...\n")

  if (preprocess) {
    # Identify the nearest line feature for each raster point
    line_id_min <- sf::st_nearest_feature(rast_points_sf, roads)
    lines_covered <- sort(unique(line_id_min))

    progressr::with_progress({
      p <- progressr::progressor(steps = length(lines_covered))

      dists_list <- furrr::future_map(1:length(lines_covered), function(l) {
        p()
        rast_point_id <- which(line_id_min == lines_covered[l])

        # Calculate Euclidean distance to the specific nearest line
        # st_distance returns a matrix; we take the diagonal or use drop=F
        d <- sf::st_distance(rast_points_sf[rast_point_id, ], roads[lines_covered[l], ])

        return(cbind(rast_point_id, as.numeric(d)))
      })
    })

    dists_mat <- do.call(rbind, dists_list)
    final_dists <- dists_mat[order(dists_mat[, 1]), 2]

  } else {
    # Calculate distance to ALL lines and take the minimum (Slower)
    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(roads))

      dists_list <- furrr::future_map(1:nrow(roads), function(l) {
        p()
        as.numeric(sf::st_distance(rast_points_sf, roads[l, ]))
      })
    })
    final_dists <- do.call(pmin, dists_list)
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
