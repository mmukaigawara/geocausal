#' Get the baseline density
#'
#' @description `get_base_dens()` takes a dataframe and
#' returns the baseline densities using Scott's rule of thumb (out-of-sample data)
#' or fitting an inhomogeneous Poisson model (in-sample data) by regressing
#' the in-sample data on time-invariant covariates.
#'
#' @param window owin object
#' @param option "in" (using in-sample data) or "out" (using out-of-sample data). "out" by default
#' @param ndim the number of dimensions of grid cells (ndim^2). By default, ndim = 256.
#' @param out_data dataframe (if using out-of-sample data)
#' @param out_coordinates vector of column names of longitudes and latitudes (in this order) (if using in-sample data)
#' @param hfr hyperframe (if using in-sample data)
#' @param dep_var the name of the dependent variable (if using in-sample data)
#' @param indep_var the names of time-invariant independent varaibles (if using in-sample data)
#' @param ratio for random sampling of data (if using in-sample data)
#' @param input_crs the CRS of the input \code{coordinates}. Defaults to \code{4326}
#' (WGS84 decimal degrees). The function will transform these to match the \code{window} projection
#'
#' @returns an im object of baseline density

get_base_dens <- function(window,
                          option = "out",
                          ndim = 256,
                          out_data = NULL,
                          out_coordinates = c("longitude", "latitude"),
                          hfr = NULL,
                          dep_var = NULL,
                          indep_var = NULL,
                          ratio = NULL,
                          input_crs = 4326) {

  # 1. Detect CRS from Window
  detected_crs <- attr(window, "crs")
  if (is.null(detected_crs)) {
    stop("The window object has no CRS. Please ensure get_window() was used with a target_crs.")
  }

  # Option 1. Using out-of-sample data
  if (option == "out") {
    if (is.null(out_data)) stop("out_data must be provided for option = 'out'")

    message("Using out-of-sample data to obtain the baseline density")

    # 2. Project out-of-sample data to match window
    out_sf <- sf::st_as_sf(out_data, coords = out_coordinates, crs = input_crs)
    out_proj <- sf::st_transform(out_sf, detected_crs)
    out_coords_proj <- sf::st_coordinates(out_proj)

    # Convert projected data to ppp
    baseline_ppp <- spatstat.geom::as.ppp(out_coords_proj, W = window)

    # Apply Scott's rule of thumb (now accurate because coordinates are metric)
    scott_bandwidth <- spatstat.explore::bw.scott(baseline_ppp)

    baseline_density <- stats::density(baseline_ppp, scott_bandwidth, dimyx = ndim)
    baseline_density <- baseline_density / spatstat.univar::integral(baseline_density)

  } else if (option == "in") {

    # Option 2. Using in-sample data
    message("Using in-sample data to obtain the baseline density")

    # Note: hfr points were already projected in get_hfr(), so no transformation needed here.
    num_obs <- round(ratio * nrow(hfr), digits = 0)
    ids <- sample(c(rep(1, num_obs), rep(0, nrow(hfr) - num_obs)))
    hfr_subset <- hfr[which(ids == 1), ]

    text_form <- paste0(dep_var, " ~ ", paste(indep_var, collapse = " + "))

    # mppm and predict will now work correctly on the projected hyperframe
    mod <- spatstat.model::mppm(as.formula(text_form), data = hfr_subset, nd = ndim)
    pred <- spatstat.model::predict.mppm(mod, ngrid = ndim, type = "cif")[1, ]
    baseline_density <- pred[["cif"]] / spatstat.univar::integral(pred[["cif"]])
  }

  return(baseline_density)
}
