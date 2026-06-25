#' Get the baseline density
#'
#' @description `get_base_dens()` takes a dataframe and
#' returns the baseline densities using Scott's rule of thumb (out-of-sample data).
#'
#' @param window owin object
#' @param ndim the number of dimensions of grid cells (ndim^2). By default, ndim = 128.
#' @param resolution the resolution in km per pixel. If specified, overrides `ndim`.
#' For example, `resolution = 5` creates ~5km x 5km grid cells.
#' @param out_data dataframe
#' @param out_coordinates vector of column names of longitudes and latitudes (in this order)
#' @param input_crs the CRS of the input \code{coordinates}. Defaults to \code{4326}
#' @param unit_scale parameter to convert meters to kilometers
#' (WGS84 decimal degrees). The function will transform these to match the \code{window} projection
#'
#' @returns an im object of baseline density
#'
#' @details `get_base_dens()` estimates a baseline (out-of-sample) density from
#' point data supplied in `out_data`. The points are projected to match the CRS
#' attached to `window`, converted to a `ppp` object, and smoothed by a kernel
#' density estimate whose bandwidth is chosen by Scott's rule of thumb
#' (`spatstat.explore::bw.scott()`). The result is normalized to integrate to one.
#' The output grid is controlled either by `ndim` or, if supplied, by `resolution`
#' (in km per pixel), which overrides `ndim`. The window must carry a CRS
#' attribute (e.g., created via `get_window()` with a `target_crs`).
#'
#' @references
#' Papadogeorgou, G., Imai, K., Lyall, J. and Li, F. (2022). Causal inference with spatio-temporal data: estimating the effects of airstrikes on insurgent violence in Iraq. \emph{Journal of the Royal Statistical Society Series B}, 84(5), 1969--1999. \doi{10.1111/rssb.12548}
#'
#' Mukaigawara, M., Imai, K., Lyall, J. and Papadogeorgou, G. (2025). Spatiotemporal causal inference with arbitrary spillover and carryover effects. arXiv preprint. \doi{10.48550/arXiv.2504.03464}
#'
#' @seealso [get_cf_dens()]
#'
#' @family density estimation functions
#'
#' @examples
#' \donttest{
#' # Baseline density from out-of-sample (2007--2008) insurgency data
#' base <- get_base_dens(window = iraq_window, ndim = 64,
#'                       out_data = insurgencies,
#'                       out_coordinates = c("longitude", "latitude"))
#' }

get_base_dens <- function(window,
                          ndim = 128,
                          resolution = NULL,
                          out_data = NULL,
                          out_coordinates = c("longitude", "latitude"),
                          input_crs = 4326,
                          unit_scale = 1000) {

  # 1. Detect CRS from Window
  detected_crs <- attr(window, "crs")
  if (is.null(detected_crs)) {
    stop("The window object has no CRS. Please ensure get_window() was used with a target_crs.")
  }

  # Determine output dimensions
  if (!is.null(resolution)) {
    x_extent <- diff(window$xrange)
    y_extent <- diff(window$yrange)
    ngrid_x <- ceiling(x_extent / resolution)
    ngrid_y <- ceiling(y_extent / resolution)
    dimyx <- c(ngrid_y, ngrid_x)
    message("Using resolution mode: ", resolution, " km per pixel -> ", ngrid_y, "x", ngrid_x, " grid\n")
  } else {
    dimyx <- c(ndim, ndim)
  }

  # Option 1. Using out-of-sample data
  # if (option == "out") {
  # if (is.null(out_data)) stop("out_data must be provided for option = 'out'")

  # message("Using out-of-sample data to obtain the baseline density")

    # 2. Project out-of-sample data to match window
    out_sf <- sf::st_as_sf(out_data, coords = out_coordinates, crs = input_crs)
    out_proj <- sf::st_transform(out_sf, detected_crs)
    out_coords_proj <- sf::st_coordinates(out_proj) / unit_scale

    # Convert projected data to ppp
    # checkdup = FALSE: multiple events at identical coordinates are legitimate
    # in event data, so the duplicate-point check (and its warning) is skipped
    baseline_ppp <- spatstat.geom::as.ppp(out_coords_proj, W = window, checkdup = FALSE)

    # Apply Scott's rule of thumb (now accurate because coordinates are metric)
    scott_bandwidth <- spatstat.explore::bw.scott(baseline_ppp)

    baseline_density <- stats::density(baseline_ppp, scott_bandwidth, dimyx = dimyx)
    baseline_density <- baseline_density / spatstat.univar::integral(baseline_density)

  # } else if (option == "in") {
  #
  #   # Option 2. Using in-sample data
  #   message("Using in-sample data to obtain the baseline density")
  #
  #   # Note: hfr points were already projected in get_hfr(), so no transformation needed here.
  #   num_obs <- round(ratio * nrow(hfr), digits = 0)
  #   ids <- sample(c(rep(1, num_obs), rep(0, nrow(hfr) - num_obs)))
  #   hfr_subset <- hfr[which(ids == 1), ]
  #
  #   text_form <- paste0(dep_var, " ~ ", paste(indep_var, collapse = " + "))
  #
  #   # mppm and predict will now work correctly on the projected hyperframe
  #   mod <- spatstat.model::mppm(as.formula(text_form), data = hfr_subset, nd = ngrid)
  #   pred <- spatstat.model::predict.mppm(mod, ngrid = ngrid, type = "cif")[1, ]
  #   baseline_density <- pred[["cif"]] / spatstat.univar::integral(pred[["cif"]])
  # }

  return(baseline_density)
}
