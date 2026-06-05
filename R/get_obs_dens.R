#' Generate observed densities
#'
#' @description
#' `get_obs_dens()` takes a hyperframe and returns observed densities.
#' The output is used as propensity scores.
#'
#' @param hfr hyperframe
#' @param dep_var The name of the dependent variable.
#' Since we need to obtain the observed density of treatment events,
#' `dep_var` should be the name of the treatment variable.
#' @param indep_var vector of names of independent variables (covariates)
#' @param ndim the number of grid cells that is used to generate observed densities.
#' By default = 128 (128 x 128). Notice that as you increase `ndim`, the process gets computationally demanding.
#' @param resolution the resolution in km per pixel. If specified, overrides `ndim`.
#' For example, `resolution = 5` creates ~5km x 5km grid cells.
#' @param window owin object
#'
#' @returns list of the following:
#'      * `indep_var`: independent variables
#'      * `coef`: coefficients
#'      * `deviance`: deviance
#'      * `null_deviance`: null deviance
#'      * `dispersion`: dispersion parameter
#'      * `res_df`: average residuals as a dataframe
#'      * `intens_grid_cells`: im object of observed densities for each time period
#'      * `estimated_counts`: the number of events that is estimated by the poisson point process model for each time period
#'      * `sum_log_intens`: the sum of log intensities for each time period
#'      * `actual_counts`: the number of events (actual counts)
#'      * `window`: window object used as an input
#'
#' @details `get_obs_dens()` assumes the poisson point process model and
#' calculates observed densities for each time period. It depends on `spatstat.model::mppm()`.
#' Users should note that the coefficients in the output are not directly interpretable,
#' since they are the coefficients inside the exponential of the poisson model.
#'
#' @references
#' Papadogeorgou, G., Imai, K., Lyall, J. and Li, F. (2022). Causal inference with spatio-temporal data: estimating the effects of airstrikes on insurgent violence in Iraq. \emph{Journal of the Royal Statistical Society Series B}, 84(5), 1969--1999. \doi{10.1111/rssb.12548}
#'
#' Mukaigawara, M., Imai, K., Lyall, J. and Papadogeorgou, G. (2025). Spatiotemporal causal inference with arbitrary spillover and carryover effects. arXiv preprint. \doi{10.48550/arXiv.2504.03464}
#'
#' @seealso [get_cf_dens()], [get_est()]
#'
#' @family density estimation functions
#'
#' @examples
#' \donttest{
#' # Prepare data: airstrikes (treatment) and insurgencies (outcome), Iraq 2006
#' dat <- rbind(airstrikes_2006[airstrikes_2006$type == "Airstrike", ],
#'              insurgencies_2006)
#' dat$type <- ifelse(dat$type == "Airstrike", "airstrike", "insurgency")
#' dat$time <- as.numeric(dat$date - min(dat$date) + 1)
#' dat <- dat[dat$time <= 60, ]
#' hfr <- get_hfr(data = dat, col = "type", window = iraq_window,
#'                time_col = "time", time_range = c(1, 60),
#'                coordinates = c("longitude", "latitude"), combine = FALSE)
#'
#' # Covariate surface: distance from Baghdad
#' hfr$dist_bag <- rep(list(get_dist_focus(window = iraq_window, lon = 44.366,
#'                                         lat = 33.315, ndim = 64)), nrow(hfr))
#'
#' # Observed density of the treatment (propensity score)
#' obs <- get_obs_dens(hfr, dep_var = "airstrike", indep_var = "dist_bag",
#'                     ndim = 64, window = iraq_window)
#' }

get_obs_dens <- function(hfr, dep_var, indep_var, ndim = 128, resolution = NULL, window) {

  # Determine output grid dimensions
  if (!is.null(resolution)) {
    # Resolution mode: km per pixel
    x_extent <- diff(window$xrange)
    y_extent <- diff(window$yrange)
    ngrid_x <- ceiling(x_extent / resolution)
    ngrid_y <- ceiling(y_extent / resolution)
    ngrid <- c(ngrid_y, ngrid_x)
    message("Using resolution mode: ", resolution, " km per pixel -> approx. ", ngrid_y, "x", ngrid_x, " grid\n")
  } else {
    # Pixel mode: fixed dimensions
    ngrid <- c(ndim, ndim)
    message("Using pixel mode: ", ndim, "x", ndim, " grid\n")
  }

  # Define function -----
  text_form <- paste0(dep_var, " ~ ", paste(indep_var, collapse = " + "))
  message("Fitting the model...\n")
  mod <- spatstat.model::mppm(as.formula(text_form), data = hfr) #Fit mppm
  glm_summary <- summary(mod$Fit$FIT)
  coefficients <- glm_summary$coefficients # Coef
  out_deviance <- mod$Fit$FIT$deviance # Deviance
  out_null_dev <- mod$Fit$FIT$null.deviance # Null deviance
  pearson_chisq <- sum(stats::residuals(mod$Fit$FIT, type = "pearson")^2)
  out_dispersion <- pearson_chisq / mod$Fit$FIT$df.residual # Dispersion

  # Obtain residual fields -----
  res <- residuals(mod)
  res_combined <- Reduce(`+`, res)
  res_smooth_im <- spatstat.explore::Smooth(res_combined)
  res_df <- as.data.frame(res_smooth_im)
  res_df$value = res_df$value / length(res)
  colnames(res_df) <- c("x", "y", "value")

  # Obtain fitted values of the propensity score -----
  message("Calculating the intensity...\n")
  intensity_grid_cells <- spatstat.model::predict.mppm(mod, type = "cif", ngrid = ngrid)$cif #Returns intensity (cif) over nxn grid cells
  message("Integrating the intensity to obtain the propensity score...\n")
  estimated_counts <- sapply(intensity_grid_cells, function(x) integral(x, domain = window)) #Integrate intensity over the window (so, e_t(w) for each date)
  intensity_of_each_obs <- spatstat.model::fitted.mppm(mod, dataonly = TRUE) #Return fitted cif for each observation for each date
  sum_log_intensity <- sapply(intensity_of_each_obs, function(x) { #Take the sum of the above for each date
    r <- 0
    if (length(x) > 0) r <- sum(log(x))
    return(r)
  })

  # Get the number of actual counts -----
  actual_counts <- unlist(purrr::map(hfr[[dep_var]], function(x) x$n))

  out <- list(indep_var = indep_var, #List of independent variables
              coef = coefficients, #Coefficients
              deviance = out_deviance, #Deviance
              null_deviance = out_null_dev, #Null deviance
              dispersion = out_dispersion, #Dispersion
              res_df = res_df, #Average residuals as df
              intens_grid_cells = intensity_grid_cells, #Integrated intensity as images
              estimated_counts = estimated_counts, #Estimated Counts
              sum_log_intens = sum_log_intensity, #Sum of log(intensity) for each time period
              actual_counts = actual_counts, #Actual counts
              window = window)

  class(out) <- c("obs", "list")
  return(out)

}
