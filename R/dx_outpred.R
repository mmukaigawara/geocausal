#' Perform out-of-sample prediction
#'
#' @description
#' `dx_outpred()` performs out-of-sample prediction
#' (separating data into training and test sets).
#' It assumes that training and test sets have the same window.
#'
#' @param hfr hyperframe
#' @param ratio numeric. ratio between training and test sets
#' @param dep_var dependent variables
#' @param indep_var independent variables
#' @param ndim the number of grids. By default, `128` (128 x 128).
#' @param resolution the resolution in km per pixel. If specified, overrides `ndim`.
#' For example, `resolution = 5` creates ~5km x 5km grid cells.
#' @param window owin object
#'
#' @returns list of the following:
#'      * `indep_var`: independent variables
#'      * `coef`: coefficients
#'      * `intens_grid_cells`: im object of observed densities for each time period
#'      * `estimated_counts`: the number of events that is estimated by the poisson point process model for each time period
#'      * `sum_log_intens`: the sum of log intensities for each time period
#'      * `training_row_max`: the max row ID of the training set

dx_outpred <- function(hfr, ratio, dep_var, indep_var, ndim = 128, resolution = NULL, window) {

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

  # Separate data into training and test sets
  training_row_max <- trunc(nrow(hfr)*ratio)

  hfr_train <- hfr[c(1:training_row_max), ] #Training set
  #hfr_test <- hfr[c(training_row_max+1:nrow(hfr)), ] #Test set

  # Define function -----
  text_form <- paste0(dep_var, " ~ ", paste(indep_var, collapse = " + "))
  message("Fitting the model...\n")
  mod <- spatstat.model::mppm(as.formula(text_form), data = hfr_train) #Fit mppm
  coefficients <- as.numeric(summary(mod)$coef) #Coefficients

  # Obtain fitted values of the propensity score -----
  message("Calculating the intensity...\n")
  intensity_grid_cells <- spatstat.model::predict.mppm(mod, type = "cif", newdata = hfr, ngrid = ngrid)$cif #Returns intensity (cif) over nxn grid cells
  message("Integrating the intensity to obtain the propensity score...\n")
  estimated_counts <- sapply(intensity_grid_cells, function(x) integral(x, domain = window)) #Integrate intensity over the window (so, e_t(w) for each date)
  intensity_of_each_obs <- spatstat.model::fitted.mppm(mod, dataonly = TRUE) #Return fitted cif for each observation for each date
  sum_log_intensity <- sapply(intensity_of_each_obs, function(x) { #Take the sum of the above for each date
    r <- 0
    if (length(x) > 0) r <- sum(log(x))
    return(r)
  })

  return(list(indep_var = indep_var, #List of RHS variables
              coef = coefficients, #Coefficients
              intens_grid_cells = intensity_grid_cells, #Integrated intensity as images
              estimated_counts = estimated_counts, #Counts
              sum_log_intens = sum_log_intensity, #Sum of log(intensity) for each time period
              training_row_max = training_row_max)) #Max row ID of training data

}
