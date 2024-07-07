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
#' @param ngrid the number of grid cells that is used to generate observed densities.
#' By default = 100. Notice that as you increase `ngrid`, the process gets computationally demanding.
#' @param window owin object
#'
#' @returns list of the following:
#'      * `indep_var`: independent variables
#'      * `coef`: coefficients
#'      * `intens_grid_cells`: im object of observed densities for each time period
#'      * `estimated_counts`: the number of events that is estimated by the poisson point process model for each time period
#'      * `sum_log_intens`: the sum of log intensities for each time period
#'
#' @details `get_obs_dens()` assumes the poisson point process model and
#' calculates observed densities for each time period. It depends on `spatstat.model::mppm()`.
#' Users should note that the coefficients in the output are not directly interpretable,
#' since they are the coefficients inside the exponential of the poisson model.

get_obs_dens <- function(hfr, dep_var, indep_var, ngrid = 100, window) {

  # Define function -----
  text_form <- paste0(dep_var, " ~ ", paste(indep_var, collapse = " + "))
  message("Fitting the model...\n")
  mod <- spatstat.model::mppm(as.formula(text_form), data = hfr) #Fit mppm
  coefficients <- as.numeric(spatstat.model::summary.mppm(mod)$coef) #Coefficients

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

  out <- list(indep_var = indep_var, #List of independent variables
              coef = coefficients, #Coefficients
              intens_grid_cells = intensity_grid_cells, #Integrated intensity as images
              estimated_counts = estimated_counts, #Counts
              sum_log_intens = sum_log_intensity) #Sum of log(intensity) for each time period

  class(out) <- c("list", "obs")
  return(out)

}
