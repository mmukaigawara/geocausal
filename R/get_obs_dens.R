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

get_obs_dens <- function(hfr, dep_var, indep_var, ngrid = 100, window) {

  # Define function -----
  text_form <- paste0(dep_var, " ~ ", paste(indep_var, collapse = " + "))
  message("Fitting the model...\n")
  mod <- spatstat.model::mppm(as.formula(text_form), data = hfr) #Fit mppm
  #coefficients <- as.numeric(spatstat.model::summary.mppm(mod)$coef) #Coefficients
  glm_summary <- summary(mod$Fit$FIT)
  coefficients <- glm_summary$coefficients # Coef
  out_deviance <- mod$Fit$FIT$deviance # Deviance
  out_null_dev <- mod$Fit$FIT$null.deviance # Null deviance
  pearson_chisq <- sum(stats::residuals(mod$Fit$FIT, type = "pearson")^2)
  out_dispersion <- pearson_chisq / mod$Fit$FIT$df.residual # Dispersion

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
