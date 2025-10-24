#' Generate adaptive intervention densities based on historical data
#'
#' @description
#' `get_adaptive_baseline_dens()` takes a hyperframe for historical data and returns fitted densities for the current data.
#' The output is used as counterfactural densities.
#'
#' @param hfr_hist hyperframe for historical data
#' @param hfr_current hyperframe for current data
#' @param dep_var The name of the dependent variable
#' Since we need to obtain the counterfactural density of treatment events,
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
#'      * `actual_counts`: the number of events (actual counts)
#'
#' @details `get_adaptive_baseline_dens()` assumes the poisson point process model and
#' calculates observed densities for each time period. It depends on `spatstat.model::mppm()`.
#' Users should note that the coefficients in the output are not directly interpretable,
#' since they are the coefficients inside the exponential of the poisson model.



get_adaptive_baseline_dens <- function(hfr_hist, hfr_current, dep_var, indep_var,
                              ngrid = 100, window) {

  
  # ---- 0) Align factor levels across hyperframes (important for predict) ----
  for (v in indep_var) {
    if (is.factor(hfr_hist[[v]])) {
      hfr_current[[v]] <- factor(hfr_current[[v]], levels = levels(hfr_hist[[v]]))
    }
  }
  
  # ---- 1) Fit on historical data ----
  text_form <- paste0(dep_var, " ~ ", paste(indep_var, collapse = " + "))
  message("Fitting baseline model on *historical* data...\n")
  mod <- spatstat.model::mppm(as.formula(text_form), data = hfr_hist)
  
  coefficients <- as.numeric(spatstat.model::summary.mppm(mod)$coef)
  
  # ---- 2) Predict CIF on the current hyperframe ----
  # Returns a list of im objects, one per row of hfr_current
  message("Predicting CIF on *current* data using the baseline model...\n")
  cif_current <- spatstat.model::predict.mppm(
    mod, type = "cif", ngrid = ngrid, newdata = hfr_current
  )$cif
  
  # ---- 3) Integrate CIF over the window -> expected counts for current periods ----
  message("Integrating CIF over the window for expected counts...\n")
  estimated_counts <- vapply(
    cif_current,
    FUN = function(img) integral(img, domain = window),
    FUN.VALUE = numeric(1)
  )
  
  # ---- 4) Sum of log intensity at current observed points ----
  # Evaluate each predicted CIF image at the actual current point pattern
  # and sum log(lambda) safely (0 if no points that period).
  message("Computing sum of log intensity at observed points (current data)...\n")
  
  pred <- spatstat.model::predict.mppm(
    mod,
    type = "cif",
    newdata = hfr_current,
    locations = hfr_current[[dep_var]]
  )

  # Extract numeric values at point locations
  intensity_of_each_obs <- lapply(pred$cif, spatstat.geom::marks)

  # Sum log intensities
  sum_log_intensity <- sapply(intensity_of_each_obs, function(x) sum(log(x)))

  
  
  # ---- 5) Actual counts for the current periods ----
  actual_counts <- unlist(purrr::map(hfr_current[[dep_var]], function(x) x$n))
  
  # ---- 6) Return object ----
  out <- list(
    indep_var          = indep_var,
    coef               = coefficients,
    intens_grid_cells  = cif_current,      # predicted CIF images on current data
    estimated_counts   = estimated_counts, # integrated CIF over window
    sum_log_intens     = sum_log_intensity,
    actual_counts      = actual_counts
  )
  class(out) <- c("obs", "list")
  return(out)
}
