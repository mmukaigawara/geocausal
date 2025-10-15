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
  
  # ---- 2) Predict CIF on the *current* hyperframe ----
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
  
  # ---- 4) Sum of log intensity at *current* observed points ----
  # Evaluate each predicted CIF image at the actual current point pattern
  # and sum log(lambda) safely (0 if no points that period).
  message("Computing sum of log intensity at observed points (current data)...\n")
  sum_log_intensity <- purrr::map2_dbl(
    cif_current, hfr_current[[dep_var]],
    .f = function(img, pat) {
      if (is.null(pat) || spatstat.geom::npoints(pat) == 0) return(0)
      vals <- img[pat]                     # lookup CIF at event locations
      vals <- vals[is.finite(vals) & vals > 0]  # guard against NA/nonpositive
      if (length(vals) == 0) return(0)
      sum(log(vals))
    }
  )
  
  # ---- 5) Actual counts for the *current* periods ----
  actual_counts <- unlist(purrr::map(hfr_current[[dep_var]], function(x) x$n))
  
  # ---- 6) Return object similar to your obs object ----
  out <- list(
    indep_var          = indep_var,
    coef               = coefficients,
    intens_grid_cells  = cif_current,      # predicted CIF images on current data
    estimated_counts   = estimated_counts, # integrated CIF over window
    sum_log_intens     = sum_log_intensity,
    actual_counts      = actual_counts,
    model              = mod               # keep the fitted baseline model if needed
  )
  class(out) <- c("obs", "list")
  return(out)
}
