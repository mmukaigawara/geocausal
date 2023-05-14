#' Function: get_obs_density
#'
#' A function that takes a hyperframe and returns the observed densities
#' ie, propensity scores; the denominators of the equation
#'
#' @param hfr
#' @param treatment
#' @param rhs_list
#' @param ngrid
#' @param window

get_obs_density <- function(hfr, treatment, rhs_list, ngrid = 100, window) {

  # Define function -----
  text_form <- paste0(treatment, " ~ ", paste(rhs_list, collapse = " + "))
  cat("Fitting the model...\n")
  ps_mod <- mppm(as.formula(text_form), data = hfr) #Fit mppm
  ps_coefs <- as.numeric(summary(ps_mod)$coef) #Coefficients

  # Obtain fitted values of the propensity score -----
  cat("Calculating the intensity...\n")
  fitted_ps <- spatstat.model::predict.mppm(ps_mod, type = "cif", ngrid = ngrid)$cif #Returns intensity (cif) over nxn grid cells
  cat("Integrating the intensity to obtain the propensity score...\n")
  cif_integral <- sapply(fitted_ps, function(x) integral(x, domain = window)) #Integrate intensity over the window (so, e_t(w) for each date)
  cif_at_locs <- spatstat.model::fitted.mppm(ps_mod, dataonly = TRUE) #Return fitted cif for each observation for each date
  sum_log_cif <- sapply(cif_at_locs, function(x) { #Take the sum of the above for each date
    r <- 0
    if (length(x) > 0) r <- sum(log(x))
    return(r)
  })

  return(list(ps_covs = ps_covs, #List of covariates
              ps_coefs = ps_coefs, #Coefficients
              cif_integral = cif_integral, #Propensity scores
              sum_log_cif = sum_log_cif)) #Sum of log(intensity) for each time period

}
