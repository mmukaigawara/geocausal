#' Function: predict_obs_density
#'
#' A function that performs out-of-sample prediction (separating data into training and test sets)
#' (Note that this funciton assumes the same window between training and test sets)
#'
#' @param hfr
#' @param ratio The ratio between training and test sets
#' @param treatment
#' @param rhs_list
#' @param ngrid
#' @param window

predict_obs_density <- function(hfr, ratio,
                                treatment, rhs_list, ngrid = 100, window) {
  
  # Separate data into training and test sets
  training_row_max <- trunc(nrow(hfr)*ratio)
  
  hfr_train <- hfr[c(1:training_row_max), ] #Training set
  #hfr_test <- hfr[c(training_row_max+1:nrow(hfr)), ] #Test set

  # Define function -----
  text_form <- paste0(treatment, " ~ ", paste(rhs_list, collapse = " + "))
  cat("Fitting the model...\n")
  ps_mod <- mppm(as.formula(text_form), data = hfr_train) #Fit mppm using the training set
  ps_coefs <- as.numeric(summary(ps_mod)$coef) #Coefficients
  
  # Obtain fitted values of the propensity score -----
  cat("Calculating the intensity...\n")
  fitted_ps <- spatstat.model::predict.mppm(ps_mod, type = "cif", newdata = hfr, ngrid = ngrid)$cif #Returns intensity (cif) over nxn grid cells
  cat("Integrating the intensity to obtain the propensity score...\n")
  cif_integral <- sapply(fitted_ps, function(x) integral(x, domain = window)) #Integrate intensity over the window (so, e_t(w) for each date)
  cif_at_locs <- spatstat.model::fitted.mppm(ps_mod, dataonly = TRUE) #Return fitted cif for each observation for each date
  sum_log_cif <- sapply(cif_at_locs, function(x) { #Take the sum of the above for each date
    r <- 0
    if (length(x) > 0) r <- sum(log(x))
    return(r)
  })
  
  return(list(ps_covs = ps_covs, #List of RHS variables
              ps_coefs = ps_coefs, #Coefficients
              fitted_ps = fitted_ps, #Integrated intensity as images
              cif_integral = cif_integral, #Counts
              sum_log_cif = sum_log_cif, #Sum of log(intensity) for each time period
              train_max_row = training_row_max)) #Max row ID of training set
  
}