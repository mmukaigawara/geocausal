#' Function: predict_obs_density
#'
#' A function that performs out-of-sample prediction (separating data into training and test sets)
#' (Note that this funciton assumes the same window between training and test sets)
#'
#' @param hfr
#' @param ratio The ratio between training and test sets
#' @param dep_var
#' @param indep_var
#' @param ngrid
#' @param window

predict_obs_density <- function(hfr, ratio,
                                dep_var, indep_var, ngrid = 100, window) {
  
  # Separate data into training and test sets
  training_row_max <- trunc(nrow(hfr)*ratio)
  
  hfr_train <- hfr[c(1:training_row_max), ] #Training set
  #hfr_test <- hfr[c(training_row_max+1:nrow(hfr)), ] #Test set

  # Define function -----
  text_form <- paste0(dep_var, " ~ ", paste(indep_var, collapse = " + "))
  cat("Fitting the model...\n")
  mod <- mppm(as.formula(text_form), data = hfr) #Fit mppm
  coefficients <- as.numeric(summary(mod)$coef) #Coefficients
  
  # Obtain fitted values of the propensity score -----
  cat("Calculating the intensity...\n")
  intensity_grid_cells <- spatstat.model::predict.mppm(mod, type = "cif", ngrid = ngrid)$cif #Returns intensity (cif) over nxn grid cells
  cat("Integrating the intensity to obtain the propensity score...\n")
  estimated_counts <- sapply(intensity_grid_cells, function(x) integral(x, domain = window)) #Integrate intensity over the window (so, e_t(w) for each date)
  intensity_of_each_obs <- spatstat.model::fitted.mppm(mod, dataonly = TRUE) #Return fitted cif for each observation for each date
  sum_log_intensity <- sapply(intensity_of_each_obs, function(x) { #Take the sum of the above for each date
    r <- 0
    if (length(x) > 0) r <- sum(log(x))
    return(r)
  })
  
  return(list(independent_variables = indep_var, #List of RHS variables
              coefficients = coefficients, #Coefficients
              intensity_grid_cells = intensity_grid_cells, #Integrated intensity as images
              estimated_counts = estimated_counts, #Counts
              sum_log_intensity = sum_log_intensity, #Sum of log(intensity) for each time period
              training_row_max = training_row_max)) #Max row ID of training data
  
}