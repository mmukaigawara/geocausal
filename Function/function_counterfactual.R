# Function: counterfactual

# This function does the following:
# 1. Fit a poisson process model
# 2. Fit a counterfactual model (at this moment, modify intensities only: baseline density x constant)
# 3. Return fitted hyperframes for the original and counterfactual distributions

# Specifications:
# outcome: an outcome variable
# covariates: a vector of names of covariates
# data: data to fit the poisson model
# newdata: data used for prediction; by default data = newdata
# counter: a constant to multiply the baseline density. By default = 1 and returns the original distribution
# scenario: "intensity" or "location"; if "intensity," this fx allows users to consider counterfactuals wrt intensity
#           if "location", it allows users to consider counterfactuals wrt locations (at this moment, intensity only)

counterfactual <- function(outcome, covariates, data,
                           newdata = data,
                           counter = 1,
                           multiple = TRUE,
                           scenario = "intensity"){
  
  # Diagnostics (data and counterfactual specifications) -----
  if (class(data)[1] != "hyperframe") {
    cat("Convert data to a hyperframe class object.")
  }
  
  if (counter <= 0){
    cat("Set counter > 0.")
  }
  
  # Formula -----
  ps_eq <- paste(outcome, "~", paste(ps_covs, collapse = "+"))
  
  # Fit the model (ppm or mppm) -----
  if (multiple == TRUE){
    ps_mod <- mppm(as.formula(ps_eq), data)
  } else if (multiple == FALSE){
    ps_mod <- ppm(as.formula(ps_eq), data)
  }
  
  # Actual and counterfactual intensities -----
  
  ## Conditional intensity with the actual distribution
  fitted_ps <- predict.mppm(ps_mod, type = "cif", newdata, ngrid = 100)$cif
  
  ## Conditional intensity with a counterfactual distribution (multiplied by counter)
  fitted_ps_counter <- counter * fitted_ps
  
  return(list(fitted_ps = fitted_ps,
              fitted_ps_counter = fitted_ps_counter)) # A list of two elements
    
}
