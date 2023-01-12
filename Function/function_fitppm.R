# Function: fitppm

# This function does the following:
# 1. Fit a poisson process model

# Specifications:
# outcome: an outcome variable
# covariates: a vector of names of covariates
# data: data to fit the poisson model
# multiple: choice of ppm or mppm

fitppm <- function(outcome, 
                   covariates, 
                   data,
                   multiple = TRUE){
  
  # Diagnostics (data) -----
  if (class(data)[1] != "hyperframe") {
    cat("Convert data to a hyperframe class object.")
  }
  
  # Formula -----
  ps_eq <- paste(outcome, "~", paste(ps_covs, collapse = "+"))
  
  # Fit the model (ppm or mppm) -----
  cat("Fitting the model to the original dataset.")
  
  if (multiple == TRUE){
    ps_mod <- mppm(as.formula(ps_eq), data)
  } else if (multiple == FALSE){
    ps_mod <- ppm(as.formula(ps_eq), data)
  }
  
  return(ps_mod)
  
}
