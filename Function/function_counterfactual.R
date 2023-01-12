# Function: counterfactual

# This function does the following:
# 1. Fit a counterfactual model (at this moment, modify intensities only: baseline density x constant)
# 2. Return fitted hyperframes for the original and counterfactual distributions

# Specifications:
# data: data to fit the poisson model
# newdata: data used for prediction
# counter: a constant to multiply the baseline density. By default = 1 and returns the original distribution
# multiple: choice of ppm or mppm
# scenario: "intensity" or "location"; if "intensity," this fx allows users to consider counterfactuals wrt intensity
#           if "location", it allows users to consider counterfactuals wrt locations (at this moment, intensity only)

counterfactual <- function(ps_mod, 
                           data,
                           newdata,
                           counter = counter,
                           multiple = TRUE,
                           scenario = "intensity"){
  
  # Diagnostics (counterfactual specifications) -----
  if (counter <= 0){
    cat("Set counter > 0.")
  }
  
  # Actual and counterfactual intensities -----
  
  ## Conditional intensity with the actual distribution
  cat("Obtaining the conditional intensity.")
  
  if (multiple == TRUE){
    fitted_ps <- predict.mppm(ps_mod, type = "cif", newdata, ngrid = 100)$cif
  } else if (multiple == FALSE){
    fitted_ps <- predict.ppm(ps_mod, type = "cif", newdata, ngrid = 100)$cif
  }

  ## Conditional intensity with a counterfactual distribution (multiplied by counter)
  if (counter == 1){ #If c = 1, just return fitted_ps
    
    return(list(fitted_ps = fitted_ps))
    
  } else if (counter > 1){ #If not, obtain a counterfactual distribution and return a list
    
    fitted_ps_counter <- counter * fitted_ps
    return(list(fitted_ps = fitted_ps,
                fitted_ps_counter = fitted_ps_counter)) # A list of two elements
  }
  
}
