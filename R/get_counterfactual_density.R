#' Function: get_counterfactual_density
#'
#' A function that takes the target number, baseline density, and power density,
#' and generates a hyperframe with point patterns
#'
#' @param counterfactual_type Either "intensity" or "location." If "intensity," it multiplies the baselin density by the target number. If "location," it shifts the focus location.
#' @param target_number The expected number of observations. If "location," it is treated as a constant.
#' @param baseline_density The baseline density (an im object)
#' @param power_density The power density (an im object)
#' @param window An owin object

get_counterfactual_density <- function(counterfactual_type,
                                       target_number,
                                       baseline_density,
                                       power_density,
                                       window) {
  
  if (counterfactual_type == "intensity") { # counterfactual_type = "intensity"
    
    counterfactual_density <- target_number * baseline_density
    
  } else if (counterfactual_type == "location") { # counterfactual_type = "location"
    
    product_power_baseline <- baseline_density * power_density
    counterfactual_density <- product_power_baseline/
      integral(product_power_baseline, W = iraq_window) * target_number
    
  }
  
  return(counterfactual_density)
  
}
