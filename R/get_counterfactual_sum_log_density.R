#' Function: get_counterfactual_sum_log_intensity
#'
#' A function that takes a hyperframe and returns the log counterfactual densities
#' ie, the numerator of the equation
#'
#' @param counterfactual_density A counterfactual density (an im object)
#' @param treatment_data In the form of hyperframe$column

get_counterfactual_sum_log_intensity <- function(counterfactual_density,
                                                 treatment_data) {
  
  intensity_of_each_obs <- lapply(1:length(treatment_data), function(t) {
    spatstat.geom::interp.im(counterfactual_density, 
                             x = treatment_data[[t]],
                             bilinear = TRUE)
  })
  
  sum_log_intensity <- sapply(intensity_of_each_obs, function(x) { #Take the sum of the above for each date
    r <- 0
    if (length(x) > 0) r <- sum(log(x))
    return(r)
  })
  
  return(sum_log_intensity)
}