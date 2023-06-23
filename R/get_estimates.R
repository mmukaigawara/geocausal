#' Function: get_estimates
#'
#' A function that takes a list of power densities
#' and returns simulated counterfactual densities
#'
#' @param counterfactual_simulation_results An output of the simulate_counterfactual_density function
#' @param density_of_interest A density that is being manipulated
#' @param entire_window An owin object (the entire region)
#' @param gray_scale By default, FALSE
#' @param expectation_use_raw Whether to use the actual expectation or proportion for the y-axis. By default, FALSE

get_estimates <- function(observed_density, 
                          counterfactual_density,
                          treatment_data, 
                          smoothed_outcome, 
                          lag,
                          entire_window, 
                          window_of_interest) {
  
  # 1. Weight
  
  # 1-1. Log density
  counterfactual_sum_log <- get_counterfactual_sum_log_intensity(counterfactual_density = counterfactual_density,
                                                                 treatment_data = treatment_data)
  
  observed_sum_log <- observed_density$sum_log_intensity
  
  # 1-2. Log density ratio (LDR)
  log_density_ratio <- counterfactual_sum_log - observed_sum_log
  
  # 1-3. Convert LDR to weights
  weights <- furrr::future_map_dbl((lag + 1):length(log_density_ratio), function(x) {
    weight <- exp(sum(log_density_ratio[(x - lag + 1): x]))
    return(weight)
  })
  
  # 2. Weighted smoothed outcome
  smoothed <- smoothed_outcome[(lag + 1):length(smoothed_outcome)]
  
  # 2-1. Convert smoothed outcomes to arrays (pixels, for each time period)
  mat_im <- sapply(smoothed, function(x) spatstat.geom::as.matrix.im(x))
  pixels <- smoothed[[1]]$dim[1]
  mat_im <- array(mat_im, dim = c(pixels, pixels, length(smoothed)))
  
  # 2-2. Weigh each time period by the weights
  mat_im_weighted <- sweep(mat_im, MARGIN = 3, STATS = weights, FUN = '*') #Multiply by weights
  
  # 2-3. Get average weighted densities
  # Note: Don't use IPW. This funciton only returns Hajek extimators.
  average_weighted_surface <- spatstat.geom::as.im(apply(mat_im_weighted, c(1, 2), mean), 
                                                   W = entire_window) #This is IPW
  average_weighted_surface_haj <- average_weighted_surface / mean(weights) #Hajek
  
  # 3. Integrate over the window of interest
  average_expected_events_haj <- spatstat.geom::integral(average_weighted_surface_haj, window = window_of_interest)
  
  return(list(average_weighted_density = average_weighted_surface_haj,
              average_expected_events = average_expected_events_haj,
              weights = weights))
  
}