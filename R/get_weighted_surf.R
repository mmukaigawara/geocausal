#' Generate average weighted surfaces
#'
#' @description A function that returns averaged weighted surfaces (both IPW and Hajek) along with weights
#'
#' @param obs_dens observed density
#' @param cf_dens counterfactual density
#' @param mediation whether to perform causal mediation analysis. By default, FALSE.
#' @param cate whether to perform the heterogeneity analysis. By default, FALSE.
#' @param obs_med_log_sum_dens sum of log densities of mediators for the observed (if `mediation = TRUE`)
#' @param cf_med_log_sum_dens sum of log densities of mediators for counterfactual (if `mediation = TRUE`)
#' @param treatment_data column of a hyperframe that summarizes treatment data. In the form of `hyperframe$column`.
#' @param smoothed_outcome column of a hyperframe that summarizes the smoothed outcome data
#' @param lag integer that specifies lags to calculate causal estimates
#' @param entire_window owin object (the entire region of interest)
#' @param truncation_level the level at which the weights are truncated (see `get_estimates()`)
#' @param time_after whether to include one unit time difference between treatment and outcome
#'
#' @returns list of an average weighted surface (`avarage_surf`, an `im` object),
#' a Hajek average weighted surface (`average_weighted_surf_haj`, an `im` object),
#' weights, and smoothed outcomes
#'
#' @details `get_weighted_surf()` is an internal function to `get_estimates()` function.
#' If `time_after` is TRUE, then this function uses treatment data and weights from lag to nrow(data)-1, and
#' outcome data from lag+1 to nrow(data).

get_weighted_surf <- function(obs_dens, cf_dens,
                              mediation = FALSE,
                              cate = FALSE,
                              obs_med_log_sum_dens, cf_med_log_sum_dens,
                              treatment_data,
                              smoothed_outcome,
                              lag,
                              entire_window,
                              time_after,
                              truncation_level = truncation_level) {

  # 1. Weight

  message("Calculating weights...\n")

  # 1-1. Log density
  if (class(cf_dens)[1] == "im") { #get_cf_dens returns class c("dens", "im")
    counterfactual_sum_log <- get_cf_sum_log_intens(cf_dens = cf_dens,
                                                    treatment_data = treatment_data)
  }
  else {
    counterfactual_sum_log <- cf_dens$sum_log_intens
  }
  if (class(obs_dens)[1] == "im") {
    observed_sum_log <- get_cf_sum_log_intens(cf_dens = obs_dens$density,
                                              treatment_data = treatment_data)
  }
  else {
    observed_sum_log <- obs_dens$sum_log_intens
  }

  # 1-2. Log density ratio (LDR)
  if (mediation) {
    # For causal mediation analysis
    log_density_ratio <- counterfactual_sum_log + cf_med_log_sum_dens -
      observed_sum_log - obs_med_log_sum_dens # Added conditionals
  } else {
    # For causal inference without mediation
    log_density_ratio <- obs_dens$estimated_counts - spatstat.univar::integral(cf_dens, window = entire_window) + # Expected counts
      counterfactual_sum_log - observed_sum_log # Sum log intens
  }

  # 1-3. Convert LDR to weights (weights for each time period)
  weights <- furrr::future_map_dbl(lag : length(log_density_ratio), function(x) {
    weight <- exp(sum(log_density_ratio[(x - lag + 1): x]))
    return(weight)
  })
  

  stabilizer <- furrr::future_map_dbl(1 : length(log_density_ratio), function(x) {
    weight <- exp(sum(log_density_ratio[x]))
    return(weight)
  })
  if (!is.null(truncation_level)) { #Truncation of weights
    truncate_at <- quantile(stabilizer, probs = truncation_level)
  }
  stabilizer <- mean(sapply(stabilizer, function(x) min(x, truncate_at)))^lag
  

  if (!is.null(truncation_level)) { #Truncation of weights
    truncate_at <- quantile(weights, probs = truncation_level)
    weights <- sapply(weights, function(x) min(x, truncate_at))
  }

  weights <- weights[1 : (length(weights) - time_after)] #If time_after = TRUE, then lag : nrow(data)-1

  # 2. Weighted smoothed outcome
  smoothed <- smoothed_outcome[(lag + time_after) : length(smoothed_outcome)] #Just smoothed outcomes, lag + 1 (if time_after) : nrow(data)

  # 2-1. Convert smoothed outcomes to arrays (pixels, for each time period)
  mat_im <- sapply(smoothed, function(x) spatstat.geom::as.matrix.im(x))
  pixels <- smoothed[[1]]$dim[1]
  mat_im <- array(mat_im, dim = c(pixels, pixels, length(smoothed)))

  # 2-2. Weight smoothed outcomes for each time period by the weights (smoothed outcome x weight)
  mat_im_weighted <- sweep(mat_im, MARGIN = 3, STATS = weights, FUN = '*') #Weighted smoothed outcomes as matrices

  # 2-3. Get average weighted surface
  average_weighted_surface <- spatstat.geom::as.im(apply(mat_im_weighted, c(1, 2), mean),
                                                   W = entire_window) #This is IPW; one pixel image
  average_weighted_surface_haj <- average_weighted_surface / mean(weights) #Hajek; one pixel image
  
  if(cate){ 
    
    return(list(weighted_surface_arr = mat_im_weighted,
                weighted_surface_arr_haj = mat_im_weighted/mean(weights),
                weights = weights,
                stabilizer = stabilizer))
  }

  return(list(average_surf = average_weighted_surface,
              average_surf_haj = average_weighted_surface_haj,
              weights = weights,
              stabilizer = stabilizer,
              smoothed_outcome = smoothed)) #Note that first lag-1 obs are omitted from the smoothed outcome
}
