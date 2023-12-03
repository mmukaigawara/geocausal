#' Generate average weighted surfaces
#'
#' @description A function that returns averaged weighted surfaces (both IPW and Hajek) along with weights
#'
#' @param obs_dens observed density
#' @param cf_dens counterfactual density
#' @param mediation whether to perform causal mediation analysis. By default, FALSE.
#' @param obs_med_log_sum_dens sum of log densities of mediators for the observed (if `mediation = TRUE`)
#' @param cf_med_log_sum_dens sum of log densities of mediators for counterfactual (if `mediation = TRUE`)
#' @param treatment_data column of a hyperframe that summarizes treatment data. In the form of `hyperframe$column`.
#' @param smoothed_outcome column of a hyperframe that summarizes the smoothed outcome data
#' @param lag integer that specifies lags to calculate causal estimates
#' @param entire_window owin object (the entire region of interest)
#' @param truncation_level The level at which the weights are truncated (see `get_estimates()`)
#'
#' @returns list of an average weighted surface (`avarage_surf`, an `im` object),
#' a Hajek average weighted surface (`average_weighted_surf_haj`, an `im` object),
#' weights, and smoothed outcomes
#'
#' @details `get_weighted_surf()` is an internal function to `get_estimates()` function

get_weighted_surf <- function(obs_dens, cf_dens,
                              mediation = FALSE,
                              obs_med_log_sum_dens, cf_med_log_sum_dens,
                              treatment_data,
                              smoothed_outcome,
                              lag,
                              entire_window,
                              truncation_level = truncation_level) {

  # 1. Weight

  message("Calculating weights...\n")

  # 1-1. Log density
  if(class(cf_dens[[1]])[1] == "im") {
    # If typical cf density (ie, images), then get sum log intens
    counterfactual_sum_log <- get_cf_sum_log_intens(cf_dens = cf_dens$density,
                                                    treatment_data = treatment_data)
  } else {
    # Otherwise use the calculated one as it is
    counterfactual_sum_log <- cf_dens$sum_log_intens
  }

  if(class(obs_dens[[1]])[1] == "im") {
    # If typical cf density (ie, images), then get sum log intens
    observed_sum_log <- get_cf_sum_log_intens(cf_dens = obs_dens$density,
                                              treatment_data = treatment_data)
  } else {
    # Otherwise use the calculated one as it is
    observed_sum_log <- obs_dens$sum_log_intens
  }

  # 1-2. Log density ratio (LDR)
  if (mediation) {
    # For causal mediation analysis
    log_density_ratio <- counterfactual_sum_log + cf_med_log_sum_dens -
      observed_sum_log - obs_med_log_sum_dens # Added conditionals
  } else {
    # For causal inference without mediation
    log_density_ratio <- obs_dens$estimated_counts - cf_dens$estimated_counts + # Expected counts
      counterfactual_sum_log - observed_sum_log # Sum log intens
  }

  # 1-3. Convert LDR to weights (weights for each time period)
  weights <- furrr::future_map_dbl((lag + 1):length(log_density_ratio), function(x) {
    weight <- exp(sum(log_density_ratio[(x - lag + 1): x]))
    return(weight)
  })

  if (!is.null(truncation_level)) { #Truncation of weights
    truncate_at <- quantile(weights, probs = truncation_level)
    weights <- sapply(weights, function(x) min(x, truncate_at))
  }

  # 2. Weighted smoothed outcome
  smoothed <- smoothed_outcome[(lag + 1):length(smoothed_outcome)] #Just smoothed outcomes

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

  return(list(average_surf = average_weighted_surface,
              average_surf_haj = average_weighted_surface_haj,
              weights = weights,
              smoothed_outcome = smoothed)) #Note that first #lag obs are omitted from the smoothed outcome
}
