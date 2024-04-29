#' Get causal estimates comparing two scenarios
#'
#' @description
#' `get_est()` generates causal estimates comparing two counterfactual scenarios.
#'
#' @param obs observed density
#' @param cf1 counterfactual density 1
#' @param cf2 counterfactual density 2
#' @param mediation whether to perform causal mediation analysis (don't use; still in development). By default, FALSE.
#' @param obs_med_log_sum_dens sum of log densities of mediators for the observed (don't use; still in development)
#' @param cf1_med_log_sum_dens sum of log densities of mediators for counterfactual 1 (don't use; still in development)
#' @param cf2_med_log_sum_dens sum of log densities of mediators for counterfactual 2 (don't use; still in development)
#' @param treat column of a hyperframe that summarizes treatment data. In the form of `hyperframe$column`.
#' @param sm_out column of a hyperframe that summarizes the smoothed outcome data
#' @param lag integer that specifies lags to calculate causal estimates
#' @param time_after whether to include one unit time difference between treatment and outcome. By default = TRUE
#' @param entire_window owin object (the entire region of interest)
#' @param use_dist whether to use distance-based maps. By default, TRUE
#' @param windows a list of owin objects (if `use_dist = FALSE`)
#' @param dist_map distance map (an im object, if `use_dist = TRUE`)
#' @param dist distances (a numeric vector within the max distance of `dist_map`)
#' @param trunc_level the level of truncation for the weights (0-1)
#' @param save_weights whether to save weights
#' @returns list of the following:
#' `cf1_ave_surf`: average weighted surface for scenario 1
#' `cf2_ave_surf`: average weighted surface for scenario 2
#' `est_cf`: estimated effects of each scenario
#' `est_causal`: estimated causal contrasts
#' `var_cf`: variance upper bounds for each scenario
#' `var_causal`: variance upper bounds for causal contrasts
#' `windows`: list of owin objects
#'
#' @details The level of truncation indicates the quantile of weights at which weights are truncated.
#' That is, if `trunc_level = 0.95`, then all weights are truncated at the 95 percentile of the weights.

get_est <- function(obs, cf1, cf2, treat, sm_out,
                    mediation = FALSE,
                    obs_med_log_sum_dens = NA,
                    cf1_med_log_sum_dens = NA,
                    cf2_med_log_sum_dens = NA,
                    lag,
                    time_after = TRUE,
                    entire_window,
                    use_dist,
                    windows,
                    dist_map,
                    dist,
                    trunc_level = NA,
                    save_weights = TRUE) {

  #1. Get average weighted surfaces for two counterfactuals -----

  message("Calculating the average weighted surfaces for two scenarios...\n")

  ## CF1
  estimates_1 <- get_weighted_surf(obs_dens = obs,
                                   cf_dens = cf1,
                                   treatment_data = treat,
                                   smoothed_outcome = sm_out,
                                   mediation = mediation,
                                   obs_med_log_sum_dens = obs_med_log_sum_dens,
                                   cf_med_log_sum_dens = cf1_med_log_sum_dens,
                                   lag = lag, entire_window = entire_window,
                                   time_after = time_after,
                                   truncation_level = trunc_level)

  ## CF2
  estimates_2 <- get_weighted_surf(obs_dens = obs,
                                   cf_dens = cf2,
                                   treatment_data = treat,
                                   smoothed_outcome = sm_out,
                                   mediation = mediation,
                                   obs_med_log_sum_dens = obs_med_log_sum_dens,
                                   cf_med_log_sum_dens = cf2_med_log_sum_dens,
                                   lag = lag, entire_window = entire_window,
                                   time_after = time_after,
                                   truncation_level = trunc_level)

  #2. Get estimates (contrast) -----
  message("Obtaining the causal contrast...\n")

  estimates <- get_estimates(weighted_surf_1 = estimates_1,
                             weighted_surf_2 = estimates_2,
                             use_dist = use_dist,
                             windows,
                             dist_map = dist_map,
                             dist = dist,
                             entire_window = entire_window)

  #3. Get variance upper bounds -----
  message("Obtaining the variance upper bounds...\n")

  var_bound <- get_var_bound(estimates)

  #4. Return output -----
  weights <- NULL
  if(save_weights)
    weights <- estimates$weights
  class(weights) <- weights
  
  out <- list(cf1_ave_surf = estimates_1$average_surf_haj,
              cf2_ave_surf = estimates_2$average_surf_haj,
              est_cf = estimates$est_haj,
              est_causal = estimates$est_tau_haj_cf2_vs_cf1,
              var_cf = var_bound$bound_haj,
              var_causal = var_bound$bound_tau_haj,
              windows = estimates$windows,
              weights = weights)
  
  class(out) <- c("est", "list")
  return(out)

}
