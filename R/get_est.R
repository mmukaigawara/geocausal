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
#'
#' @references
#' Papadogeorgou, G., Imai, K., Lyall, J. and Li, F. (2022). Causal inference with spatio-temporal data: estimating the effects of airstrikes on insurgent violence in Iraq. \emph{Journal of the Royal Statistical Society Series B}, 84(5), 1969--1999. \doi{10.1111/rssb.12548}
#'
#' Mukaigawara, M., Imai, K., Lyall, J. and Papadogeorgou, G. (2025). Spatiotemporal causal inference with arbitrary spillover and carryover effects. arXiv preprint. \doi{10.48550/arXiv.2504.03464}
#'
#' @seealso [get_obs_dens()], [get_cf_dens()], [get_sens()]
#'
#' @family causal effect estimation functions
#'
#' @examples
#' \donttest{
#' # Prepare data: airstrikes (treatment) and insurgencies (outcome), Iraq 2006
#' dat <- rbind(airstrikes_2006[airstrikes_2006$type == "Airstrike", ],
#'              insurgencies_2006)
#' dat$type <- ifelse(dat$type == "Airstrike", "airstrike", "insurgency")
#' dat$time <- as.numeric(dat$date - min(dat$date) + 1)
#' dat <- dat[dat$time <= 60, ]
#' hfr <- get_hfr(data = dat, col = "type", window = iraq_window,
#'                time_col = "time", time_range = c(1, 60),
#'                coordinates = c("longitude", "latitude"), combine = FALSE)
#' dist_baghdad <- get_dist_focus(window = iraq_window, lon = 44.366,
#'                                lat = 33.315, ndim = 64)
#' hfr$dist_bag <- rep(list(dist_baghdad), nrow(hfr))
#'
#' # Observed density (propensity score) and counterfactual densities
#' obs <- get_obs_dens(hfr, dep_var = "airstrike", indep_var = "dist_bag",
#'                     ndim = 64, window = iraq_window)
#' base <- get_base_dens(window = iraq_window, ndim = 64,
#'                       out_data = insurgencies,
#'                       out_coordinates = c("longitude", "latitude"))
#' cf1 <- get_cf_dens(expected_number = 2, base_dens = base, window = iraq_window)
#' cf2 <- get_cf_dens(expected_number = 4, base_dens = base, window = iraq_window)
#'
#' # Smoothed outcomes
#' hfr$sm_insurgency <- smooth_ppp(hfr$insurgency, method = "abramson", ndim = 64)
#'
#' # Causal estimates comparing the two scenarios
#' est <- get_est(obs = obs, cf1 = cf1, cf2 = cf2,
#'                treat = hfr$airstrike, sm_out = hfr$sm_insurgency,
#'                lag = 1, entire_window = iraq_window,
#'                use_dist = TRUE, dist_map = dist_baghdad,
#'                dist = c(100, 200, 300), trunc_level = 0.95)
#' }

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
