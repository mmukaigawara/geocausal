#' Generate weighted surfaces for sensitivity analysis
#'
#' @description A variant of `get_weighted_surf()` that retains per-period
#' Hajek weights for both the entire region and a window of interest,
#' so that downstream linear-programming bounds can be computed.
#'
#' @param obs_dens observed density
#' @param cf_dens counterfactual density
#' @param treatment_data column of a hyperframe that summarizes treatment data. In the form of `hyperframe$column`.
#' @param smoothed_outcome column of a hyperframe that summarizes the smoothed outcome data
#' @param lag integer that specifies lags to calculate causal estimates
#' @param entire_window owin object (the entire region of interest)
#' @param window owin object (the sub-window of interest for which bounds are computed)
#' @param time_after whether to include one unit time difference between treatment and outcome
#' @param truncation_level the level at which the weights are truncated (see `get_estimates()`)
#'
#' @returns list of the following:
#'      * `haj_wt_daily_out`: Hajek-weighted daily outcomes integrated over the window of interest
#'      * `haj_weights_window_of_interest`: per-period Hajek weights for the window of interest
#'      * `haj_weights_entire_window`: per-period Hajek weights for the entire window
#'
#' @details `sens_weighted_surf()` is an internal function used by `get_sens()`.

sens_weighted_surf <- function(obs_dens, cf_dens,
                               treatment_data,
                               smoothed_outcome,
                               lag,
                               entire_window,
                               window,
                               time_after,
                               truncation_level = 0.95) {

  # 1. Weight for the entire window (mostly same as get_weighted_surf) -----

  message("Calculating weights...\n")
  # 1-1. Log density
  counterfactual_sum_log <- get_cf_sum_log_intens(cf_dens = cf_dens,
                                                  treatment_data = treatment_data)
  observed_sum_log <- obs_dens$sum_log_intens

  # 1-2. Log density ratio (LDR)
  log_density_ratio <- obs_dens$estimated_counts -
    {
      if (!is.null(cf_dens$estimated_counts)) {
        cf_dens$estimated_counts
      } else if (spatstat.geom::is.im(cf_dens)) {
        spatstat.univar::integral(cf_dens, window = entire_window)
      } else {
        vapply(cf_dens, function(im_obj)
          spatstat.univar::integral(im_obj, window = entire_window),
          numeric(1)
        )
      }
    } +
    counterfactual_sum_log - observed_sum_log

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
  haj_weights_entire_window <- weights / mean(weights) # Hajek weights for the entire window

  # 2. Weighted smoothed outcome for a window of interest -----
  smoothed <- smoothed_outcome[(lag + time_after) : length(smoothed_outcome)] #Just smoothed outcomes, lag + 1 (if time_after) : nrow(data)

  # 2-1. Convert smoothed outcomes to arrays (pixels, for each time period)
  mat_im <- sapply(smoothed, function(x) spatstat.geom::as.matrix.im(x))
  pixels <- smoothed[[1]]$dim[1]
  mat_im <- array(mat_im, dim = c(pixels, pixels, length(smoothed)))

  # 2-2. Hajek-weighted smoothed outcomes for each time period
  mat_im_weighted_haj <- sweep(mat_im, MARGIN = 3, STATS = haj_weights_entire_window, FUN = '*')
  haj_weighted_surface <- lapply(1:dim(mat_im_weighted_haj)[3], function(x) spatstat.geom::as.im(mat_im_weighted_haj[, , x], W = entire_window))
  haj_weighted_daily_outcome <- sapply(haj_weighted_surface, function(x) integral(x, window)) # Integrated to obtain counts in a window of interest

  # 3. Weights for the window of interest (woi) -----

  message("Calculating weights for the window of interest...\n")

  # 3-1. Log density
  treatment_data_woi <- lapply(treatment_data, function(p) p[window])

  counterfactual_sum_log_woi <- get_cf_sum_log_intens(cf_dens = cf_dens,
                                                      treatment_data = treatment_data_woi)
  observed_sum_log_woi <- get_cf_sum_log_intens(cf_dens = obs_dens$intens_grid_cells,
                                                treatment_data = treatment_data_woi)

  # 3-2. Log density ratio of the window of interest (LDR)
  obs_estimated_counts_woi <- sapply(obs_dens$intens_grid_cells, function(img) integral(img, domain = window))

  log_density_ratio_woi <- obs_estimated_counts_woi -
    spatstat.univar::integral(cf_dens, window = window) +
    counterfactual_sum_log_woi - observed_sum_log_woi

  # 3-3. Convert LDR to weights (weights of the window of interest for each time period)
  weights_woi <- furrr::future_map_dbl(lag : length(log_density_ratio_woi), function(x) {
    weight <- exp(sum(log_density_ratio_woi[(x - lag + 1): x]))
    return(weight)
  })

  stabilizer_woi <- furrr::future_map_dbl(1 : length(log_density_ratio_woi), function(x) {
    weight <- exp(sum(log_density_ratio_woi[x]))
    return(weight)
  })
  if (!is.null(truncation_level)) { #Truncation of weights
    truncate_at_woi <- quantile(stabilizer_woi, probs = truncation_level)
  }
  stabilizer_woi <- mean(sapply(stabilizer_woi, function(x) min(x, truncate_at_woi)))^lag

  if (!is.null(truncation_level)) { #Truncation of weights
    truncate_at <- quantile(weights_woi, probs = truncation_level)
    weights_woi <- sapply(weights_woi, function(x) min(x, truncate_at))
  }

  weights_woi <- weights_woi[1 : (length(weights_woi) - time_after)] #If time_after = TRUE, then lag : nrow(data)-1
  haj_weights_woi <- weights_woi / mean(weights_woi) # Hajek weights for the woi (divide by the mean of the weights of the WOI window)

  return(list(haj_wt_daily_out = haj_weighted_daily_outcome,
              haj_weights_window_of_interest = haj_weights_woi,
              haj_weights_entire_window = haj_weights_entire_window))

}

#' Solve linear program for sensitivity bounds at a given gamma
#'
#' @description Solves the pair of linear programs that yield the upper and
#' lower bounds of the Hajek-weighted estimand under a given sensitivity
#' parameter `gamma`. Used internally by `get_sens()`.
#'
#' @param wto list whose first element is the numerator (daily Hajek-weighted
#' outcomes) and whose second element is the denominator (daily Hajek weights
#' for the window of interest), as returned by `sens_weighted_surf()`.
#' @param this_gamma sensitivity parameter (>= 1). `gamma = 1` corresponds to
#' no unmeasured confounding.
#'
#' @returns a tibble with columns `high` and `low` giving the upper and lower
#' bounds of the estimand at `this_gamma`.
#'
#' @details `get_linear_prog()` is an internal function to `get_sens()`.
#' It relies on `Rglpk::Rglpk_solve_LP()` to solve the transformed linear program.

get_linear_prog <- function(wto, this_gamma) {

  obj_num1 <- wto[[1]]
  obj_den1 <- wto[[2]]
  D_mat = diag(length(wto[[1]]))
  time_points = length(wto[[1]])

  mat <- rbind(D_mat, - D_mat)
  dir <- rep('<=', 2 * time_points)
  rhs <- c(rep(this_gamma, time_points), rep(- (1 / this_gamma), time_points)) # c(1, 1, ..., 1, -1, -1, ...-1)

  # Creating the transformed linear problem:
  new_obj <- c(obj_num1, 0) # daily wt'd outcome; the last is 0

  new_mat <- cbind(mat, - rhs)
  new_dir <- rep('<=', 2 * time_points)
  new_rhs <- rep(0, 2 * time_points)

  new_mat <- rbind(new_mat, c(obj_den1, 0))
  new_dir <- c(new_dir, '==')
  new_rhs <- c(new_rhs, 1)

  new_mat <- rbind(new_mat, c(rep(0, ncol(new_mat) - 1), 1))
  new_dir <- c(new_dir, '>=')
  new_rhs <- c(new_rhs, 0)

  lp_sol2_max <- Rglpk::Rglpk_solve_LP(obj = new_obj, new_mat, new_dir, new_rhs, max = TRUE, verbose = FALSE)
  lp_sol2_min <- Rglpk::Rglpk_solve_LP(obj = new_obj, new_mat, new_dir, new_rhs, max = FALSE, verbose = FALSE)

  cf1_low <- lp_sol2_min$optimum # gg=gamma, ii=intervention, bb=location
  cf1_up <- lp_sol2_max$optimum

  return(tibble::tibble(high = cf1_up, low = cf1_low))

}

#' Sensitivity analysis for counterfactual contrasts
#'
#' @description
#' `get_sens()` computes bounds on the causal contrast between two
#' counterfactual densities across a grid of sensitivity parameters `gamma`.
#' At each value of `gamma`, linear programming is used to obtain worst-case
#' upper and lower bounds of the Hajek-weighted estimand for each scenario;
#' the returned bounds describe how robust the causal contrast is to possible
#' violations of the no-unmeasured-confounding assumption.
#'
#' @param obs observed density
#' @param cf1 counterfactual density 1
#' @param cf2 counterfactual density 2
#' @param treat column of a hyperframe that summarizes treatment data. In the form of `hyperframe$column`.
#' @param sm_out column of a hyperframe that summarizes the smoothed outcome data
#' @param lag integer that specifies lags to calculate causal estimates
#' @param entire_window owin object (the entire region of interest)
#' @param window owin object (the sub-window over which the contrast is evaluated)
#' @param gamma_vals numeric vector of sensitivity parameters (>= 1) at which to compute bounds. By default, `seq(1, 1.2, by = 0.01)`.
#' @param time_after whether to include one unit time difference between treatment and outcome. By default = TRUE
#' @param trunc_level the level of truncation for the weights (0-1)
#' @param tol_slack numeric tolerance used to determine whether zero is attainable. Default is `1e-6`.
#' @param grid_init number of grid points used in each adaptive lambda search step. Default is 15.
#' @param max_refine maximum number of adaptive lambda refinement steps. Default is 5.
#'
#' @returns a tibble with columns:
#'      * `gamma`: the sensitivity parameter
#'      * `lb`: lower bound on the causal contrast at `gamma`
#'      * `ub`: upper bound on the causal contrast at `gamma`
#'      * `zero_attainable`: whether zero is attainable at `gamma`
#'      * `robust_gamma`: the largest gamma at which zero is not attainable
#'
#' @details `gamma = 1` corresponds to no unmeasured confounding and recovers
#' point estimates analogous to those of `get_est()`. As `gamma` increases, the
#' bounds widen, reflecting greater allowance for unobserved confounding.
#' The `lb` and `ub` columns retain the conservative bounds from the original
#' implementation. The `zero_attainable` column indicates whether the result is
#' no longer robust under the current value of `gamma`, or equivalently under
#' the current allowance for unmeasured confounding. The underlying linear
#' programs are solved with
#' `Rglpk::Rglpk_solve_LP()`.
#'
#' This function performs its computation using the `future` framework
#' (via `furrr`); to run it in parallel, set `future::plan(future::multisession)`
#' before calling this function. The results are identical regardless of the plan.
#'
#' @references
#' Papadogeorgou, G., Imai, K., Lyall, J. and Li, F. (2022). Causal inference with spatio-temporal data: estimating the effects of airstrikes on insurgent violence in Iraq. \emph{Journal of the Royal Statistical Society Series B}, 84(5), 1969--1999. \doi{10.1111/rssb.12548}
#'
#' Mukaigawara, M., Imai, K., Lyall, J. and Papadogeorgou, G. (2025). Spatiotemporal causal inference with arbitrary spillover and carryover effects. arXiv preprint. \doi{10.48550/arXiv.2504.03464}
#'
#' @family sensitivity analysis functions
#'
#' @export

get_sens <- function(obs, cf1, cf2,
                     treat,
                     sm_out,
                     lag,
                     entire_window,
                     window,
                     gamma_vals = seq(1, 1.2, by = 0.01),
                     time_after = TRUE,
                     trunc_level = NA,
                     tol_slack = 1e-6,
                     grid_init = 15,
                     max_refine = 5) {

  sens1 <- sens_weighted_surf(obs_dens = obs, cf_dens = cf1,
                              treatment_data = treat, smoothed_outcome = sm_out,
                              lag = lag, entire_window = entire_window,
                              window = window, time_after = time_after,
                              truncation_level = trunc_level)

  sens2 <- sens_weighted_surf(obs_dens = obs, cf_dens = cf2,
                              treatment_data = treat, smoothed_outcome = sm_out,
                              lag = lag, entire_window = entire_window,
                              window = window, time_after = time_after,
                              truncation_level = trunc_level)

  bounds <- tibble::tibble(gamma = gamma_vals,
                           lb = NA_real_,
                           ub = NA_real_,
                           zero_attainable = FALSE,
                           robust_gamma = NA_real_)

  for (gg in 1 : length(gamma_vals)) {

    this_gamma <- gamma_vals[gg]
    bounds_cf1 <- get_linear_prog(sens1, this_gamma = this_gamma)
    bounds_cf2 <- get_linear_prog(sens2, this_gamma = this_gamma)

    lb = bounds_cf2$low - bounds_cf1$high
    ub = bounds_cf2$high - bounds_cf1$low

    bounds[gg, "lb"] <- lb
    bounds[gg, "ub"] <- ub

    lambda_lo <- max(bounds_cf1$low, bounds_cf2$low)
    lambda_hi <- min(bounds_cf1$high, bounds_cf2$high)

    if (lambda_lo <= lambda_hi) {
      lower_rho <- rep(1 / this_gamma, length(sens1[[1]]))
      upper_rho <- rep(this_gamma, length(sens1[[1]]))

      out <- sens_zero_attainability_search(lambda_lo = lambda_lo,
                                            lambda_hi = lambda_hi,
                                            A = sens1[[1]],
                                            B = sens1[[2]],
                                            C = sens2[[1]],
                                            D = sens2[[2]],
                                            lower_rho = lower_rho,
                                            upper_rho = upper_rho,
                                            tol_slack = tol_slack,
                                            grid_init = grid_init,
                                            max_refine = max_refine)

      bounds[gg, "zero_attainable"] <- out$attainable
    }

  }

  not_zero <- which(!bounds$zero_attainable)
  if (length(not_zero) > 0) {
    bounds$robust_gamma <- bounds$gamma[max(not_zero)]
  }

  bounds

}
