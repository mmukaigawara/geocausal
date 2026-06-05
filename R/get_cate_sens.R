#' Sensitivity analysis for conditional average treatment effects
#'
#' @description
#' `get_cate_sens()` evaluates how robust conditional average treatment
#' effect (CATE) estimates are to possible unmeasured confounding. The function
#' projects each period-specific weighted surface onto the same effect-modifier
#' basis used by `get_cate()`, and then checks whether each non-intercept basis
#' coefficient can be shifted to zero under each sensitivity parameter `gamma`.
#'
#' @param obs observed density
#' @param cf1 counterfactual density 1
#' @param cf2 counterfactual density 2
#' @param treat column of a hyperframe that summarizes treatment data. In the form of `hyperframe$column`.
#' @param pixel_count_out column of a hyperframe that summarizes the number of outcome events in each pixel
#' @param lag integer that specifies lags to calculate causal estimates.
#' @param trunc_level the level of truncation for the weights (0-1).
#' @param time_after whether to include one unit time difference between treatment and outcome. By default = TRUE
#' @param entire_window owin object (the entire region of interest)
#' @param em column of a hyperframe that summarizes the effect modifier data. In the form of `hyperframe$column`. It can be NULL if `E_mat` is provided.
#' @param E_mat optional covariate matrix (excluding the intercept) for the effect modifier.
#' @param nbase number of bases for splines
#' @param spline_type type of splines. Either `"ns"` or `"bs"`.
#' @param intercept whether to include intercept in the regression model. Default is TRUE.
#' @param eval_values currently unused; reserved for future CATE-value sensitivity output.
#' @param eval_mat currently unused; reserved for future CATE-value sensitivity output.
#' @param gamma_vals numeric vector of sensitivity parameters (>= 1) at which to check zero-attainability. By default, `seq(1.05, 1.2, by = 0.05)`.
#' @param save_weights whether to save weights. Default is `TRUE`
#' @param tol_slack numeric tolerance used to determine whether zero is attainable. Default is `1e-6`.
#' @param grid_init number of grid points used in each adaptive lambda search step. Default is 15.
#' @param max_refine maximum number of adaptive lambda refinement steps. Default is 5.
#' @param ... arguments passed onto the spline basis function
#'
#' @returns list of the following:
#' `beta`: a data frame with `basis`, `gamma`, and `zero_attainable` for the
#' non-intercept basis coefficients.
#' `robust_gamma`: the smallest coefficient-specific robust gamma across all
#' non-intercept basis coefficients.
#' `specification`: information about the spline basis, evaluated values, and
#' sensitivity parameters.
#'
#' @details
#' Unlike `get_sens()`, which returns lower and upper bounds for an average
#' treatment effect, `get_cate_sens()` checks zero-attainability for the
#' projected coefficients. For each `gamma`, `zero_attainable` indicates
#' whether the corresponding non-intercept basis coefficient is no longer
#' robust under the current allowance for unmeasured confounding. The
#' `robust_gamma` value summarizes the largest sensitivity level at which all
#' included coefficients remain robust. The underlying linear programs are
#' solved with `Rglpk::Rglpk_solve_LP()`.
#'
#' `gamma = 1` corresponds to no unmeasured confounding. Larger values allow
#' more deviation from the observed treatment process.
#'
#' @references
#' Zhou, L., Imai, K., Lyall, J. and Papadogeorgou, G. (2024). Estimating heterogeneous treatment effects for spatio-temporal causal inference: how economic assistance moderates the effects of airstrikes on insurgent violence. arXiv preprint. \doi{10.48550/arXiv.2412.15128}
#'
#' Mukaigawara, M., Imai, K., Lyall, J. and Papadogeorgou, G. (2025). Spatiotemporal causal inference with arbitrary spillover and carryover effects. arXiv preprint. \doi{10.48550/arXiv.2504.03464}
#'
#' @family sensitivity analysis functions
#'
#' @export
get_cate_sens <- function(obs, cf1, cf2, treat, pixel_count_out, lag,
                          trunc_level = 0.95, time_after = TRUE,
                          entire_window = NULL,
                          em = NULL, E_mat = NULL,
                          nbase = 6, spline_type = "ns",
                          intercept = TRUE,
                          eval_values = NULL, eval_mat = NULL,
                          gamma_vals = seq(1.05, 1.2, by = 0.05),
                          save_weights = TRUE,
                          tol_slack = 1e-6,
                          grid_init = 15,
                          max_refine = 5, ...) {
  E_mat_provided <- !is.null(E_mat)
  weights <- NULL

  if (is.null(em) & is.null(E_mat)) {
    stop("Both em and E_mat are null.")
  }

  if (any(is.na(gamma_vals)) || any(gamma_vals < 1)) {
    stop("gamma_vals must be a numeric vector with all values greater than or equal to 1")
  }

  if (is.null(nbase)) {
    nbase <- ncol(E_mat) + intercept
  }

  message("Get weighted surfaces... \n")
  estimates_1 <- get_weighted_surf(obs_dens = obs,
                                   cf_dens = cf1,
                                   treatment_data = treat,
                                   smoothed_outcome = pixel_count_out,
                                   mediation = FALSE, cate = TRUE,
                                   obs_med_log_sum_dens = NA,
                                   cf_med_log_sum_dens = NA,
                                   lag = lag, entire_window = entire_window,
                                   time_after,
                                   truncation_level = trunc_level)

  estimates_2 <- get_weighted_surf(obs_dens = obs,
                                   cf_dens = cf2,
                                   treatment_data = treat,
                                   smoothed_outcome = pixel_count_out,
                                   mediation = FALSE, cate = TRUE,
                                   obs_med_log_sum_dens = NA,
                                   cf_med_log_sum_dens = NA,
                                   lag = lag, entire_window = entire_window,
                                   time_after,
                                   truncation_level = trunc_level)

  dimyx <- dim(estimates_1$weighted_surface_arr_haj)[c(1, 2)]
  message("In the analysis, pixel grid dimension is ", dimyx[1], "x", dimyx[2], "\n")
  time_points <- dim(estimates_1$weighted_surface_arr_haj)[3]

  if (is.null(E_mat)) {
    message("Generate spline basis...\n")
    if ("im" %in% class(em[[1]])) {
      em <- lapply(seq_along(em), function(x) as.matrix(as.im(em[[x]], dimyx = dimyx)))
    } else if (!"array" %in% class(em[[1]])) {
      stop("em is not a list of im or 2D arrays")
    }

    if (length(em) == 1) {
      em <- lapply(seq_len(time_points), function(x) em[[1]])
    } else {
      em <- em[seq_len(time_points)]
    }

    if (spline_type == "ns") {
      E_mat <- splines::ns(unlist(em[seq_len(time_points)]), df = nbase - intercept, ...)
    } else if (spline_type == "bs") {
      E_mat <- splines::bs(unlist(em[seq_len(time_points)]), df = nbase - intercept, ...)
    } else {
      stop('spline_type must be either "ns" or "bs"')
    }
  }
  knots <- attr(E_mat, "knots")

  if (intercept) {
    E_mat <- cbind(1, E_mat)
  }

  message("Fit the model...\n")
  df <- cbind.data.frame(E = E_mat,
                         estimates1 = c(estimates_1$weighted_surface_arr_haj),
                         estimates2 = c(estimates_2$weighted_surface_arr_haj))
  weights <- rbind(estimates_1$weights, estimates_2$weights)

  df$est <- df$estimates2 - df$estimates1
  df$time <- sort(rep(seq_len(time_points), prod(dimyx)))
  df <- stats::na.omit(df)
  mean_effect <- mean(df$est, na.rm = TRUE)
  total_effect <- mean_effect * sum(df$time == 1)

  p <- ncol(E_mat)
  beta_arr <- array(NA_real_, c(time_points, 2 * p + 2))

  for (tt in seq_len(time_points)) {
    df_tt <- df[df$time == tt, -ncol(df)]

    tryCatch({
      Z <- as.matrix(df_tt[, -c(ncol(df_tt) - 0:2)])
      Q <- solve(t(Z) %*% Z)
      beta_arr[tt, ] <- c(Q %*% t(Z) %*% df_tt$estimates1,
                          Q %*% t(Z) %*% df_tt$estimates2,
                          weights[, tt])
    }, error = function(e) {
      message("Warning: covariate matrix is computationally singular for time period ", tt)
    })
  }

  message("Perform sensitivity analysis...\n")
  beta1 <- beta_arr[, seq_len(p), drop = FALSE]
  beta2 <- beta_arr[, (p + 1):(2 * p), drop = FALSE]

  beta_index <- if (intercept) {
    if (p < 2) integer(0) else seq(2, p)
  } else {
    seq_len(p)
  }
  npoints_beta <- length(beta_index)
  beta_names <- paste0("basis_", seq_len(npoints_beta))

  # CATE-value sensitivity is reserved for a future version. The beta-level
  # sensitivity below is the current package-facing output.
  # cate_results <- expand.grid(eval_values = eval_values,
  #                             gamma = gamma_vals,
  #                             KEEP.OUT.ATTRS = FALSE)
  # cate_results$zero_attainable <- FALSE

  beta_results <- expand.grid(basis = beta_names,
                              gamma = gamma_vals,
                              KEEP.OUT.ATTRS = FALSE)
  beta_results$zero_attainable <- FALSE

  # for (gg in seq_along(gamma_vals)) {
  #   this_gamma <- gamma_vals[gg]
  #   message("Start gamma: ", this_gamma)
  #
  #   lower_rho <- rep((1 / this_gamma)^lag, time_points)
  #   upper_rho <- rep(this_gamma^lag, time_points)
  #
  #   for (point in seq_len(npoints)) {
  #     A <- c(beta1 %*% eval_mat[point, ])
  #     B <- weights[1, ] / mean(weights[1, ])
  #     C <- c(beta2 %*% eval_mat[point, ])
  #     D <- weights[2, ] / mean(weights[2, ])
  #
  #     bounds1 <- sens_ratio_bounds(A, B, lower_rho, upper_rho)
  #     bounds2 <- sens_ratio_bounds(C, D, lower_rho, upper_rho)
  #
  #     lambda_lo <- max(bounds1["min"], bounds2["min"])
  #     lambda_hi <- min(bounds1["max"], bounds2["max"])
  #
  #     if (lambda_lo <= lambda_hi) {
  #       out <- sens_zero_attainability_search(lambda_lo = lambda_lo,
  #                                             lambda_hi = lambda_hi,
  #                                             A = A, B = B, C = C, D = D,
  #                                             lower_rho = lower_rho,
  #                                             upper_rho = upper_rho,
  #                                             tol_slack = tol_slack,
  #                                             grid_init = grid_init,
  #                                             max_refine = max_refine)
  #
  #       cate_results$zero_attainable[row_id] <- out$attainable
  #     }
  #   }
  # }

  for (gg in seq_along(gamma_vals)) {
    this_gamma <- gamma_vals[gg]
    message("Start gamma (beta): ", this_gamma)

    lower_rho <- rep((1 / this_gamma)^lag, time_points)
    upper_rho <- rep(this_gamma^lag, time_points)

    for (point in seq_along(beta_index)) {
      A <- c(beta1[, beta_index[point]])
      B <- weights[1, ] / mean(weights[1, ])
      C <- c(beta2[, beta_index[point]])
      D <- weights[2, ] / mean(weights[2, ])

      bounds1 <- sens_ratio_bounds(A, B, lower_rho, upper_rho)
      bounds2 <- sens_ratio_bounds(C, D, lower_rho, upper_rho)

      lambda_lo <- max(bounds1["min"], bounds2["min"])
      lambda_hi <- min(bounds1["max"], bounds2["max"])

      row_id <- (gg - 1) * npoints_beta + point

      if (lambda_lo <= lambda_hi) {
        out <- sens_zero_attainability_search(lambda_lo = lambda_lo,
                                              lambda_hi = lambda_hi,
                                              A = A, B = B, C = C, D = D,
                                              lower_rho = lower_rho,
                                              upper_rho = upper_rho,
                                              tol_slack = tol_slack,
                                              grid_init = grid_init,
                                              max_refine = max_refine)

        beta_results$zero_attainable[row_id] <- out$attainable
      }
    }
  }

  beta_robust_gamma <- sapply(beta_names, function(basis) {
    not_zero <- which(beta_results$basis == basis & !beta_results$zero_attainable)
    if (length(not_zero) > 0) max(beta_results$gamma[not_zero]) else NA_real_
  })
  robust_gamma <- if (all(is.na(beta_robust_gamma))) {
    NA_real_
  } else {
    min(beta_robust_gamma, na.rm = TRUE)
  }

  if (!E_mat_provided) {
    specification <- list(spline_type = spline_type,
                          intercept = intercept,
                          nbase = nbase,
                          knots = knots,
                          gamma_vals = gamma_vals,
                          tol_slack = tol_slack)
  } else {
    specification <- list(intercept = intercept,
                          gamma_vals = gamma_vals,
                          tol_slack = tol_slack)
  }

  cate_sens <- list(beta = beta_results,
                    robust_gamma = robust_gamma,
                    specification = specification,
                    mean_effect = mean_effect,
                    total_effect = total_effect,
                    weights = if (save_weights) weights else NULL)

  return(cate_sens)
}
