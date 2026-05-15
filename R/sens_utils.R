# Utility functions for sensitivity analyses. These are intentionally shared
# across CATE and ATE sensitivity code paths.

sens_ratio_bounds <- function(obj_num, obj_den, lower_rho, upper_rho) {
  time_points <- length(obj_num)
  D_mat <- diag(time_points)

  mat <- rbind(D_mat, -D_mat)
  rhs <- c(upper_rho, -lower_rho)

  new_obj <- c(obj_num, 0)
  new_mat <- cbind(mat, -rhs)
  new_dir <- rep("<=", 2 * time_points)
  new_rhs <- rep(0, 2 * time_points)

  new_mat <- rbind(new_mat, c(obj_den, 0))
  new_dir <- c(new_dir, "==")
  new_rhs <- c(new_rhs, 1)

  new_mat <- rbind(new_mat, c(rep(0, ncol(new_mat) - 1), 1))
  new_dir <- c(new_dir, ">=")
  new_rhs <- c(new_rhs, 0)

  lp_max <- Rglpk::Rglpk_solve_LP(obj = new_obj, mat = new_mat, dir = new_dir,
                                  rhs = new_rhs, max = TRUE, verbose = FALSE)
  lp_min <- Rglpk::Rglpk_solve_LP(obj = new_obj, mat = new_mat, dir = new_dir,
                                  rhs = new_rhs, max = FALSE, verbose = FALSE)

  c(min = lp_min$optimum, max = lp_max$optimum)
}

sens_lambda_slack <- function(lambda, A, B, C, D, lower_rho, upper_rho,
                              eps_denom = 1e-8) {
  time_points <- length(A)
  obj <- c(rep(0, time_points), 1)

  v1 <- A - lambda * B
  v2 <- C - lambda * D

  mat <- rbind(
    c(v1, -1),
    c(-v1, -1),
    c(v2, -1),
    c(-v2, -1),
    cbind(diag(time_points), rep(0, time_points)),
    cbind(-diag(time_points), rep(0, time_points)),
    c(B, 0),
    c(D, 0),
    c(rep(0, time_points), 1)
  )

  dir <- c("<=", "<=", "<=", "<=",
           rep("<=", time_points),
           rep("<=", time_points),
           ">=", ">=", ">=")

  rhs <- c(0, 0, 0, 0,
           upper_rho,
           -lower_rho,
           eps_denom, eps_denom,
           0)

  sol <- Rglpk::Rglpk_solve_LP(obj = obj, mat = mat, dir = dir, rhs = rhs,
                               max = FALSE, verbose = FALSE)

  if (sol$status != 0) {
    return(list(status = sol$status,
                optimum = Inf,
                rho = NULL,
                s = Inf,
                residual1 = NA_real_,
                residual2 = NA_real_,
                max_residual = NA_real_))
  }

  rho_sol <- sol$solution[seq_len(time_points)]
  s_sol <- sol$solution[time_points + 1]
  residual1 <- sum((A - lambda * B) * rho_sol)
  residual2 <- sum((C - lambda * D) * rho_sol)
  max_residual <- max(abs(residual1), abs(residual2))

  list(status = sol$status,
       optimum = sol$optimum,
       rho = rho_sol,
       s = s_sol,
       residual1 = residual1,
       residual2 = residual2,
       max_residual = max_residual)
}

sens_zero_attainability_search <- function(lambda_lo, lambda_hi, A, B, C, D,
                                           lower_rho, upper_rho,
                                           tol_slack = 1e-6,
                                           grid_init = 15,
                                           max_refine = 5) {
  if (lambda_lo > lambda_hi) {
    return(list(attainable = FALSE, best_lambda = NA_real_, best_slack = Inf))
  }

  current_lo <- lambda_lo
  current_hi <- lambda_hi
  best_lambda <- NA_real_
  best_slack <- Inf
  best_sol <- NULL

  for (iter in seq_len(max_refine)) {
    lambda_grid <- seq(current_lo, current_hi, length.out = grid_init)
    slack_vals <- rep(NA_real_, length(lambda_grid))

    for (j in seq_along(lambda_grid)) {
      sol <- sens_lambda_slack(lambda = lambda_grid[j],
                               A = A, B = B, C = C, D = D,
                               lower_rho = lower_rho,
                               upper_rho = upper_rho)
      slack_vals[j] <- sol$optimum
    }

    j_best <- which.min(slack_vals)

    if (slack_vals[j_best] < best_slack) {
      best_slack <- slack_vals[j_best]
      best_lambda <- lambda_grid[j_best]
      best_sol <- sens_lambda_slack(lambda = best_lambda,
                                    A = A, B = B, C = C, D = D,
                                    lower_rho = lower_rho,
                                    upper_rho = upper_rho)
    }

    if (best_slack <= tol_slack) {
      return(list(attainable = TRUE,
                  best_lambda = best_lambda,
                  best_slack = best_slack,
                  residual1 = best_sol$residual1,
                  residual2 = best_sol$residual2,
                  max_residual = best_sol$max_residual,
                  rho = best_sol$rho,
                  status = best_sol$status))
    }

    left_idx <- max(1, j_best - 1)
    right_idx <- min(length(lambda_grid), j_best + 1)

    current_lo <- lambda_grid[left_idx]
    current_hi <- lambda_grid[right_idx]

    if ((current_hi - current_lo) < 1e-10) {
      break
    }
  }

  list(attainable = (best_slack <= tol_slack),
       best_lambda = best_lambda,
       best_slack = best_slack)
}

sens_first_gamma <- function(results, id_col, gamma_vals) {
  ids <- unique(results[[id_col]])

  pointwise_first_gamma <- sapply(ids, function(id) {
    rows <- results[[id_col]] == id & results$zero_attainable
    idx <- match(results$gamma[rows], gamma_vals)
    if (length(idx) > 0) gamma_vals[min(idx)] else NA_real_
  })

  overall_idx <- which(vapply(gamma_vals, function(gamma) {
    any(results$gamma == gamma & results$zero_attainable)
  }, logical(1)))

  list(overall_first_gamma = if (length(overall_idx) > 0) gamma_vals[min(overall_idx)] else NA_real_,
       pointwise_first_gamma = pointwise_first_gamma)
}
