#' Calculate variance upper bounds
#'
#' @description A function that calculates variance upper bounds
#'
#' @param estimates an object returned from `get_est()` function
#' 
#' @returns list of variance upper bounds for each scenario (`bound_haj`) and causal contrasts (`bound_tau_haj`). 
#' Note that this function returns variance upper bounds for Hajek estimators
#'  
#' @details `get_var_bound()` is an internal function to `get_estimates()` function, 
#' performing the estimation analysis after `get_est()` function. 

get_var_bound <- function(estimates) {
  
  # Define arguments
  weights <- estimates$weights
  stabilizer <- estimates$stabilizer
  B <- estimates$windows
  smoothed_outcome <- estimates$smoothed_outcome
  num_interv <- dim(weights)[1]
  num_B <- length(B)
  num_time_points <- length(smoothed_outcome)
  
  # Integrate smoothed outcomes over each window
  calc_integrals_for_domain <- function(domain) {
    sapply(smoothed_outcome, function(s) spatstat.univar::integral(s, domain = domain))
  }
  out_in_B <- suppressMessages( 
    purrr::map_dfc(B, calc_integrals_for_domain)
  ) #Suppress messages for renaming the columns
  out_in_B <- as.matrix(out_in_B) #Output = row x column = days x windows
  
  # Obtain weighted, integrated outcomes
  all_est <- array(NA, dim = c(num_interv, num_time_points, num_B)) #Row = CF, col = time, z = windows
  for (ii in 1 : num_interv) {
    for (bb in 1 : num_B) {
      all_est[ii, , bb] <- out_in_B[, bb] * weights[ii, ]
    }
  }
  
  # Variance upper bound (IPW, raw)
  bound <- apply(all_est, c(1, 3), function(x) mean(x ^ 2) / num_time_points)
  

  rownames(bound) <- paste0("cf_", 1 : num_interv)
  
  # Bound for tau
  bound_t <- array(NA, dim = c(num_interv, num_interv, num_B))
  for (ii in 1 : (num_interv - 1)) {
    for (jj in (ii + 1) : num_interv) {
      est <- all_est[ii, , , drop = FALSE] - all_est[jj, , , drop = FALSE]
      bound_t[ii, jj, ] <- apply(est, 3, function(x) mean(x ^ 2) / num_time_points)
    }
  }
  
  # Bound for Hajek
  mean_weight <- apply(weights, 1, mean)
  all_est <- sweep(all_est, MARGIN = 1, mean_weight, FUN = '/')
  
  weights <- sweep(weights, MARGIN = 1, stabilizer, FUN = '/')
  


  # bound_haj <- sweep(bound, 1, mean_weight ^ 2, FUN = '/')     # ad hoc approach
  # bound_t_haj <- array(NA, dim = c(num_interv, num_interv, num_B))
  # for (ii in 1 : (num_interv - 1)) {
  #   for (jj in (ii + 1) : num_interv) {
  #     est <- all_est[ii, , , drop = FALSE] - all_est[jj, , , drop = FALSE]
  #     bound_t_haj[ii, jj, ] <- apply(est, 3, function(x) mean(x ^ 2) / num_time_points)
  #   }
  # }
  
  bound_haj <- array(NA,c(num_interv,num_B))
  rownames(bound_haj) <- paste0("cf_", 1 : num_interv)
  for (ii in 1 : num_interv) {
    for (bb in 1:num_B) {
      mean_est <- apply(all_est, c(1,3), mean)
      J <- c(1, -mean_est[ii,bb])
      tmp <- rbind(all_est[ii,,bb],weights[ii,])
      bound_haj[ii,bb] <- J%*%tmp%*%(t(tmp)/num_time_points^2)%*%J
    }
    
  }
  
  # Bound for tau and Hajek
  bound_t_haj <- array(NA, dim = c(num_interv, num_interv, num_B))
  for (ii in 1 : (num_interv - 1)) {
    for (jj in (ii + 1) : num_interv) {
      for (bb in 1:num_B) {
        J <- c(-1,1,mean_est[ii,bb],-mean_est[jj,bb])
        tmp <- rbind(all_est[ii,,bb],all_est[jj,,bb],weights[ii,],weights[jj,])
        bound_t_haj[ii, jj,bb] <- J%*%tmp%*%(t(tmp)/num_time_points^2)%*%J
      }
    }
  }
  
  
  

  
  return(list(bound_haj = bound_haj, # Return Hajek only
              bound_tau_haj = bound_t_haj[1, 2, ]))
  
}
