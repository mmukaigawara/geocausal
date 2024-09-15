#' Generate a Hajek estimator
#'
#' @description A function that returns a Hajek estimator of causal contrasts
#'
#' @param weighted_surf_1 a weighted surface for scenario 1
#' @param weighted_surf_2 another weighted surface for scenario 2
#' @param use_dist whether to use distance-based maps. By default, TRUE
#' @param windows a list of owin objects (if `use_dist = FALSE`)
#' @param dist_map distance map (an im object, if `use_dist = TRUE`)
#' @param dist distances (a numeric vector within the max distance of `dist_map`)
#' @param entire_window an owin object of the entire map
#' 
#' @returns list of Hajek estimators for each scenario (`est_haj`), 
#' causal contrasts (Hajek estimator) as a matrix (`est_tau_haj_matrix`), and 
#' causal contrast (scenario 2 - scenario 1) as a numeric vector (`est_tau_haj_cf2_vs_cf1`), 
#' along with weights, windows, and smoothed outcomes
#'  
#' @details `get_estimates()` is an internal function to `get_est()` function, 
#' performing the estimation analysis after `get_weighted_surf()` function

get_estimates <- function(weighted_surf_1,
                          weighted_surf_2,
                          use_dist = TRUE,
                          windows,
                          dist_map,
                          dist,
                          entire_window) {
  
  # Function to integrate weighted surface over entire windows
  Integrate <- function(intensity, B) { #Integration
    
    r <- matrix(NA, nrow = length(intensity), ncol = length(B))
    for (bb in 1 : length(B)) {
      r[, bb] <- sapply(intensity, function(x) spatstat.geom::integral.im(x, domain = B[[bb]]))
    }
    return(r)
  }
  
  # Enlist average weighted surfaces and create a matrix for weights
  av_surf <- list(weighted_surf_1$average_surf, weighted_surf_2$average_surf)
  av_surf_haj <- list(weighted_surf_1$average_surf_haj, weighted_surf_2$average_surf_haj)
  weights <- rbind(weighted_surf_1$weights, weighted_surf_2$weights)
  stabilizer <- c(weighted_surf_1$stabilizer, weighted_surf_2$stabilizer) 
  
  if(use_dist) { #Distance-based windows - first generate a list of owin objects
    
    ## Get the range of standardized distances
    distance_range <- range(`dist_map`$v, na.rm = TRUE)
    
    if (max(dist) > distance_range[2]) {
      stop("The max distance should be within the range of the entire window.")
    } #Confirm that max(distances) <= max(distance_range)
    
    distances <- c(dist, distance_range[2])
    
    #Note: another idea is to use quantiles, but this could take time when we obtain variances
    #distance_quantiles <- quantile(distance_range, probs = seq(0, 1, by = 0.01))
    
    ## Convert the distance map to windows
    distance_window <- matrix(`dist_map`$v, nrow = nrow(`dist_map`$v))
    distance_windows <- lapply(distances, function(x) distance_window < x) # A list of binary matrices based on quantiles
    distance_owin <- lapply(distance_windows, function(x) {
      spatstat.geom::owin(mask = x, xrange = `entire_window`$xrange, yrange = `entire_window`$yrange)
    }) # Owin objects with different distances from the focus
    windows <- distance_owin
    
  }
  
  # Then integrate over the windows
  est <- Integrate(intensity = av_surf, B = windows) #IPW estimates
  est_haj <- sweep(est, MARGIN = 1, STATS = apply(weights, 1, mean), FUN = '/')
  rownames(est_haj) <- c("cf_1", "cf_2")
  
  est_tau_haj <- array(NA, dim = c(nrow(est), nrow(est), ncol(est)))
  for (ind1 in 1 : nrow(est)) {
    for (ind2 in 1 : nrow(est)) {
      est_tau_haj[ind1, ind2, ] <- est_haj[ind1, ] - est_haj[ind2, ] #Comparing two counterfactual scenarios
    }
  }
  
  return(list(est_haj = est_haj, 
              est_tau_haj_matrix = est_tau_haj, 
              est_tau_haj_cf2_vs_cf1 = est_tau_haj[2, 1, ],
              weights = weights,
              stabilizer = stabilizer,
              windows = windows,
              smoothed_outcome = weighted_surf_1$smoothed_outcome))
  
}