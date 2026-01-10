#' Perform superthinning tests
#'
#' @description
#' `dx_supthin()` performs superthinning tests to examine model validity.
#'
#' @param hfr hyperframe
#' @param dep_var The name of the dependent variable.
#' Since we need to obtain the observed density of treatment events,
#' `dep_var` should be the name of the treatment variable.
#' @param indep_var vector of names of independent variables (covariates)
#' @param window owin object
#' @param rescale conversion as needed (namely when the unit of distance of the owin object is in meters).
#' By default = 1 (no conversion)
#' @param max_r max distance in which the envelope tests are performed
#' @param n_sample the number of points to sample. by default = 1000, if the number of points are smaller than this, no sampling is performed
#' @param nsim the number of simulations to perform for the envelope tests
#' @param unit distance units after conversion. By default "km"
#'
#' @returns A list of resulting dataframe (`result_data`), windows (`window_list`), data for distance quantiles,
#' and a window object for the entire window

dx_supthin <- function(hfr, dep_var, indep_var, window,
                       rescale = 1, max_r = 50, n_sample = 1000,
                       nsim = 1000, unit = "km") {

  text_form <- paste0(dep_var, " ~ ", paste(indep_var, collapse = " + "))
  message("Fitting the model...\n")
  mod <- spatstat.model::mppm(as.formula(text_form), data = hfr) #Fit mppm

  # Set the critical value and rescale the map to km -----
  lambda_im <- predict(mod, type = "trend") # Prediction

  w_km <- spatstat.geom::rescale(spatstat.geom::Window(lambda_im$trend[[1]]), rescale, "km")
  total_area_km <- spatstat.geom::area(w_km)
  total_points <- sum(sapply(hfr[[dep_var]], function(x) x$n))

  cval_km <- total_points / total_area_km

  # Perform the superthinning loop -----
  superthinning_result <- lapply(1:nrow(lambda_im), function(x) {

    # Rescale inputs for each time period to km
    lambda_oneperiod <- spatstat.geom::rescale(lambda_im$trend[[x]], rescale, unit)
    X <- spatstat.geom::rescale(hfr[[dep_var]][[x]], rescale, unit)

    # Define Residual Intensity (c - lambda)
    lambda_resid <- spatstat.geom::eval.im(pmax(cval_km - lambda_oneperiod, 0))

    # Thinning
    if(X$n == 0) {
      X_super <- spatstat.random::rpoispp(lambda_resid)
      Y <- spatstat.geom::superimpose(superposed = X_super, W = w_km)
    } else {
      lambda_at_points <- lambda_oneperiod[X, drop = FALSE]
      lambda_at_points[is.na(lambda_at_points) | lambda_at_points <= 0] <- 1e-20 # Edge cases

      thin_prob <- cval_km / lambda_at_points
      thin_prob[thin_prob > 1] <- 1
      thin_prob[thin_prob < 0] <- 0

      # Apply Thinning and Superposition
      keep <- stats::rbinom(X$n, size = 1, prob = thin_prob) == 1
      X_thinned <- X[keep]
      X_super <- spatstat.random::rpoispp(lambda_resid)

      Y <- spatstat.geom::superimpose(thinned = X_thinned, superposed = X_super, W = w_km)
    }
    return(Y)
  })

  # Aggregate Results
  Y_all_km <- do.call(spatstat.geom::superimpose, superthinning_result)
  Y_all_km <- spatstat.geom::unmark(Y_all_km)

  # Global Envelope Test (Optimized for Memory)
  if (Y_all_km$n > n_sample) {
    # Generate a random selection of indices
    idx <- sample(seq_len(Y_all_km$n), size = n_sample, replace = FALSE)
    Y_sub <- Y_all_km[idx]
    # Recalculate cval specifically for this sample size/area
    cval_sub <- n_sample / spatstat.geom::area(w_km)

  } else {
    # If not many points exist, perform the text without sampling
    Y_sub <- Y_all_km
    cval_sub <- cval_km
  }

  # Now run the envelope
  E_lfunc <- spatstat.explore::envelope(
    Y_sub,
    fun = spatstat.explore::Lest,
    transform = expression(. - r),
    nsim = nsim,
    simulate = expression(spatstat.random::rpoispp(cval_sub, win = w_km)),
    global = TRUE,
    rmax = max_r,
    savefuns = TRUE
  )

  E_kfunc <- spatstat.explore::envelope(
    Y_sub,
    fun = spatstat.explore::Kest,
    nsim = nsim,
    simulate = expression(spatstat.random::rpoispp(cval_sub, win = w_km)),
    global = TRUE,
    rmax = max_r,
    correction = "best",
    savefuns = TRUE
  )

  # Results
  env_l <- as.data.frame(E_lfunc)
  env_l$fx <- "l"
  env_k <- as.data.frame(E_kfunc)
  env_k$fx <- "k"

  env_rest <- rbind(env_l, env_k)

  class(env_rest) <- c("supthin", "data.frame")

  return(env_rest)

}
