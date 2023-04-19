#' Function: get_smoothed_outcome
#'
#' A function that takes a column of hyperframes and generates a smoothed ppp
#'
#' @param data_interest Data to convert; should be in the form of "hyperframe$column"
#' @param method Methods for smoothing. Either "mclust" (fixed) or "abramson" (adaptive)
#' @param initialization Whether to use smaller samples to initialize mclust. Need to set seed for reproduction. By default = TRUE
#' @param sampling Determines the proportion of data to use for initialization. By default = 0.05, ie using 5% of samples first

get_smoothed_outcome <- function(data_interest,
                                 method,
                                 initialization = TRUE,
                                 sampling = 0.05) {

  # Obtain coordinates of interest -----

  all_points_coords <- data.table::rbindlist(purrr::map(data_interest, spatstat.geom::as.data.frame.ppp))

  # Fit the Gaussian mixture model (mclust) -----

  if (method == "mclust") {

    ## Identify the number of components (EII model)
    cat("Fitting the Gaussian mixture model\n")

    if (initialization == TRUE) {

      ## Prepare for initialization
      M <- round((nrow(all_points_coords)*sampling), digits = 0)
      init <- list(subset = sample(1:nrow(all_points_coords), size = M))

      ## Fit the mixture Gaussian model
      BIC <- mclust::mclustBIC(all_points_coords, modelNames = c("EII"),
                               initialization = init)
      mod_mcl <- mclust::Mclust(all_points_coords, x = BIC, modelNames = "EII")

    } else {

      ## Fit the model without initialization
      BIC <- mclust::mclustBIC(all_points_coords, modelNames = c("EII"))
      mod_mcl <- mclust::Mclust(all_points_coords, x = BIC, modelNames = "EII")

    }

    ## Get the value of sigma
    spat_sigma <- mod_mcl$parameters$variance
    spat_sigma <- sqrt(spat_sigma$sigmasq)

    ## Obtain smoothed outcomes
    cat("Smoothing ppps\n")

    smoothed_outcome <- furrr::future_map(data_interest, spatstat.explore::density.ppp,
                                          diggle = TRUE, kernel = "gaussian", adjust = 1,
                                          sigma = spat_sigma, edge = TRUE)

  }

  # Fit with Abramson -----

  if (method == "abramson") {

    ## Get h0 using CV on isotropic, spherical kernel smoothing
    window <- data_interest[[1]]$window # Extract window
    all_points <- spatstat.geom::as.ppp(cbind(x = all_points_coords$x,
                                              y = all_points_coords$y), W = window)

    ## Use Scott's rule of thumb to obtain h0
    scott_bw <- spatstat.explore::bw.scott(X = all_points, isotropic = FALSE) # Two h0 for each coordinate
    use_h0 <- as.numeric(scott_bw)
    pilot_dens <- density(all_points, sigma = use_h0, kernel = "gaussian") # Density based on h0

    him_points <- bw.abram(all_points, h0 = mean(use_h0), at = "points", pilot = pilot_dens)
    num_points <- as.numeric(purrr::map(as.list(data_interest), 2, .default = NA) %>% unlist())
    bw_pt <- split(him_points, rep(1 : length(data_interest), num_points))

    ## Obtain smoothed outcomes
    cat("Smoothing ppps\n")

    smoothed_outcome <- furrr::future_map2(data_interest, bw_pt, spatstat.explore::densityAdaptiveKernel,
                                           diggle = TRUE, kernel = "gaussian", edge = TRUE)

  }

  return(smoothed_outcome)

}
