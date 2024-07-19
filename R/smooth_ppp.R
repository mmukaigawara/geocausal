#' Smooth outcome events
#'
#' @description
#' `smooth_ppp()` takes a column of hyperframes (ppp objects) and smoothes them.
#'
#' @param data the name of a hyperframe and column of interest.
#' `data` should be in the form of `"hyperframe$column"`.
#' @param method methods for smoothing ppp objects.
#' Either `"mclust"` or `"abramson"`. See details.
#' @param sampling numeric between 0 and 1. `sampling` determines the proportion of data
#' to use for initialization. By default, NA (meaning that it uses all data without sampling).
#'
#' @returns im objects
#'
#' @details To smooth ppp objects, users can choose either the Gaussian mixture model (`method = "mclust"`)
#' or Abramson's adaptive smoothing (`method = "abramson"`).
#' The Gaussian mixture model is essentially the method that performs model-based clustering of all the observed points.
#' In this package, we employ the EII model (equal volume, round shape (spherical covariance)).
#' This means that we model observed points by several Gaussian densities with the same, round shape.
#' This is why this model is called fixed-bandwidth smoothing. This is a simple model to smooth observed points,
#' yet given that analyzing spatiotemporal data is often computationally demanding, it is often the best place to start (and end).
#' Sometimes this process can also take time, which is why an option for `init` is included in this function.
#'
#' Another, more precise, method for smoothing outcomes is adaptive smoothing (`method = "abram"`).
#' This method allows users to vary bandwidths based on `Abramson (1982)`.
#' Essentially, this model assumes that the bandwidth is inversely proportional to the square root of the target densities.
#' Since the bandwidth is adaptive, the estimation is usually more precise than the Gaussian mixture model.
#' However, the caveat is that this method is often extremely computationally demanding.

smooth_ppp <- function(data,
                       method,
                       sampling = NA) {

  # Obtain coordinates of interest -----

  all_points_coords <- data.table::rbindlist(purrr::map(data, spatstat.geom::as.data.frame.ppp))

  # Fit the Gaussian mixture model (mclust) -----

  if (method == "mclust") {

    ## Identify the number of components (EII model)
    message("Fitting the Gaussian mixture model\n")

    if (is.na(sampling)) {

      ## Fit the model without initialization
      BIC <- mclust::mclustBIC(all_points_coords, modelNames = c("EII"))
      mod_mcl <- mclust::Mclust(all_points_coords, x = BIC, modelNames = "EII")


    } else {

      ## Prepare for initialization
      M <- round((nrow(all_points_coords)*sampling), digits = 0)
      init <- list(subset = sample(1:nrow(all_points_coords), size = M))

      ## Fit the mixture Gaussian model
      BIC <- mclust::mclustBIC(all_points_coords, modelNames = c("EII"),
                               initialization = init)
      mod_mcl <- mclust::Mclust(all_points_coords, x = BIC, modelNames = "EII")

    }

    ## Get the value of sigma
    spat_sigma <- mod_mcl$parameters$variance
    spat_sigma <- sqrt(spat_sigma$sigmasq)

    ## Obtain smoothed outcomes
    message("Smoothing ppps\n")

    smoothed_outcome <- furrr::future_map(data, spatstat.explore::density.ppp,
                                          diggle = TRUE, kernel = "gaussian", adjust = 1,
                                          sigma = spat_sigma, edge = TRUE)

  }

  # Fit with Abramson -----

  if (method == "abramson") {

    ## Get h0 using CV on isotropic, spherical kernel smoothing
    window <- data[[1]]$window # Extract window
    all_points <- spatstat.geom::as.ppp(cbind(x = all_points_coords$x,
                                              y = all_points_coords$y), W = window)

    ## Use Scott's rule of thumb to obtain h0
    scott_bw <- spatstat.explore::bw.scott(X = all_points, isotropic = FALSE) # Two h0 for each coordinate
    use_h0 <- as.numeric(scott_bw)
    pilot_dens <- density(all_points, sigma = use_h0, kernel = "gaussian") # Density based on h0

    him_points <- spatstat.explore::bw.abram.ppp(all_points, h0 = mean(use_h0), at = "points", pilot = pilot_dens) # Bandwidth
    num_points <- as.numeric(purrr::map(as.list(data), 2, .default = NA) %>% unlist()) # Num of points for each time frame
    bw_pt <- split(him_points, rep(1 : length(data), num_points))

    no_point_id <- which(num_points == 0) # IDs of time frames with the num of points = 0
    if (length(no_point_id) != 0) {
      empty_elements <- vector("list", length(no_point_id))
      names(empty_elements) <- as.character(no_point_id)
      for (i in 1:length(no_point_id)) {
        bw_pt <- c(bw_pt[seq_along(bw_pt) < no_point_id[i]],
                   empty_elements[i], bw_pt[seq_along(bw_pt) >=
                                              no_point_id[i]])
      }
    }

    ## Obtain smoothed outcomes
    message("Smoothing ppps\n")

    smoothed_outcome <- furrr::future_map2(data, bw_pt, spatstat.univar::densityAdaptiveKernel,
                                           diggle = TRUE, kernel = "gaussian", edge = TRUE)

  }

  return(smoothed_outcome)

}
