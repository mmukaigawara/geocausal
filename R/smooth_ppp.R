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
#' @param resolution resolution of raster (distance map) (in km)
#' @param ndim the number of dimensions of grid cells (ndim^2). Users need to set either resolution or ndim.
#' @param ngroups (`method = "abramson"` only) the number of groups into which
#' the point-specific bandwidths are partitioned, passed on to
#' `spatstat.univar::densityAdaptiveKernel()`. By default, `NULL`, which uses
#' `spatstat`'s default (the square root of the number of points). Smaller values
#' speed up the computation at the cost of coarser bandwidth approximation.
#'
#' @returns im objects as a list
#'
#' @details To smooth ppp objects, users can choose either the Gaussian mixture model (`method = "mclust"`)
#' or Abramson's adaptive smoothing (`method = "abramson"`).
#' The Gaussian mixture model is essentially the method that performs model-based clustering of all the observed points.
#' In this package, we employ the EII model (equal volume, round shape (spherical covariance)).
#' This means that we model observed points by several Gaussian densities with the same, round shape.
#' This is why this model is called fixed-bandwidth smoothing. This is a simple model to smooth observed points,
#' yet given that analyzing spatiotemporal data is often computationally demanding, it is often the best place to start (and end).
#' Sometimes this process can also take time, which is why the `sampling` option is included in this function:
#' with `sampling`, the model is initialized with a random subset of the points, which considerably reduces
#' the computation time for large datasets.
#'
#' Another, more precise, method for smoothing outcomes is adaptive smoothing (`method = "abramson"`).
#' This method allows users to vary bandwidths based on `Abramson (1982)`.
#' Essentially, this model assumes that the bandwidth is inversely proportional to the square root of the target densities.
#' Since the bandwidth is adaptive, the estimation is usually more precise than the Gaussian mixture model.
#' However, the caveat is that this method is often extremely computationally demanding.
#'
#' Abramson's rule requires a pilot estimate of the spatially varying density,
#' which `smooth_ppp()` obtains automatically as follows. First, the points of all
#' time periods are pooled into a single point pattern. Second, a global bandwidth
#' is selected for this pooled pattern by Scott's rule of thumb
#' (`spatstat.explore::bw.scott()`, with separate bandwidths for the two coordinates),
#' and the pilot density is estimated by fixed-bandwidth Gaussian kernel smoothing
#' of the pooled pattern with these bandwidths. Third, point-specific bandwidths are
#' computed with `spatstat.explore::bw.abram.ppp()` following the inverse-square-root
#' rule, \eqn{h(u) = h_0 \min\{\tilde{f}(u)^{-1/2}/\gamma, \mathrm{trim}\}}{h(u) = h0 * min(f(u)^(-1/2)/gamma, trim)}, where
#' \eqn{h_0}{h0} is the global bandwidth (the mean of the two Scott bandwidths),
#' \eqn{\tilde{f}(u)}{f(u)} is the pilot density, \eqn{\gamma}{gamma} is the geometric mean of the
#' \eqn{\tilde{f}(u)^{-1/2}}{f(u)^(-1/2)} terms over the observed points (which puts \eqn{h_0}{h0} on
#' the scale of a fixed bandwidth), and the trimming value (5, the `spatstat` default)
#' prevents excessively large bandwidths at isolated points (Hall and Marron, 1988).
#' Finally, the point-specific bandwidths are assigned back to the points of each
#' time period, and each time period is smoothed by
#' `spatstat.univar::densityAdaptiveKernel()` with these bandwidths.
#'
#' Parallel computation: `smooth_ppp()` smooths the point patterns of all time periods
#' using the `future` framework (via `furrr`), which runs sequentially unless a parallel
#' backend is registered. To smooth the time periods in parallel, set a parallel plan
#' before calling this function, e.g., `future::plan(future::multisession)`.
#' The results are identical regardless of the plan. Parallelization is most
#' beneficial for `method = "abramson"`, whose computation time is dominated by the
#' adaptive smoothing of each time period; for `method = "mclust"`, the model fitting
#' itself is not parallelized, and the one-time cost of launching the parallel workers
#' may outweigh the gains for the (fast) smoothing stage.
#'
#' @references Abramson, I. S. (1982). On bandwidth variation in kernel estimates
#' --- a square root law. \emph{The Annals of Statistics}, 10(4), 1217--1223.
#'
#' Hall, P. and Marron, J. S. (1988). Variable window width kernel density estimates
#' of probability densities. \emph{Probability Theory and Related Fields}, 80, 37--49.
#'
#' Scott, D. W. (1992). \emph{Multivariate Density Estimation: Theory, Practice and
#' Visualization}. New York: Wiley.
#'
#' @family data preparation functions
#'
#' @examples
#' \donttest{
#' # Insurgency outcomes in Iraq, 2006 (first 60 days)
#' dat <- insurgencies_2006
#' dat$time <- as.numeric(dat$date - min(dat$date) + 1)
#' dat <- dat[dat$time <= 60, ]
#' hfr <- get_hfr(data = dat, col = "type", window = iraq_window,
#'                time_col = "time", time_range = c(1, 60),
#'                coordinates = c("longitude", "latitude"), combine = TRUE)
#'
#' # Adaptive smoothing of the outcomes
#' # (to run in parallel, call future::plan(future::multisession) first)
#' smoothed <- smooth_ppp(hfr$all_combined, method = "abramson", ndim = 64)
#' }

smooth_ppp <- function(data,
                       method,
                       sampling = NA,
                       resolution = NULL,
                       ndim = NULL,
                       ngroups = NULL) {

  # Get window from first ppp object

  window <- data[[1]]$window

  # Determine output dimensions based on parameters
  if (!is.null(ndim)) {
    # Pixel mode: fixed dimensions
    dimyx <- c(ndim, ndim)
    message("Using pixel mode: ", ndim, "x", ndim, " pixels\n")
  } else if (!is.null(resolution)) {
    # Resolution mode: km per pixel
    x_extent <- diff(window$xrange)
    y_extent <- diff(window$yrange)
    nc <- ceiling(x_extent / resolution)
    nr <- ceiling(y_extent / resolution)
    dimyx <- c(nr, nc)
    message("Using resolution mode: ", resolution, " km per pixel -> ", nr, "x", nc, " pixels\n")
  } else {
    # Default: 128x128
    dimyx <- c(128, 128)
    message("Using default dimensions: 128x128 pixels\n")
  }

  # Combine all point coordinates
  all_points_coords <- data.table::rbindlist(purrr::map(data, spatstat.geom::as.data.frame.ppp))

  if (method == "mclust") {
    message("Fitting the Gaussian mixture model\n")

    if (is.na(sampling) && nrow(all_points_coords) > 10000) {
      message("The data contain ", nrow(all_points_coords), " points; ",
              "consider setting the `sampling` argument ",
              "(e.g., sampling = 0.1) to speed up the model fitting\n")
    }

    if (is.na(sampling)) {
      BIC <- mclust::mclustBIC(all_points_coords, modelNames = c("EII"))
      mod_mcl <- mclust::Mclust(all_points_coords, x = BIC, modelNames = "EII")
    } else {
      M <- round((nrow(all_points_coords) * sampling), digits = 0)
      init <- list(subset = sample(1:nrow(all_points_coords), size = M))
      BIC <- mclust::mclustBIC(all_points_coords, modelNames = c("EII"),
                               initialization = init)
      mod_mcl <- mclust::Mclust(all_points_coords, x = BIC, modelNames = "EII")
    }

    spat_sigma <- mod_mcl$parameters$variance
    spat_sigma <- sqrt(spat_sigma$sigmasq)

    message("Smoothing ppps\n")
    smoothed_outcome <- furrr::future_map(data, spatstat.explore::density.ppp,
                                          diggle = TRUE, kernel = "gaussian", adjust = 1,
                                          sigma = spat_sigma, edge = TRUE,
                                          dimyx = dimyx)
  }

  if (method == "abramson") {
    # Create ppp object from combined coordinates
    # checkdup = FALSE: pooling points across time periods produces legitimate
    # spatial duplicates, so the duplicate-point check (and its warning) is skipped
    all_points <- spatstat.geom::as.ppp(cbind(x = all_points_coords$x,
                                              y = all_points_coords$y), W = window,
                                        checkdup = FALSE)

    # Compute Scott bandwidth
    scott_bw <- spatstat.explore::bw.scott(X = all_points, isotropic = FALSE)
    use_h0 <- as.numeric(scott_bw)

    # Create pilot density at DEFAULT resolution
    pilot_dens <- density(all_points, sigma = use_h0, kernel = "gaussian")

    # Compute per-point bandwidths using default-resolution pilot
    him_points <- spatstat.explore::bw.abram.ppp(all_points, h0 = mean(use_h0),
                                                 at = "points", pilot = pilot_dens)

    # Split bandwidths by time period
    num_points <- unlist(as.numeric(purrr::map(as.list(data), 2, .default = NA)))
    bw_pt <- split(him_points, rep(1:length(data), num_points))

    # Handle days with no points
    no_point_id <- which(num_points == 0)
    if (length(no_point_id) != 0) {
      empty_elements <- vector("list", length(no_point_id))
      names(empty_elements) <- as.character(no_point_id)
      for (i in 1:length(no_point_id)) {
        bw_pt <- c(bw_pt[seq_along(bw_pt) < no_point_id[i]],
                   empty_elements[i], bw_pt[seq_along(bw_pt) >= no_point_id[i]])
      }
    }

    message("Smoothing ppps\n")
    # Apply densityAdaptiveKernel with CUSTOM dimyx (output resolution)
    # The dimyx here controls the output image size, not bandwidth computation
    # ngroups = NULL uses spatstat's default grouping (sqrt of the number of points)
    # spatstat.explore must be loaded on parallel workers so that
    # densityAdaptiveKernel() dispatches to its ppp method
    smoothed_outcome <- furrr::future_map2(data, bw_pt, spatstat.univar::densityAdaptiveKernel,
                                           diggle = TRUE, kernel = "gaussian", edge = TRUE,
                                           dimyx = dimyx, ngroups = ngroups,
                                           .options = furrr::furrr_options(packages = "spatstat.explore"))
  }

  class(smoothed_outcome) <- c("list", "imlist")
  return(smoothed_outcome)
}
