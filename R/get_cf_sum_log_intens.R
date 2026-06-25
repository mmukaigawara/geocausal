#' Calculate the log counterfactual densities
#'
#' @description A function that takes a hyperframe and returns the log counterfactual densities
#' ie, the numerator of the equation
#'
#' @param cf_dens A counterfactual density (an im object or a list of im objects)
#' @param treatment_data In the form of hyperframe$column
#'
#' @returns A numeric vector of sums of log densities for each time period
#'
#' @details `get_cf_sum_log_intens()` evaluates a counterfactual density at the
#' observed treatment locations for each time period using bilinear interpolation
#' (`spatstat.geom::interp.im()`), and returns the sum of the log intensities per
#' period. `cf_dens` may be a single `im` object reused across all periods or a
#' list of `im` objects with one per period. Points falling on the boundary, where
#' the intensity cannot be interpolated, are dropped; a period with no usable
#' points contributes a sum of zero.
#'
#' @references
#' Papadogeorgou, G., Imai, K., Lyall, J. and Li, F. (2022). Causal inference with spatio-temporal data: estimating the effects of airstrikes on insurgent violence in Iraq. \emph{Journal of the Royal Statistical Society Series B}, 84(5), 1969--1999. \doi{10.1111/rssb.12548}
#'
#' Mukaigawara, M., Imai, K., Lyall, J. and Papadogeorgou, G. (2025). Spatiotemporal causal inference with arbitrary spillover and carryover effects. arXiv preprint. \doi{10.48550/arXiv.2504.03464}
#'
#' @family density estimation functions

get_cf_sum_log_intens <- function(cf_dens,
                                  treatment_data) {

  if (spatstat.geom::is.im(cf_dens)) {
    # Case 1: single image used for all treatment times
    intensity_of_each_obs <- lapply(seq_along(treatment_data), function(t) {
      spatstat.geom::interp.im(
        cf_dens,
        x = treatment_data[[t]],
        bilinear = TRUE
      )
    })

  } else if (is.list(cf_dens) && all(vapply(cf_dens, spatstat.geom::is.im, logical(1)))) {
    # Case 2: list of images (same length as treatment_data)
    if (length(cf_dens) != length(treatment_data)) {
      stop("When cf_dens is a list, it must have the same length as treatment_data.")
    }

    intensity_of_each_obs <- lapply(seq_along(treatment_data), function(t) {
      spatstat.geom::interp.im(
        cf_dens[[t]],
        x = treatment_data[[t]],
        bilinear = TRUE
      )
    })

  } else {
    stop("cf_dens must be a spatstat 'im' or a list of 'im' objects.")
  }


  # Observations at the boundary cause troubles since intensity can't be determined.
  # This is mainly an issue pertaining to the resolution.
  # Fix this by removing those at the boundary.
  intensity_of_each_obs <- lapply(intensity_of_each_obs, function(x) x[!is.na(x)])

  sum_log_intensity <- sapply(intensity_of_each_obs, function(x) { # Take the sum of the above for each date
    r <- 0
    if (length(x) > 0) r <- sum(log(abs(x)))
    return(r)
  })

  return(sum_log_intensity)
}

