#' Calculate the log counterfactual densities
#'
#' @description A function that takes a hyperframe and returns the log counterfactual densities
#' ie, the numerator of the equation
#'
#' @param cf_dens A counterfactual density (an im object)
#' @param treatment_data In the form of hyperframe$column
#'
#' @returns A numeric vector of sums of log densities for each time period

get_cf_sum_log_intens <- function(cf_dens,
                                  treatment_data) {

  intensity_of_each_obs <- lapply(1:length(treatment_data), function(t) {
    spatstat.geom::interp.im(cf_dens,
                             x = treatment_data[[t]],
                             bilinear = TRUE)
  })

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
