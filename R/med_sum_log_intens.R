#' Calculate the sum of conditional log densities of mediators
#'
#' @description A function that takes a dataframe and returns the sum of conditional log densities
#' of the mediators, which will be used for the numerator of the equation
#'
#' @param data A dataframe (the treatment data)
#' @param mediator Name of the mediator
#' @param indep_var Independent variables to run regression analyses
#' @param time_var Name of the time variable
#' @param model Model selection. The current version supports Poisson only
#'
#' @returns A numeric vector of sums of conditional log densities of mediators for each time period

med_sum_log_intens <- function(data,
                               mediator,
                               indep_var,
                               time_var,
                               model) {

  temp <- data
  temp$time <- temp[[time_var]]
  text_form <- paste0(mediator, " ~ ", paste(indep_var, collapse = " + "))

  if (model == "pois") {

    message("Using Poisson regression to obtain the sum of log intensities")

    # Run a Poisson regression
    model <- stats::glm(formula = as.formula(text_form), data = temp, family = "poisson")

    # Obtain intensities for each observation
    temp$intensities <- stats::predict(model, type = "response")

    # Compute the density using dpois
    temp$density <- stats::dpois(temp[[mediator]], lambda = temp$intensities)

    # Take the sum of log intensities
    sum_log_cond_dens <- temp |>
      dplyr::group_by(time) |>
      dplyr::summarise(sum_log_density = sum(log(density)))

    # Identify time periods with no observations
    no_obs <- which(!(c(range(temp$time)[1]:range(temp$time)[2]) %in%
                        sum_log_cond_dens$time))

    if (length(no_obs) > 0) {
      ## For these time periods, set log intens = 0 (b/c conditional intens = 1)
      log_impute <- data.frame(time = no_obs,
                               sum_log_density = 0)

      sum_log_cond_dens <- rbind(sum_log_cond_dens, log_impute)
      sum_log_cond_dens <- sum_log_cond_dens |> dplyr::arrange(time)

    }

    return(sum_log_cond_dens)
  }

}
