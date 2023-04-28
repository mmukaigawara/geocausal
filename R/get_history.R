#' Function: get_history
#'
#' A function that takes hyperframe that was generated with get_hfr
#' and generates treatment and outcome histories
#'
#' @param tt Point of time of interest
#' @param Xt A treatment column of a hyperframe
#' @param Yt An outcome column of a hyperframe
#' @param lag Lag to aggregate treatment and outcome
#' @param window Window, should be saved as an owin object
#' @param x_only Whether to generate treatment history only; by default, FALSE

get_history <- function(tt, Xt, Yt = NA, lag, window, x_only = TRUE) {

  lags <- rep(lag, 2)

  # The variable up_to_time is length 2
  # corresponding to the starting points to collect history of treatment and outcome
  up_to_time <- sapply(lags, function(x) max(1, tt - x))

  # Creating history -----
  Xt_hist <- spatstat.geom::as.ppp(X = matrix(0, nrow = 0, ncol = 2), W = window)
  start_time <- tt - 1

  # If the time period is one time point, then this is the time point we use,
  # otherwise, we superimpose all points.
  if (start_time == up_to_time[1]) {

    Xt_hist <- Xt[[start_time]]

  } else if (start_time > up_to_time[1]) {

    Xt_hist <- do.call(eval(parse(text = "spatstat.geom::superimpose")), 
                       Xt[start_time : up_to_time[1]])
    Xt_hist$marks <- NULL # Change the shapes of points

  }

  # If x_only = TRUE, then stop here
  if (x_only) {

    return(list(Xt_hist))

  } else {

    # Creating outcome history -----
    Yt_hist <- spatstat.geom::as.ppp(X = matrix(0, nrow = 0, ncol = 2), W = window)
    if (tt > 1) {

      Yt_hist <- Yt[[start_time]]

      if (start_time > up_to_time[2]) {

        Yt_hist <- do.call(eval(parse(text = "spatstat.geom::superimpose")), 
                           Yt[start_time : up_to_time[2]])
        Yt_hist$marks <- NULL

      }

    }

    return(list(Xt_hist = Xt_hist, Yt_hist = Yt_hist))

  }

}
