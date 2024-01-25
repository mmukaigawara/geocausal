#' Obtain histories of treatment or outcome events
#'
#' @description
#' `get_hist()` takes a hyperframe and time and columns of interest, and generates histories of events of interest.
#'
#' @param tt values of the time variable of interest for which `get_hist()` generates histories
#' @param Xt the name of a treatment column
#' @param Yt the name of an outcome column
#' @param lag numeric. `lag` specifies the number of time periods over which
#' `get_hist()` aggregates treatment and outcome columns.
#' @param window owin object.
#' @param x_only logical. `x_only` specifies whether to generate only treatment history (no outcome history). By default, `FALSE`.
#'
#' @returns list of treatment and outcome histories
#'
#' @examples
#' dat_out <- insurgencies[1:100, ]
#' dat_out$time <- as.numeric(dat_out$date - min(dat_out$date) + 1)
#'
#' # Hyperframe
#' dat_hfr <- get_hfr(data = dat_out,
#'                    col = "type",
#'                    window = iraq_window,
#'                    time_col = "time",
#'                    time_range = c(1, max(dat_out$time)),
#'                    coordinates = c("longitude", "latitude"),
#'                    combine = TRUE)
#'
#' # Histories
#' lapply(1:nrow(dat_hfr), get_hist,
#'        Xt = dat_hfr$all_outcome,
#'        lag = 1, window = iraq_window)

get_hist <- function(tt, Xt, Yt = NA, lag, window, x_only = TRUE) {

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
