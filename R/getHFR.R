#' Function: getHFR
#'
#' Funciton that takes the dataframes with coordinates
#' and reforms them to a hyperframe with point patterns
#'
#' @param x Data for treatment
#' @param date Name of the column for dates in dataset x
#' @param coordinates Names of columns for coordinates. By default, x = longitude and y = latitude
#' @param x.class Name of the column that speficies subtypes of treatment in dataset x
#' @param x.subtype Names of the subtypes of treatments
#' @param y Data for outcomes
#' @param y.class Name of the column that specifies subtypes of outcomes in dataset y
#' @param y.subtype Names of the subtypes of outcomes
#' @param window Window, should be saved as an owin object
#' @param jitter Indicating whether to jitter; by default TRUE
#' @param jitter_amount Amount of jittering; by default 0.00001

getHFR <- function(x, date, coordinates, x.class, x.subtype,
                   y, y.class, y.subtype, window,
                   jitter = TRUE, jitter_amount = 0.0001) {

  # Checking the class of date column and converting the column to Date objects if necessary
  if (class(x$date) != "Date"){
    cat("Converting the column for dates into an object of class 'Date'...")
    x$date <- as.Date(x$date, format = "%Y-%m-%d")
  }

  date_range <- range(x$date) # Getting the range of dates ...
  all_dates <- seq(date_range[1], date_range[2], by = 1) # and a sequence of all dates

  for(i in c(x.subtype, y.subtype)){
    # Generating empty lists to save outputs
    assign(paste0(i, "_pp"), list())
  }

  all.y_pp <- list() # All activities

  tt <- 1 # Initiating the counter by 1

  for (ii in 1 : length(all_dates)) { # Looping over all the dates

    if (tt %% 30 == 0) {
      cat("Converting the data from", as.character(all_dates[ii]), "...\n")
    }

    # For the treatment variable/s -----

    # Subsetting by date and event types
    x_ii <- subset(x, date == all_dates[ii])

    for(i in x.subtype){
      # Generating data for one date and one subtype, named as variable_ii
      assign(paste0(i, "_ii"), x_ii[which(x_ii[, x.class] == i), ])

      # Turning to point patterns
      temp <- get(paste0(i, "_ii"))
      temp.ppp <- as.ppp(cbind(x = temp[, coordinates[1]],
                               y = temp[, coordinates[2]]),
                         W = window)
      temp <- assign(paste0(i, "_ii"), temp.ppp)

      if (jitter) {
        # If jitter = true, then jitter by the predefined amount of jittering
        temp.ppp.j <- spatstat.geom::rjitter(temp, radius = jitter_amount)
        temp <- assign(paste0(i, "_ii"), temp.ppp.j)
      }

      # Adding on to the list
      assign(paste0(i, "_pp"), append(get(paste0(i, "_pp")), list(temp)))

    }

    # For the outcome variables -----

    # Subsetting by date and event types
    y_ii <- subset(y, date == all_dates[ii])

    # First work on all activities --

    ## Turning to point patterns
    temp <- y_ii
    temp.ppp <- as.ppp(cbind(x = temp[, coordinates[1]],
                             y = temp[, coordinates[2]]),
                       W = window)
    temp <- assign(paste0("all.y", "_ii"), temp.ppp)

    if (jitter) {
      # If jitter = true, then jitter by the predefined amount of jittering
      temp.ppp.j <- spatstat.geom::rjitter(temp, radius = jitter_amount)
      temp <- assign(paste0("all.y", "_ii"), temp.ppp.j)
    }

    # Adding on to the list
    assign(paste0("all.y", "_pp"), append(get(paste0("all.y", "_pp")), list(temp)))

    # Then subtypes --
    for(i in y.subtype){
      # Generating data for one date and one subtype, named as variable_ii
      assign(paste0(i, "_ii"), y_ii[which(y_ii[, y.class] == i), ])

      # Turning to point patterns
      temp <- get(paste0(i, "_ii"))
      temp.ppp <- as.ppp(cbind(x = temp[, coordinates[1]],
                               y = temp[, coordinates[2]]),
                         W = window)
      temp <- assign(paste0(i, "_ii"), temp.ppp)

      if (jitter) {
        # If jitter = true, then jitter by the predefined amount of jittering
        temp.ppp.j <- spatstat.geom::rjitter(temp, radius = jitter_amount)
        temp <- assign(paste0(i, "_ii"), temp.ppp.j)
      }

      # Adding on to the list
      assign(paste0(i, "_pp"), append(get(paste0(i, "_pp")), list(temp)))

    }

    tt <- tt + 1 # Adding one to counters
  }

  # Saving all results as a hyperframe -----
  r <- hyperframe()

  for(i in c(x.subtype, y.subtype)){
    r[, i] <- get(paste0(i, "_pp"))
  }

  r[, "all.y"] <- all.y_pp

  r$date <- all_dates

  return(r)
}
