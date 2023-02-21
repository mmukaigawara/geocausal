#' Function: get_hfr
#'
#' A function that takes the dataframes with coordinates
#' and reforms them to a hyperframe with point patterns
#'
#' @param treatment Data for treatment
#' @param treatment_type Name of the column that speficies subtypes of treatment in dataset x
#' @param outcome Data for outcomes
#' @param outcome_type Name of the column that specifies subtypes of outcomes in dataset y
#' @param window Window, should be saved as an owin object
#' @param date Name of the column for dates in the treatment dataset
#' @param coordinates Names of columns for coordinates. By default, x = longitude and y = latitude
#' @param jitter Indicating whether to jitter; by default TRUE
#' @param jitter_amount Amount of jittering; by default 0.00001

get_hfr <- function(treatment, treatment_type,
                    outcome, outcome_type, window,
                    date = "date",
                    coordinates = c("longitude", "latitude"),
                    jitter = TRUE, jitter_amount = 0.0001) {

  # Getting the range of dates -----------
  date_range <- range(as.vector(treatment[, date])[[1]])
  all_dates <- seq(date_range[1], date_range[2], by = 1)

  # Converting treatment data to ppp ----------
  cat("Converting the treatment data to ppp objects...\n")

  ## Creating empty point process for imputation
  empty <- data.frame(latitude = double(), longitude = double())
  empty_ppp <- as.ppp(cbind(x = empty$longitude, y = empty$latitude), W = window)

  ## Converting data to point process
  x_ppp <- treatment %>%
    group_by_at(vars(date, paste0(treatment_type))) %>%
    group_map(~as.ppp(cbind(x = .x[, coordinates[1]],
                            y = .x[, coordinates[2]]),
                      W = window))

  ## Generating data and point process with all treatment combined
  x_c <- treatment
  x_c[, treatment_type] <- "all_treatment"
  x_c_ppp <- x_c %>%
    group_by_at(vars(date, paste0(treatment_type))) %>%
    group_map(~as.ppp(cbind(x = .x[, coordinates[1]],
                            y = .x[, coordinates[2]]),
                      W = window))

  if (jitter) {
    x_ppp <- purrr::map(x_ppp, spatstat.geom::rjitter, radius = jitter_amount)
    x_c_ppp <- purrr::map(x_c_ppp, spatstat.geom::rjitter, radius = jitter_amount)
  }

  cat("Converting the treatment data to a hyperframe...\n")

  ## Identifying missing dates
  treatment_dates_types <- treatment %>%
    group_by_at(vars(date, paste0(treatment_type))) %>%
    dplyr::select(date, paste0(treatment_type)) %>%
    distinct()

  treatment_dates_types_c <- x_c %>%
    group_by_at(vars(date, paste0(treatment_type))) %>%
    dplyr::select(date, paste0(treatment_type)) %>%
    distinct()

  all_date_type <- expand.grid(date = all_dates, type = pull(unique(treatment[, treatment_type])))
  obs_date_type <- data.frame(date = pull(treatment_dates_types[, 1]),
                              type = pull(treatment_dates_types[, 2]))
  missing_date_type <- setdiff(all_date_type, obs_date_type)

  all_date_type_c <- data.frame(date = all_dates, type = pull(unique(x_c[, treatment_type])))
  obs_date_type_c <- data.frame(date = pull(treatment_dates_types_c[, 1]),
                                type = pull(treatment_dates_types_c[, 2]))
  missing_date_type_c <- setdiff(all_date_type_c, obs_date_type_c)

  ## Combining observed and missing data
  x_hfr <- rbind(hyperframe(date = pull(treatment_dates_types[, 1]),
                            type = pull(treatment_dates_types[, 2]),
                            ppp = x_ppp),
                 hyperframe(date = missing_date_type$date,
                            type = missing_date_type$type,
                            ppp = empty_ppp)
  )

  x_hfr_out <- hyperframe(date = all_dates)
  for (i in pull(unique(treatment[, treatment_type]))) { ### NEED TO WORK ON THIS
    x_hfr_out[, i] <- subset(x_hfr, type == i)[order(subset(x_hfr, type == i)$date)]$ppp
  }

  rm(list = c("x_hfr"))

  x_hfr_c <- rbind(hyperframe(date = pull(treatment_dates_types_c[, 1]),
                              type = pull(treatment_dates_types_c[, 2]),
                              ppp = x_c_ppp),
                   hyperframe(date = missing_date_type_c$date,
                              type = missing_date_type_c$type,
                              ppp = empty_ppp)
  )
  x_hfr_out_c <- hyperframe(date = all_dates,
                            all_treatment = x_hfr_c[order(x_hfr_c$date)]$ppp)

  x_hyperframe <- cbind.hyperframe(x_hfr_out, x_hfr_out_c[, -1])

  rm(list = c("x_hfr_out_c"))

  # Converting outcome data to ppp ----------
  cat("Converting the outcome data to ppp objects...\n")

  ## Converting data to point process
  y_ppp <- outcome %>%
    group_by_at(vars(date, paste0(outcome_type))) %>%
    group_map(~as.ppp(cbind(x = .x[, coordinates[1]],
                            y = .x[, coordinates[2]]),
                      W = window))

  ## Generating data and point process with all treatment combined
  y_c <- outcome
  y_c[, outcome_type] <- "all_outcome"
  y_c_ppp <- y_c %>%
    group_by_at(vars(date, paste0(outcome_type))) %>%
    group_map(~as.ppp(cbind(x = .x[, coordinates[1]],
                            y = .x[, coordinates[2]]),
                      W = window))

  if (jitter) {
    y_ppp <- purrr::map(y_ppp, spatstat.geom::rjitter, radius = jitter_amount)
    y_c_ppp <- purrr::map(y_c_ppp, spatstat.geom::rjitter, radius = jitter_amount)
  }

  cat("Converting the outcome data to a hyperframe...\n")

  outcome_dates_types_y <- outcome %>%
    group_by_at(vars(date, paste0(outcome_type))) %>%
    dplyr::select(date, paste0(outcome_type)) %>%
    distinct()

  y_hfr <- hyperframe(date = pull(outcome_dates_types_y[, 1]),
                      type = pull(outcome_dates_types_y[, 2]),
                      ppp = y_ppp)

  outcome_dates_types_c_y <- y_c %>%
    group_by_at(vars(date, paste0(outcome_type))) %>%
    dplyr::select(date, paste0(outcome_type)) %>%
    distinct()

  y_hfr_c <- hyperframe(date = pull(outcome_dates_types_c_y[, 1]),
                        type = pull(outcome_dates_types_c_y[, 2]),
                        ppp = y_c_ppp)

  ## Identifying missing dates
  outcome_dates_types_y <- outcome %>%
    group_by_at(vars(date, paste0(outcome_type))) %>%
    dplyr::select(date, paste0(outcome_type)) %>%
    distinct()

  outcome_dates_types_c_y <- y_c %>%
    group_by_at(vars(date, paste0(outcome_type))) %>%
    dplyr::select(date, paste0(outcome_type)) %>%
    distinct()

  all_date_type_y <- expand.grid(date = all_dates, type = pull(unique(outcome[, outcome_type])))
  obs_date_type_y <- data.frame(date = pull(outcome_dates_types_y[, 1]),
                                type = pull(outcome_dates_types_y[, 2]))
  missing_date_type_y <- setdiff(all_date_type_y, obs_date_type_y)

  all_date_type_c_y <- data.frame(date = all_dates, type = pull(unique(y_c[, outcome_type])))
  obs_date_type_c_y <- data.frame(date = pull(outcome_dates_types_c_y[, 1]),
                                  type = pull(outcome_dates_types_c_y[, 2]))
  missing_date_type_c_y <- setdiff(all_date_type_c_y, obs_date_type_c_y)

  ## Combining observed and missing data
  y_hfr <- rbind(hyperframe(date = pull(outcome_dates_types_y[, 1]),
                            type = pull(outcome_dates_types_y[, 2]),
                            ppp = y_ppp),
                 hyperframe(date = missing_date_type_y$date,
                            type = missing_date_type_y$type,
                            ppp = empty_ppp)
  )

  y_hfr_out <- hyperframe(date = all_dates)
  for (i in pull(unique(outcome[, outcome_type]))) {
    y_hfr_out[, i] <- subset(y_hfr, type == i)[order(subset(y_hfr, type == i)$date)]$ppp
  }

  rm(list = c("y_hfr"))

  y_hfr_c <- rbind(hyperframe(date = pull(outcome_dates_types_c_y[, 1]),
                              type = pull(outcome_dates_types_c_y[, 2]),
                              ppp = y_c_ppp),
                   hyperframe(date = missing_date_type_c_y$date,
                              type = missing_date_type_c_y$type,
                              ppp = empty_ppp)
  )
  y_hfr_out_c <- hyperframe(date = all_dates,
                            all_outcome = y_hfr_c[order(y_hfr_c$date)]$ppp)

  y_hyperframe <- cbind.hyperframe(y_hfr_out, y_hfr_out_c[, -1])

  rm(list = c("y_hfr_out_c"))

  # Finalizing the hyperframe ----------
  cat("Generating a hyperframe of treatment and outcome point processes...\n")

  return(cbind.hyperframe(x_hyperframe, y_hyperframe[, -1]))

}
