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
    get_jitter <- function(x) {
      spatstat.geom::rjitter(x, radius = jitter_amount)
    }
    x_ppp <- lapply(x_ppp, get_jitter)
    x_c_ppp <- lapply(x_c_ppp, get_jitter)
  }

  cat("Converting the treatment data to a hyperframe...\n")

  x_hfr <- hyperframe(date = pull(unique(treatment %>%
                                           dplyr::select(date, paste0(treatment_type)))[, date]),
                      type = pull(unique(treatment %>%
                                           dplyr::select(date, paste0(treatment_type)))[, treatment_type]),
                      ppp = x_ppp)

  x_hfr_c <- hyperframe(date = pull(unique(x_c %>% dplyr::select(date))[, date]),
                        type = pull(unique(x_c[, treatment_type])),
                        ppp = x_c_ppp)

  ## Identifying missing dates
  all_date_type <- expand.grid(date = all_dates, type = pull(unique(treatment[, treatment_type])))
  obs_date_type <- data.frame(date = pull(unique(treatment %>%
                                                   dplyr::select(date, paste0(treatment_type)))[, date]),
                              type = pull(unique(treatment %>%
                                                   dplyr::select(date, paste0(treatment_type)))[, treatment_type]))
  missing_date_type <- setdiff(all_date_type, obs_date_type)

  x_missing <- hyperframe(date = missing_date_type$date,
                          type = missing_date_type$type,
                          ppp = empty_ppp)

  all_date_type_c <- data.frame(date = all_dates, type = pull(unique(x_c[, treatment_type])))
  obs_date_type_c <- data.frame(date = pull(unique(x_c %>% dplyr::select(date))[, date]),
                                type = pull(unique(x_c[, treatment_type])))
  missing_date_type_c <- setdiff(all_date_type_c, obs_date_type_c)

  x_missing_c <- hyperframe(date = missing_date_type_c$date,
                            type = missing_date_type_c$type,
                            ppp = empty_ppp)

  ## Combining observed and missing data
  x_hfr <- rbind(x_hfr, x_missing)
  types <- pull(unique(treatment[, treatment_type]))

  get_subset_ppp <- function(i, data_original, type_column, data_to_subset){
    types <- pull(unique(data_original[, type_column]))
    temp_type <- types[i]
    temp_subset <- subset(data_to_subset, type == temp_type)
    return(temp_subset[order(temp_subset$date)]$ppp)
  }

  d <- 1:length(pull(unique(treatment[, treatment_type])))
  temp_subset_ppp <- lapply(d, get_subset_ppp, treatment, treatment_type, x_hfr)

  x_hfr_out <- hyperframe(date = all_dates)
  for (i in d) {
    x_hfr_out[, paste0(types[i])] <- temp_subset_ppp[[i]]
  }

  x_hfr_c <- rbind(x_hfr_c, x_missing_c)
  x_hfr_out_c <- hyperframe(date = all_dates,
                            all_treatment = x_hfr_c[order(x_hfr_c$date)]$ppp)

  x_hyperframe <- cbind.hyperframe(x_hfr_out, x_hfr_out_c[, -1])

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
    get_jitter <- function(x) {
      spatstat.geom::rjitter(x, radius = jitter_amount)
    }
    y_ppp <- lapply(y_ppp, get_jitter)
    y_c_ppp <- lapply(y_c_ppp, get_jitter)
  }

  cat("Converting the outcome data to a hyperframe...\n")

  y_hfr <- hyperframe(date = pull(unique(outcome %>%
                                           dplyr::select(date, paste0(outcome_type)))[, date]),
                      type = pull(unique(outcome %>%
                                           dplyr::select(date, paste0(outcome_type)))[, outcome_type]),
                      ppp = y_ppp)

  y_hfr_c <- hyperframe(date = pull(unique(y_c %>% dplyr::select(date))[, date]),
                        type = pull(unique(y_c[, outcome_type])),
                        ppp = y_c_ppp)

  ## Identifying missing dates
  all_date_type <- expand.grid(date = all_dates, type = pull(unique(outcome[, outcome_type])))
  obs_date_type <- data.frame(date = pull(unique(outcome %>%
                                                   dplyr::select(date, paste0(outcome_type)))[, date]),
                              type = pull(unique(outcome %>%
                                                   dplyr::select(date, paste0(outcome_type)))[, outcome_type]))
  missing_date_type <- setdiff(all_date_type, obs_date_type)

  y_missing <- hyperframe(date = missing_date_type$date,
                          type = missing_date_type$type,
                          ppp = empty_ppp)

  all_date_type_c <- data.frame(date = all_dates, type = pull(unique(y_c[, outcome_type])))
  obs_date_type_c <- data.frame(date = pull(unique(y_c %>% dplyr::select(date))[, date]),
                                type = pull(unique(y_c[, outcome_type])))
  missing_date_type_c <- setdiff(all_date_type_c, obs_date_type_c)

  y_missing_c <- hyperframe(date = missing_date_type_c$date,
                            type = missing_date_type_c$type,
                            ppp = empty_ppp)

  ## Combining observed and missing data
  y_hfr <- rbind(y_hfr, y_missing)
  types <- pull(unique(outcome[, outcome_type]))

  d <- 1:length(pull(unique(outcome[, outcome_type])))
  temp_subset_ppp <- lapply(d, get_subset_ppp, outcome, outcome_type, x_hfr)

  y_hfr_out <- hyperframe(date = all_dates)
  for (i in d) {
    y_hfr_out[, paste0(types[i])] <- temp_subset_ppp[[i]]
  }

  y_hfr_c <- rbind(y_hfr_c, y_missing_c)
  y_hfr_out_c <- hyperframe(date = all_dates,
                            all_outcome = y_hfr_c[order(y_hfr_c$date)]$ppp)

  y_hyperframe <- cbind.hyperframe(y_hfr_out, y_hfr_out_c[, -1])

  # Finalizing the hyperframe ----------
  cat("Generating a hyperframe of treatment and outcome point processes...\n")

  return(cbind.hyperframe(x_hyperframe, y_hyperframe[, -1]))

}
