#' Function: get_hfr
#'
#' A function that takes dataframes with coordinates
#' and generates a hyperframe with point patterns
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

  # Cleaning the data ----------
  treatment <- treatment %>% dplyr::select(date, treatment_type, coordinates[1], coordinates[2])
  outcome <- outcome %>% dplyr::select(date, outcome_type, coordinates[1], coordinates[2])

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
  all_date_type <- expand.grid(date = all_dates,
                               type = pull(unique(treatment[, treatment_type])))

  obs_date_type <- treatment %>%
    group_by_at(vars(date, paste0(treatment_type))) %>%
    dplyr::select(date, paste0(treatment_type)) %>%
    distinct() %>%
    rename(type = treatment_type)

  missing_date_type <- setdiff(all_date_type, obs_date_type)

  all_date_type_c <- data.frame(date = all_dates,
                                type = pull(unique(x_c[, treatment_type])))

  obs_date_type_c <- x_c %>%
    group_by_at(vars(date, paste0(treatment_type))) %>%
    dplyr::select(date, paste0(treatment_type)) %>%
    distinct() %>%
    rename(type = treatment_type)

  missing_date_type_c <- setdiff(all_date_type_c, obs_date_type_c)

  ## Combining observed and missing data
  x_list <- c(x_ppp, rep(list(empty_ppp), nrow(missing_date_type)))
  x_index <- rbind(obs_date_type, missing_date_type)
  treatment_types <- pull(unique(treatment[, treatment_type]))

  lapply(1:length(treatment_types),
         function(ii){
           x_list[which(x_index$type == treatment_types[ii])]
         }) -> x_list

  x_hyperframe <- hyperframe(date = all_dates)

  for(jj in 1:length(treatment_types)){
    x_hyperframe[, treatment_types[jj]] <-
      x_list[[jj]][order(subset(x_index, type == treatment_types[[1]])$date)]
  }

  x_list_c <- c(x_c_ppp, rep(list(empty_ppp), nrow(missing_date_type_c)))
  x_index_c <- rbind(obs_date_type_c, missing_date_type_c)

  x_hyperframe[, "all_treatment"] <- x_list_c[order(x_index_c$date)]

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
  all_date_type_y <- expand.grid(date = all_dates,
                                 type = pull(unique(outcome[, outcome_type])))

  obs_date_type_y <- outcome %>%
    group_by_at(vars(date, paste0(outcome_type))) %>%
    dplyr::select(date, paste0(outcome_type)) %>%
    distinct() %>%
    rename(type = outcome_type)

  missing_date_type_y <- setdiff(all_date_type_y, obs_date_type_y)

  all_date_type_c_y <- data.frame(date = all_dates,
                                  type = pull(unique(y_c[, outcome_type])))

  obs_date_type_c_y <- y_c %>%
    group_by_at(vars(date, paste0(outcome_type))) %>%
    dplyr::select(date, paste0(outcome_type)) %>%
    distinct() %>%
    rename(type = outcome_type)

  missing_date_type_c_y <- setdiff(all_date_type_c_y, obs_date_type_c_y)

  ## Combining observed and missing data
  y_list <- c(y_ppp, rep(list(empty_ppp), nrow(missing_date_type_y)))
  y_index <- rbind(obs_date_type_y, missing_date_type_y)
  outcome_types <- pull(unique(outcome[, outcome_type]))

  lapply(1:length(outcome_types),
         function(ii){
           y_list[which(y_index$type == outcome_types[ii])]
         }) -> y_list

  y_hyperframe <- hyperframe(date = all_dates)

  for(jj in 1:length(outcome_types)){
    y_hyperframe[, outcome_types[jj]] <-
      y_list[[jj]][order(subset(y_index, type == outcome_types[[1]])$date)]
  }

  y_list_c <- c(y_c_ppp, rep(list(empty_ppp), nrow(missing_date_type_c_y)))
  y_index_c <- rbind(obs_date_type_c_y, missing_date_type_c_y)

  y_hyperframe[, "all_outcome"] <- y_list_c[order(y_index_c$date)]

  # Finalizing the hyperframe ----------
  cat("Generating a hyperframe of treatment and outcome point processes...\n")

  return(cbind.hyperframe(x_hyperframe, y_hyperframe[, -1]))

}
