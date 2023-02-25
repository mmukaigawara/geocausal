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
#' @param combined Whether to generate output for all treatment and all outcomes combined. By default TRUE

get_hfr <- function(treatment, treatment_type,
                    outcome, outcome_type, window,
                    date = "date",
                    coordinates = c("longitude", "latitude"),
                    combined = TRUE) {

  # Getting the range of dates -----------
  all_dates <- seq(min(treatment$date), max(treatment$date), by = 1)

  # Cleaning the data ----------
  treatment <- treatment %>%
    dplyr::select(date, treatment_type, coordinates[1], coordinates[2])
  outcome <- outcome %>%
    dplyr::select(date, outcome_type, coordinates[1], coordinates[2])

  colnames(treatment) <- c("date", "type", "longitude", "latitude")
  colnames(outcome) <- c("date", "type", "longitude", "latitude")

  setDT(treatment)
  setDT(outcome)

  if (combined){
    treatment_c <- treatment
    treatment_c[, "type"] <- "all_treatment"
    treatment <- rbind(treatment, treatment_c)

    outcome_c <- outcome
    outcome_c[, "type"] <- "all_outcome"
    outcome <- rbind(outcome, outcome_c)
  }

  # Converting treatment data to ppp ----------
  cat("Converting the treatment data to ppp objects...\n")

  ## Creating empty point process for imputation
  empty <- data.table(latitude = double(), longitude = double())
  empty_ppp <- as.ppp(cbind(x = empty$longitude, y = empty$latitude), W = window)

  ## Converting data to point process
  as.list(
    treatment[, .(list(as.ppp(cbind(x = .SD$longitude,
                                    y = .SD$latitude),
                              W = window))),
              by = list(date, type)]
  ) -> x_ppp

  cat("Converting the treatment data to a hyperframe...\n")

  ## Identifying missing dates
  all_date_type <- expand.grid(date = all_dates,
                               type = unique(treatment[, type]))
  obs_date_type <- data.table(date = x_ppp$date,
                              type = x_ppp$type)
  missing_date_type <- setdiff(all_date_type, obs_date_type)

  ## Combining observed and missing data
  x_list <- c(x_ppp$V1, rep(list(empty_ppp), nrow(missing_date_type)))
  x_index <- rbind(obs_date_type, missing_date_type)
  treatment_types <- unique(treatment[, type])

  lapply(1:length(treatment_types),
         function(ii){
           x_list[which(x_index$type == treatment_types[ii])]
         }) -> x_list

  x_hyperframe <- hyperframe(date = all_dates)

  for(jj in 1:length(treatment_types)){
    x_hyperframe[, treatment_types[jj]] <-
      x_list[[jj]][order(subset(x_index, type == treatment_types[[jj]])$date)]
  }

  # Converting outcome data to ppp ----------
  cat("Converting the outcome data to ppp objects...\n")

  ## Converting data to point process
  as.list(
    outcome[, .(list(as.ppp(cbind(x = .SD$longitude,
                                  y = .SD$latitude),
                            W = window))),
            by = list(date, type)]
  ) -> y_ppp

  cat("Converting the outcome data to a hyperframe...\n")

  ## Identifying missing dates
  all_date_type2 <- expand.grid(date = all_dates,
                                type = unique(outcome[, type]))
  obs_date_type2 <- data.table(date = y_ppp$date,
                               type = y_ppp$type)
  missing_date_type2 <- setdiff(all_date_type2, obs_date_type2)

  ## Combining observed and missing data
  y_list <- c(y_ppp$V1, rep(list(empty_ppp), nrow(missing_date_type2)))
  y_index <- rbind(obs_date_type2, missing_date_type2)
  outcome_types <- unique(outcome[, type])

  lapply(1:length(outcome_types),
         function(ii){
           y_list[which(y_index$type == outcome_types[ii])]
         }) -> y_list

  y_hyperframe <- hyperframe(date = all_dates)

  for(jj in 1:length(outcome_types)){
    y_hyperframe[, outcome_types[jj]] <-
      y_list[[jj]][order(subset(y_index, type == outcome_types[[jj]])$date)]
  }

  # Finalizing the hyperframe ----------
  cat("Generating a hyperframe of treatment and outcome point processes...\n")

  return(cbind.hyperframe(x_hyperframe, y_hyperframe[, -1]))

}
