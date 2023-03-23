#' Function: get_hfr
#'
#' A function that takes dataframes with coordinates
#' and generates a hyperframe with point patterns
#'
#' @param data Data to convert
#' @param subtype_column Name of the column that speficies subtypes
#' @param window Window, should be saved as an owin object
#' @param date Name of the column for dates in the treatment dataset
#' @param date_range Range of dates
#' @param coordinates Names of columns for coordinates. By default, x = longitude and y = latitude
#' @param combined Whether to generate output for all treatment and all outcomes combined. By default TRUE

get_hfr <- function(data, subtype_column,
                    window,
                    date = "date",
                    date_range = c("2001-01-01", "2001-12-31"),
                    coordinates = c("longitude", "latitude"),
                    combined = TRUE) {

  # Getting the range of dates -----------
  all_dates <- seq(as.Date(date_range[1]), as.Date(date_range[2]), by = 1)

  # Cleaning the data ----------
  data <- data %>%
    dplyr::select(date, subtype_column, coordinates[1], coordinates[2])
  colnames(data) <- c("date", "type", "longitude", "latitude")
  setDT(data)

  if (combined){
    data_c <- data
    data_c[, "type"] <- "all_combined"
    data <- rbind(data, data_c)
  }

  # Converting data to ppp ----------
  cat("Converting the data to ppp objects...\n")

  ## Creating empty point process for imputation
  empty <- data.table(latitude = double(), longitude = double())
  empty_ppp <- as.ppp(cbind(x = empty$longitude, y = empty$latitude), W = window)

  ## Converting data to point process
  as.list(
    data[, .(list(as.ppp(cbind(x = .SD$longitude, y = .SD$latitude),
                         W = window))),
         by = list(date, type)]
  ) -> x_ppp

  cat("Converting the data to a hyperframe...\n")

  ## Identifying missing dates
  all_date_type <- expand.grid(date = all_dates,
                               type = unique(data[, type]))
  obs_date_type <- data.table(date = x_ppp$date,
                              type = x_ppp$type)
  missing_date_type <- setdiff(all_date_type, obs_date_type)

  ## Combining observed and missing data
  x_list <- c(x_ppp$V1, rep(list(empty_ppp), nrow(missing_date_type)))
  x_index <- rbind(obs_date_type, missing_date_type)
  data_types <- unique(data[, type])

  lapply(1:length(data_types),
         function(ii){
           x_list[which(x_index$type == data_types[ii])]
         }) -> x_list

  x_hyperframe <- hyperframe(date = all_dates)

  for(jj in 1:length(data_types)){
    x_hyperframe[, data_types[jj]] <-
      x_list[[jj]][order(subset(x_index, type == data_types[[jj]])$date)]
  }

  # Finalizing the hyperframe ----------
  cat("Generating a hyperframe of point processes...\n")

  return(x_hyperframe)

}
