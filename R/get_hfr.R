#' Function: get_hfr
#'
#' @description A function that takes dataframes with coordinates
#' and generates a hyperframe with point patterns
#'
#' @param data Data to convert
#' @param subtype_column Name of the column that specifies subtypes
#' @param window Window, which should be saved as an owin object
#' @param time_column Name of the column for time. By default, dates
#' @param time_range Range of time with min and max. By default, dates
#' @param coordinates Names of columns for coordinates. By default, x = longitude and y = latitude
#' @param combined Whether to generate output for all treatment and all outcomes combined. By default TRUE
#' 
#' @returns A hyperframe

get_hfr <- function(data, subtype_column,
                    window,
                    time_column,
                    time_range,
                    coordinates = c("longitude", "latitude"),
                    combined = TRUE) {

  # Getting the range of dates -----------
  all_time <- seq(as.Date(time_range[1]), as.Date(time_range[2]), by = 1)

  # Cleaning the data ----------
  data <- data %>%
    dplyr::select(time_column, subtype_column, coordinates[1], coordinates[2])
  colnames(data) <- c("time", "type", "longitude", "latitude")
  data.table::setDT(data)

  if (combined){
    data_c <- data
    data_c[, "type"] <- "all_combined"
    data <- rbind(data, data_c)
  }

  # Converting data to ppp ----------
  cat("Converting the data to ppp objects...\n")

  ## Creating empty point process for imputation
  empty <- data.table::data.table(latitude = double(), longitude = double())
  empty_ppp <- spatstat.geom::as.ppp(cbind(x = empty$longitude, y = empty$latitude), W = window)

  ## Converting data to point process
  as.list(
    data[, .(list(spatstat.geom::as.ppp(cbind(x = .SD$longitude, y = .SD$latitude),
                                        W = window))),
         by = list(time, type)]
  ) -> x_ppp

  cat("Converting the data to a hyperframe...\n")

  ## Identifying missing dates
  all_time_type <- expand.grid(time = all_time,
                               type = as.character(unique(data[, type])),
                               KEEP.OUT.ATTRS = FALSE)
  all_time_type$type <- as.character(all_time_type$type)
  data.table::setDT(all_time_type)

  obs_time_type <- data.table::data.table(time = x_ppp$time, type = x_ppp$type)

  missing_time_type <- all_time_type[!obs_time_type, on = c("time", "type")]

  ## Combining observed and missing data
  x_list <- c(x_ppp$V1, rep(list(empty_ppp), length(missing_time_type[[1]])))
  x_index <- rbind(obs_time_type, missing_time_type)
  data_types <- unique(data[, type])

  lapply(1:length(data_types),
         function(ii){
           x_list[which(x_index$type == data_types[ii])]
         }) -> x_list

  x_hyperframe <- spatstat.geom::hyperframe(time = all_time)

  for(jj in 1:length(data_types)){
    x_hyperframe[, data_types[jj]] <-
      x_list[[jj]][order(subset(x_index, type == data_types[[jj]])$time)]
  }

  # Finalizing the hyperframe ----------
  cat("Generating a hyperframe of point processes...\n")

  return(x_hyperframe)

}
