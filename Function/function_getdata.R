# Function: getdata

# This function does the following:
# 1. Let users to generate a hyperframe that can be used for analysis

### A copy of GetAnalysisData function in the Iraq paper below ###

# Specifications:
# outcome: an outcome variable
# covariates: a vector of names of covariates
# data: data to fit the poisson model
# multiple: choice of ppm or mppm

#' Getting the data we analyze in proper format.
#'
#' @param subset_dates Start and end date of the period we wish to analyze.
#' @param trt_lags
#' @param sof_priority_coef
#' @param hist_priority_coefs
#' @param covs_priority_coefs
#' @param df_time Number of degrees of freedom for splines.
#' @param get_history Logical, defaults to TRUE. If TRUE, the history over the
#' specified lags will be calculated. If FALSE, the history will not be
#' returned. Set to FALSE for acquiring only the treatment and outcome point
#' patterns faster.
#' @param get_covariates Logical. Like get_history but for the covariates.
#'
GetAnalysisData <- function(subset_dates, use_lags = c(1, 4, 7),
                            sof_priority_coef = 3,
                            hist_priority_coefs = c(3, 3),
                            covs_priority_coefs = NULL,
                            df_time = 3, clean_data = NULL,
                            iraq_window = NULL,
                            load_path = NULL, jitter = FALSE,
                            jitter_amount = 0.0001,
                            get_history = TRUE,
                            get_covariates = TRUE) {
  
  if (is.null(load_path)) {
    load_path <- '~/Dropbox/Research/spatiotemporal/Application/Data/'
  }
  
  use_lags <- unique(use_lags)
  if (is.null(covs_priority_coefs)) {
    covs_priority_coefs <- list(cities = seq(1.5, 6.3, length.out = 5),
                                rivers = 3, roads = 3, settle = 7.5)
  }
  
  
  # -------- Getting the Iraq map window ---------- #
  
  if (is.null(iraq_window) | is.null(clean_data)) {
    iraq1 <- rgdal::readOGR(dsn = paste0(load_path, "Map_data/Map_Level1/."))
    iraq1@data$id <- iraq1@data$admin1Name
    iraq1.points <- fortify(iraq1, region = "id")
    iraq1.df <- plyr::join(iraq1.points, iraq1@data, by = "id")
    all_iraq <- st_union(st_as_sf(iraq1))
    all_iraq.df <- st_coordinates(all_iraq)
    all_iraq.df <- all_iraq.df[nrow(all_iraq.df) : 1, ]
    iraq_window <- owin(poly = lapply(list(x = all_iraq.df[, 1], y = all_iraq.df[, 2]), rev))
  }
  
  
  # ------- Getting the airstrikes and attacks data --------- #
  
  if (is.null(clean_data)) {
    airstr <- read.csv(paste0(load_path, 'IraqAirstrikes.csv'))
    activ <- read.csv(paste0(load_path, 'SIGACTS.csv'))
    clean_data <- CleanData(airstr = airstr, activ = activ, iraq = iraq1,
                            subset_dates = subset_dates)
  }
  
  # Updating the subset dates if they were set too wide:
  subset_dates <- range(clean_data$airstr$date)
  
  print(subset_dates)
  print(diff(subset_dates))
  cat('Reforming data to point patterns. \n')
  dta <- ReformData(airstr = clean_data$airstr, activ = clean_data$activ,
                    iraq_window = iraq_window, jitter = jitter,
                    jitter_amount = jitter_amount)
  
  
  # ------- Getting the covariate data ------------ #
  
  # Loading the covariates.
  load(paste0(load_path, 'image_covariates/cities_dist.dat'))
  load(paste0(load_path, 'image_covariates/ethnicity.dat'))
  load(paste0(load_path, 'image_covariates/rivers_dist.dat'))
  load(paste0(load_path, 'image_covariates/routes_dist.dat'))
  load(paste0(load_path, 'image_covariates/settle_dist.dat'))
  load(paste0(load_path, 'covariates/spending_data/aid_district.dat'))
  
  aid_district <- subset(aid_district, USE_DATE >= subset_dates[1] &
                           USE_DATE <= subset_dates[2])
  
  all_covariates <- list(cities_dist = cities_dist,
                         ethnicity = ethnicity,
                         rivers_dist = rivers_dist,
                         routes_dist = routes_dist,
                         settle_dist = settle_dist,
                         aid_district = aid_district)
  
  
  # ------- Getting the data in hyperframe format --------- #
  
  cat('Getting history & adding covariates. \n')
  ps_dta <- GetPSdata(dta = dta, use_lags = use_lags,
                      all_covariates = all_covariates,
                      iraq_window = iraq_window,
                      sof_priority_coef = sof_priority_coef,
                      hist_priority_coefs = hist_priority_coefs,
                      covs_priority_coefs = covs_priority_coefs,
                      df_time = df_time, get_history = get_history,
                      get_covariates = get_covariates)
  
  
  # Excluding days that do not have the appropriate number of lags, and days
  # during the black out period.
  start1 <- ps_dta$date[[1]] + max(use_lags)
  end1 <- as.Date("2006-09-24")
  keep_dates <- NULL
  if (end1 > start1) {
    keep_dates <- seq(start1, end1, by = 1)
  }
  start2 <- as.Date("2007-02-23") + max(use_lags)
  end2 <- ps_dta$date[[nrow(ps_dta)]]
  if (end2 > start2) {
    keep_dates <- c(keep_dates, seq(start2, end2, by = 1))
  }
  
  return(list(ps_dta = ps_dta, keep_dates = keep_dates,
              iraq_window = iraq_window))
  
}