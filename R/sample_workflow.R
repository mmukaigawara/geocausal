# Sample workflow

library(readr)
library(dplyr)
library(spatstat)
library(data.table)
library(purrr)
library(furrr)
library(mclust)

# Generate iraq1 for cleaning purposes -----
iraq1 <- rgdal::readOGR(dsn = paste0(load_path, "Map_data/Map_Level1/."))
iraq1@data$id <- iraq1@data$admin1Name
iraq1.points <- fortify(iraq1, region = "id")
iraq1.df <- plyr::join(iraq1.points, iraq1@data, by = "id")
all_iraq <- st_union(st_as_sf(iraq1))
all_iraq.df <- st_coordinates(all_iraq)
all_iraq.df <- all_iraq.df[nrow(all_iraq.df) : 1, ]
iraq_window <- owin(poly = lapply(list(x = all_iraq.df[, 1], y = all_iraq.df[, 2]), rev))

# Load the original data -----
airstr <- read_csv("Application/Data/IraqAirstrikes.csv")
activ <- read_csv("Application/Data/SIGACTS.csv")
clean_data <- CleanData(airstr = airstr, activ = activ, iraq = iraq1,
                        subset_dates = subset_dates)
airstr <- clean_data$airstr
activ <- clean_data$activ

activ <- activ %>%
  dplyr::mutate(type_out = ifelse(type == "IED", "IED",
                                  ifelse(type == "SAF", "SAF", "other_outcome")))

# 1. Generate a hyperframe of treatment and outcome data -----

## Treatment
treatment_hfr <- get_hfr(data = airstr,
                         subtype_column = "Type",
                         window = iraq_window,
                         time = "date",
                         time_range = c("2007-02-23", "2008-07-05"),
                         coordinates = c("longitude", "latitude"),
                         combined = TRUE)

## Outcome
outcome_hfr <- get_hfr(data = activ,
                       subtype_column = "type_out",
                       window = iraq_window,
                       time_column = "date",
                       time_range = c("2007-02-23", "2008-07-05"),
                       coordinates = c("longitude", "latitude"),
                       combined = TRUE)

## Combine the two
dat_hfr <- cbind.hyperframe(treatment_hfr, outcome_hfr[, -1])

names(dat_hfr)[names(dat_hfr) == "all_combined"] <- "all_treatment"
names(dat_hfr)[names(dat_hfr) == "all_combined.1"] <- "all_outcome"

## Note: Airstrike = Kinetic, all_outcome = Activity, other_outcome = OtherAttack

## Generate history of treatment and outcomes
lags <- c(1, 7, 30)
time_interval <- 1:nrow(dat_hfr)

hist_kinetic_1 <- lapply(time_interval, get_history,
                         Xt = dat_hfr$Airstrike,
                         Yt = dat_hfr$all_outcome,
                         lag = lags[1],
                         window = iraq_window)

hist_kinetic_7 <- lapply(time_interval, get_history,
                         Xt = dat_hfr$Airstrike,
                         Yt = dat_hfr$all_outcome,
                         lag = lags[2],
                         window = iraq_window)

hist_kinetic_30 <- lapply(time_interval, get_history,
                          Xt = dat_hfr$Airstrike,
                          Yt = dat_hfr$all_outcome,
                          lag = lags[3],
                          window = iraq_window)

hist_SOF_1 <- lapply(time_interval, get_history,
                     Xt = ps_dta$SOF,
                     lag = lags[1],
                     window = iraq_window, x_only = TRUE)

hist_SOF_7 <- lapply(time_interval, get_history,
                     Xt = ps_dta$SOF,
                     lag = lags[2],
                     window = iraq_window, x_only = TRUE)

hist_SOF_30 <- lapply(time_interval, get_history,
                      Xt = ps_dta$SOF,
                      lag = lags[3],
                      window = iraq_window, x_only = TRUE)

dat_hfr$Airstrike_1 <- purrr::map(hist_kinetic_1, 1)
dat_hfr$Airstrike_7 <- purrr::map(hist_kinetic_7, 1)
dat_hfr$Airstrike_30 <- purrr::map(hist_kinetic_30, 1)
dat_hfr$SOF_1 <- purrr::map(hist_SOF_1, 1)
dat_hfr$SOF_7 <- purrr::map(hist_SOF_7, 1)
dat_hfr$SOF_30 <- purrr::map(hist_SOF_30, 1)
dat_hfr$all_outcome_1 <- purrr::map(hist_kinetic_1, 2)
dat_hfr$all_outcome_7 <- purrr::map(hist_kinetic_7, 2)
dat_hfr$all_outcome_30 <- purrr::map(hist_kinetic_30, 2)


## Add covariates (from GetAnalysisData after ReformData)
load(paste0(load_path, 'image_covariates/cities_dist.dat'))
load(paste0(load_path, 'image_covariates/ethnicity.dat'))
load(paste0(load_path, 'image_covariates/rivers_dist.dat'))
load(paste0(load_path, 'image_covariates/routes_dist.dat'))
load(paste0(load_path, 'image_covariates/settle_dist.dat'))
load(paste0(load_path, 'covariates/spending_data/aid_district.dat'))

aid_district <- subset(aid_district, USE_DATE >= subset_dates[1] &
                         USE_DATE <= subset_dates[2])

## List of covariates
all_covariates <- list(cities_dist = cities_dist, #Hyperframe of ppp and image
                       ethnicity = ethnicity, #list of ppp and image
                       rivers_dist = rivers_dist, #im
                       routes_dist = routes_dist, #im
                       settle_dist = settle_dist, #hyperframe of ppp and image
                       aid_district = aid_district) #data table

# 2. Smoothing outcomes -----
smooth_IED <- get_smoothed_outcome(data_interest = dat_hfr$IED,
                                   method = "mclust", initialization = TRUE,
                                   sampling = 0.05)

smooth_SAF <- get_smoothed_outcome(data_interest = dat_hfr$SAF,
                                   method = "mclust", initialization = TRUE,
                                   sampling = 0.05)

smooth_other <- get_smoothed_outcome(data_interest = dat_hfr$other_outcome,
                                     method = "mclust", initialization = TRUE,
                                     sampling = 0.05)

smooth_allout <- get_smoothed_outcome(data_interest = dat_hfr$all_outcome,
                                      method = "mclust", initialization = TRUE,
                                      sampling = 0.05)

## Save the smoothed outcome as fitting is computationally demanding
#saveRDS(smooth_IED, file = "smooth_IED.RData")
#saveRDS(smooth_SAF, file = "smooth_SAF.RData")
#saveRDS(smooth_other, file = "smooth_other.RData")
#saveRDS(smooth_allout, file = "smooth_allout.RData")
#readRDS("smooth_IED.RData")

## Integrating outcomes
integrate_IED <- unlist(future_map(smooth_IED, integral, domain = iraq_window))
integrate_SAF <- unlist(future_map(smooth_SAF, integral, domain = iraq_window))
integrate_other <- unlist(future_map(smooth_other, integral, domain = iraq_window))
integrate_allout <- unlist(future_map(smooth_allout, integral, domain = iraq_window))

## Save the integrated counts as integration is computationally demanding (note: window = iraq_window)
#integrate_outcomes <- data.table(n_IED = integrate_IED, n_SAF = integrate_SAF,
#                                 n_other = integrate_other, n_allout = integrate_allout)
#saveRDS(integrate_outcomes,  "integrate_outcomes.rds")
#readRDS("integrate_outcomes.rds")
