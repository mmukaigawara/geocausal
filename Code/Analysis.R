library(spatstat)

setwd("~/Dropbox/Research/geocausal.package/")

# Load functions

# Load data

## This dataset is obtained via GetAnalysisData function (for example, see get_ps.R)

##set.seed(1234)

##dta <- GetAnalysisData(subset_dates = subset_dates, use_lags = use_lags,
##                       sof_priority_coef = sof_priority_coef,
##                       hist_priority_coefs = hist_priority_coefs,
##                       covs_priority_coefs = covs_priority_coefs,
##                       df_time = df_time, jitter = jitter,
##                       jitter_amount = jitter_amount)

##ps_dta <- subset(dta$ps_dta, date %in% dta$keep_dates)

load("~/Dropbox/Research/geocausal.package/Data/ps_dta.RData")
source('~/Dropbox/Research/geocausal.package/Function/function_counterfactual.R')

# List of covariates
use_lags <- c(1, 7, 30) #Lags for lagged interventions
df_time <- 3
ps_covs <- c('logPopulation', 'prior_rivers', 'prior_roads', 'All_Cities', 'aid',
             sapply(c('prior_PrevKinetic', 'prior_PrevAttacks'),
                    function(x) paste0(x, '.', use_lags)),
             sapply('prior_PrevSOF', function(x) paste0(x, '.', use_lags)),
             paste0('time.', 1 : df_time),
             paste0('Settle.IQ.G0', 1 : 9),
             paste0('Settle.IQ.G', 10 : 18)) #List of covariates

# Obtain actual and counterfactual distributions
counter.out <- counterfactual(outcome = "Kinetic",
                              covariates = ps_covs,
                              data = ps_dta,
                              newdata = data,
                              counter = 3,
                              multiple = TRUE)

# Plot actual and counterfactual distributions
plot(counter.out[[1]][[5]], main = "Actual distribution") #Actual
plot(counter.out[[2]][[5]], main = "Counterfactual distribution (c = 3)") #Counterfactual
