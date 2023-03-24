#' Function: get_smoothed
#'
#' A function that takes a column of hyperframes
#' and generates a smoothed ppp
#'
#' @param data_interest Data to convert; should be in the form of hyperframe$column
#' @param initialization Whether to use smaller samples to initialize mclust. By default = TRUE, using 5% of data
#' @param seed A seed for initialization. By default, 02138

get_smoothed_outcome <- function(data_interest,
                                 initialization = TRUE,
                                 seed = 02138) {

  # Obtain coordinates of interest -----
  all_points_coords <- rbindlist(map(data, as.data.frame.ppp))

  # Fit the mixture Gaussian model (mclust) -----

  ## Identify the number of components (EII model)
  cat("Fitting the mixture Gaussian model\n")

  if (initialization == TRUE) {

    set.seed(seed)

    # Prepare for initialization
    M <- round(nrow(all_points_coords)/20, digits = 0) #Use 5% for initialization
    init <- list(subset = sample(1:nrow(all_points_coords), size = M))

    # Fit the mixture Gaussian model
    BIC <- mclust::mclustBIC(all_points_coords, modelNames = c("EII"),
                             initialization = init)
    mod_mcl <- mclust::Mclust(all_points_coords, x = BIC, modelNames = "EII")

  } else {

    # Fit the model without initialization
    BIC <- mclust::mclustBIC(all_points_coords, modelNames = c("EII"))
    mod_mcl <- mclust::Mclust(all_points_coords, x = BIC, modelNames = "EII")
  }

  ## Get the value of sigma
  spat_sigma <- mod_mcl$parameters$variance
  spat_sigma <- sqrt(spat_sigma$sigmasq)

  ## Obtain smoothed outcomes
  cat("Smoothing ppps\n")

  smoothed_outcome <- lapply(data_interest, density.ppp, diggle = TRUE,
                             kernel = "gaussian", adjust = 1,
                             sigma = spat_sigma, edge = TRUE)

  return(smoothed_outcome)

}
