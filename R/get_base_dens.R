#' Get the baseline density
#'
#' @description `get_base_dens()` takes a dataframe and
#' returns the baseline densities using Scott's rule of thumb (out-of-sample data)
#' or fitting an inhomogeneous Poisson model (in-sample data) by regressing
#' the in-sample data on time-invariant covariates.
#'
#' @param window owin object
#' @param option "in" (using in-sample data) or "out" (using out-of-sample data)
#' @param ndim the number of dimensions of grid cells (ndim^2). By default, ndim = 256.
#' @param out_data dataframe (if using out-of-sample data)
#' @param out_coordinates vector of column names of longitudes and latitudes (in this order) (if using in-sample data)
#' @param hfr hyperframe (if using in-sample data)
#' @param dep_var the name of the dependent variable (if using in-sample data)
#' @param indep_var the names of time-invariant independent varaibles (if using in-sample data)
#' @param ratio for random sampling of data (if using in-sample data)

#'
#' @returns an im object of baseline density

get_base_dens <- function(window,
                          option,
                          ndim = 256,
                          out_data,
                          out_coordinates = c("longitude", "latitude"),
                          hfr,
                          dep_var,
                          indep_var,
                          ratio
){

  # Option 1. Using out-of-sample data

  if (option == "out") {

    message("Using out-of-sample data to obtain the baseline density")

    # Convert out-of-sample data to ppp
    coordinates_data <- out_data[, out_coordinates]
    baseline_ppp <- spatstat.geom::as.ppp(coordinates_data, W = window)

    # Apply Scott's rule of thumb
    scott_bandwidth <- spatstat.explore::bw.scott(baseline_ppp)

    baseline_density <- stats::density(baseline_ppp, scott_bandwidth, dimyx = ndim) #Kernel density estimation
    baseline_density <- baseline_density / spatstat.univar::integral(baseline_density) #Divide by integral of the density

  } else if (option == "in") {

  # Option 2. Using in-sample data

    message("Using in-sample data to obtain the baseline density")

    num_obs <- round(ratio*nrow(hfr), digits = 0) #The number of obs to use
    ids <- sample(c(rep(1, num_obs), rep(0, nrow(hfr) - num_obs)))
    hfr <- hfr[which(ids == 1), ]

    text_form <- paste0(dep_var, " ~ ", paste(indep_var, collapse = " + "))

    mod <- spatstat.model::mppm(as.formula(text_form), data = hfr, nd = ndim) #Use mppm
    pred <- spatstat.model::predict.mppm(mod, ngrid = ndim, type = "cif")[1, ] #Pick the first one (all identical)
    baseline_density <- pred[["cif"]] / spatstat.univar::integral(pred[["cif"]])

  }

  return(baseline_density)

}
