#' convert a list of im objects to a vector
#'
#' @description
#' `get_em_vec()` get the vector form of a column of a hyperframe that summarizes the effect modifier data in heterogeneity analysis
#'
#' @param em column of a hyperframe that summarizes effect modifier data. In the form of `hyperframe$column`.
#' @param outcome_pixel_count
#'   A list of integer-valued pixel images giving outcome event counts per spatial pixel,
#'   typically obtained from `pixel_count_ppp()`.
#' @param time_after whether to include one unit time difference between treatment and outcome. By default = TRUE
#' @param lag integer that specifies lags to calculate causal estimates
#' @param entire_window owin object (the entire region of interest). If given, then the values outside the region will be set to `NA`.
#'
#' @details The function `get_em_vec()` get the vector form of the effect modifier in the heterogeneity analysis. It is useful
#' if you want to construct the variance matrix `E_mat` that is passed to the function `get_cate()`
get_em_vec <- function(em,
                       outcome_pixel_count = NULL,
                       time_after = TRUE,
                       entire_window = NULL,
                       lag) {
  
  
  if (is.null(outcome_pixel_count)) {
    stop("`outcome_pixel_count` must be provided to determine the pixel grid.")
  }

  l <- length(em)
  dimyx <- dim(outcome_pixel_count[[1]])

  res <- imls_to_arr(em,
                     start = 1,
                     end = l - time_after - lag + 1,
                     entire_window = entire_window,
                     dimyx = dimyx)
  
  res <- c(res)
  return(res)
}
