#' convert a list of im objects to a vector
#'
#' @description
#' `get_em_vec()` get the vector form of a column of a hyperframe that summarizes the effect modifier data in heterogeneity analysis
#'
#' @param em column of a hyperframe that summarizes effect modifier data. In the form of `hyperframe$column`.
#' @param time_after whether to include one unit time difference between treatment and outcome. By default = TRUE
#' @param lag integer that specifies lags to calculate causal estimates
#' @param entire_window owin object (the entire region of interest). If given, then the values outside the region will be set to `NA`.
#' @param ngrid a number or a vector of two numbers that specify the dimension of pixels. If NULL, the pixel dimension of the original
#' images will not be changed
#' 
#' @details The function `get_em_vec()` get the vector form of the effect modifier in the heterogeneity analysis. It is useful
#' if you want to construct the variance matrix `E_mat` that is passed to the function `get_cate()`


get_em_vec <- function(em,time_after = TRUE,lag,entire_window = NULL,ngrid=NULL){
  if(length(ngrid)==1){
    ngrid <- rep(ngrid,2)
  }
  l <- length(em)
  res <- imls_to_arr(em,start=1,end=l-time_after-lag+1,entire_window,ngrid = ngrid)
  res <- c(res)
  
  return(res)
}

