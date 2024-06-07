#' Get number of events in a pixel
#'
#' @description
#' `pixel_count_ppp()` takes a column of hyperframes (ppp objects) and gets the number of events in each pixel.
#'
#' @param data the name of a hyperframe and column of interest.
#' @param W Optional window mask (object of class `"owin"`) determining the pixel raster.
#' `data` should be in the form of `"hyperframe$column"`.
#' @param ngrid a number or a vector of two numbers specifying the pixel array dimensions. A single integer, or an integer vector of length 2 giving dimensions in the y and x directions.Default is `c(128,128)`.
#' @param weights Optional vector of weights associated with the points.
#' @param DivideByPixelArea Logical value determining whether the resulting pixel values should be devided by the pixel area. Default value is `False`.
#' @param ... parameters passed on to the function.
#'
#' @returns im objects
#'
#'
#' @examples
#' # Time variable
#' dat_out <- insurgencies[1:100, ]
#' dat_out$time <- as.numeric(dat_out$date - min(dat_out$date) + 1)
#'
#' # Hyperframe
#' dat_hfr <- get_hfr(data = dat_out,
#'                    col = "type",
#'                    window = iraq_window,
#'                    time_col = "time",
#'                    time_range = c(1, max(dat_out$time)),
#'                    coordinates = c("longitude", "latitude"),
#'                    combine = TRUE)
#'
#' # Get the number of events for each pixel
#' pixel_count_ppp(data = dat_hfr$all_combined)

pixel_count_ppp <- function(data,ngrid = c(128,128), W=NULL, weights = NULL,DivideByPixelArea = FALSE,...) {
  if(length(ngrid)==1){
    ngrid <- rep(ngrid,2)
  }
  pixel_count <- furrr::future_map(data, spatstat.geom::pixellate.ppp,dimyx = ngrid,W = W, weights = weights, DivideByPixelArea = DivideByPixelArea,...)
  
  return(pixel_count)
  
}