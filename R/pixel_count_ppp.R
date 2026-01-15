#' Get number of events in a pixel
#'
#' @description
#' `pixel_count_ppp()` takes a column of hyperframes (ppp objects) and gets the number of events in each pixel.
#'
#' @param data the name of a hyperframe and column of interest.
#' @param W Optional window mask (object of class `"owin"`) determining the pixel raster.
#' `data` should be in the form of `"hyperframe$column"`.
#' @param resolution resolution of raster (distance map) (in km)
#' @param ndim the number of dimensions of grid cells (ndim^2). Users need to set either resolution or ndim.
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

pixel_count_ppp <- function(data, resolution = NULL,
                            ndim = NULL, W=NULL, weights = NULL,DivideByPixelArea = FALSE,...) {
  
  # Get window from first ppp object
  window <- if (!is.null(W)) W else data[[1]]$window
  
  
  # Determine output dimensions based on parameters
  if (!is.null(ndim)) {
    # Pixel mode: fixed dimensions
    dimyx <- c(ndim, ndim)
    message("Using pixel mode: ", ndim, "x", ndim, " pixels\n")
  } else if (!is.null(resolution)) {
    # Resolution mode: km per pixel
    x_extent <- diff(window$xrange)
    y_extent <- diff(window$yrange)
    nc <- ceiling(x_extent / resolution)
    nr <- ceiling(y_extent / resolution)
    dimyx <- c(nr, nc)
    message("Using resolution mode: ", resolution, " km per pixel -> ", nr, "x", nc, " pixels\n")
  } else {
    # Default: 128x128
    dimyx <- c(128, 128)
    message("Using default dimensions: 128x128 pixels\n")
  }
  
  # avoid passing W twice if user included it in ...
  dots <- list(...)
  if ("W" %in% names(dots)) {
    stop("Do not pass `W` via `...`; use the explicit `W =` argument.")
  }
  if ("dimyx" %in% names(dots)) {
    stop("Do not pass `dimyx` via `...`; use `resolution`/`ndim` instead.")
  }
  
  pixel_count <- furrr::future_map(data, spatstat.geom::pixellate.ppp,dimyx = dimyx,W = W, weights = weights, DivideByPixelArea = DivideByPixelArea,...)
  
  return(pixel_count)
  
}