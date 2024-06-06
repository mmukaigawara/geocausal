#' convert a list of im objects to a three-dimensional array
#'
#' @description
#' `imls_to_arr()` convert a list of im object to a 3D array
#'
#' @param imls a list of im objects (imlist)
#' @param start the index of the first im to be converted. Default is 1.
#' @param end the index of the last im to be converted. If not provided, then it will be set to the length of the list.
#' @param entire_window a owin object. If given, then the values outside the region will be set to `NA`
#' @param ngrid  an optional arugument that takes one integer or  vector of two integers specifying the dimensions of the `im` objects. If provided, the dimensions of the objects will be adjusted to `ngrid` before the conversion to the array.
#' 
#' @details `imls_to_arr()` is a internal function for `imls_to_vec()`. By default, it returns a three-dimensional array of dimension \eqn{n} by \eqn{m} by\eqn{l} where \eqn{n} and \eqn{m}
#' are the dimensions of the im objects, and \eqn{l} is the length of the list. All the im objects in the list need to have the same dimensions. 

imls_to_arr <- function(imls,start = 1,end = NULL,entire_window = NULL,ngrid = NULL){
  adjust_dim <- TRUE
  
  if(is.null(end)){
    end <- length(imls)
  }
  
  if(is.null(ngrid)){
    ngrid <- dim(imls[[1]])
    adjust_dim <- FALSE
  }
  if(length(ngrid)==1){
    ngrid <- rep(ngrid,2)
  }
    
  if(!is.null(entire_window)){
    window_matrix <- spatstat.geom::as.matrix.owin(entire_window,dimyx = ngrid)
    window_matrix[window_matrix==0] <- NA
    if(adjust_dim){
      imls <- lapply(start:end, function(x) spatstat.geom::as.matrix.im(spatstat.geom::as.im(imls[[x]],dimyx = ngrid))*window_matrix)
    }else{
      imls <- lapply(start:end, function(x) spatstat.geom::as.matrix.im(imls[[x]])*window_matrix)
    }
    
  }else{
    if(adjust_dim){
      imls <- lapply(start:end, function(x) spatstat.geom::as.matrix.im(spatstat.geom::as.im(imls[[x]],dimyx = ngrid)))
    }else{
      imls <- lapply(start:end, function(x) spatstat.geom::as.matrix.im(imls[[x]]))
    }
    
  }
  
  imls <- array(unlist(imls),c(dim(imls[[1]]),end-start+1))
  
  return(imls)
}

