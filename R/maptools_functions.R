# maptools functions

maptools_as.im.RasterLayer <- function(from, factor.col.name = NULL) {
  
  rs <- raster::res(from)
  orig <- sp::bbox(from)[, 1] + 0.5 * rs
  dm <- dim(from)[2:1]
  xx <- unname(orig[1] + cumsum(c(0, rep(rs[1], dm[1]-1))))
  yy <- unname(orig[2] + cumsum(c(0, rep(rs[2], dm[2]-1))))
  val <- raster::values(from)
  if(is.factor(from)){
    lev <- levels(from)[[1]]
    if(!is.null(factor.col.name)){
      if(factor.col.name %in% colnames(lev)){
        factor.col <- which(colnames(lev) == factor.col.name)
      } else {
        stop("'factor.col.name' is not a column name of the raster 'from'")
      }
    }else{
      factor.col <- length(lev)
    }
    val <- factor(val, levels = lev$ID, labels = lev[[factor.col]])
  }
  ## Assign dimensions to `val` as a matrix in raster layout:
  dim(val) <- dm
  ## Transform to spatstat format
  val <- spatstat.geom::transmat(val, from = list(x="-i", y="j"), to = "spatstat")
  im <- spatstat.geom::im(val, xcol=xx, yrow=yy)
  return(im)
}
