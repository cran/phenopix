splitROI <- function(roi.data, nsplit, names=NULL) {
  original.vector <- values(roi.data[[1]]$mask)
  npixels <- length(which(original.vector==1))
  one.positions <- which(original.vector==1)
  out.npixels <- floor(npixels/nsplit)
  the.seq <- seq(1, length(one.positions), by=out.npixels)
  the.seq[length(the.seq)] <- length(one.positions)
  splits <- rep(roi.data, nsplit)
  if (is.null(names)) names(splits) <- paste0('roi', 1:nsplit)
    blank.container <- rep(0, length(original.vector))
  for (a in 1:(nsplit)) {
    raster::values(splits[[a]]$mask) <- blank.container
    if (a != nsplit)  raster::values(splits[[a]]$mask)[one.positions[the.seq[a]:(the.seq[a+1]-1)]] <- 1  else {
      raster::values(splits[[a]]$mask)[one.positions[the.seq[a]:(the.seq[a+1])]] <- 1
    }  
  }
  return(splits)
}
