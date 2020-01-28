updateROI <- function(old.roi, new.img) {
    image.in <- new.img
    raster::extent(image.in) <- c(0,1,0,1)
    old.roi.data <- old.roi
    roi.names <- names(old.roi)
    roi.data <- NULL
    nroi <- length(old.roi.data)
    for (i in seq(as.numeric(nroi))) {
        tmp.mask <- old.roi.data[[i]]$mask
        raster::extent(tmp.mask) <- c(0,1,0,1)
        res.mask <- resample(tmp.mask, image.in, method='ngb')    
        vertices <- NA
        masked.img <- res.mask
        raster::extent(masked.img) <- extent(new.img)
        out <- c(masked.img, vertices)
        names(out) <- c('mask', 'polygons')      
        roi.data[[i]] <- out
    }
    names(roi.data) <- roi.names
    return(roi.data)
}