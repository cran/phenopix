updateROI <- function(old.roi, new.img) {
    img.converted <- new.img
    img.converted[, , 1] <- new.img[, , 1] * 255
    img.converted[, , 2] <- new.img[, , 2] * 255
    img.converted[, , 3] <- new.img[, , 3] * 255
    ratio <- dim(new.img)[1]/dim(new.img)[2]
    nrows <- nrow(new.img)
    ncols <- ncol(new.img)
    old.roi.data <- old.roi
    roi.names <- names(old.roi)
    roi.data <- NULL
    nroi <- length(old.roi.data)
    for (i in seq(as.numeric(nroi))) {
        vertices <- old.roi.data[[i]]$vertices
       coordinates <- data.frame(rowpos = ratio - vertices$y, 
            colpos = vertices$x)
        image.array <- expand.grid(rowpos = seq(1:nrow(new.img))/(nrow(new.img)/ratio), 
            colpos = seq(1:ncol(new.img))/ncol(new.img))
         pixels.in.roi <- pnt.in.poly(image.array, coordinates)
        ## dev.print(jpeg, file = paste(path_ROIs, "/ROI", i, "_", 
        ##     roi.names[i], ".jpg", sep = ""), width = 1024, height = 1024)
        ## dev.off()
        out <- list(pixels.in.roi, vertices)
        names(out) <- c("pixels.in.roi", "vertices")
        roi.data[[i]] <- out
    }
    names(roi.data) <- roi.names
    return(roi.data)
}
