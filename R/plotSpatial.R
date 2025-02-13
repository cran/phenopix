plotSpatial <- function(data, param, roi.data.path, image.path, probs=c(0.01, 0.99), ...) {
	roi.data <- NULL
	load(roi.data.path)
	if (is.data.frame(data)) {
		r <- raster::flip(brick(image.path))
		plotRGB(r)
		the.data <- data[,param]
		quantiles <- quantile(the.data, probs, na.rm=TRUE)
		the.data[the.data<quantiles[1]] <- NA
		the.data[the.data>quantiles[2]] <- NA
		phase <- roi.data[[1]]$mask
		raster::values(phase)[values(roi.data[[1]]$mask) == 0] <- NA
		raster::values(phase)[values(roi.data[[1]]$mask) ==1] <- the.data	
		suppressWarnings(plot(phase, add=TRUE, ...))
	} else {
	## match data and roi names
		matched <- match(names(data), names(roi.data))
		roi.data.ordered <- roi.data[matched]
		roi.num <- length(roi.data.ordered)
		x <- list()
		for (a in 1:roi.num) {
			x.tmp <- data[[a]][, param]
			x[[a]] <- x.tmp
		}
		r <- raster::flip(brick(image.path))
		plotRGB(r)
		phase <- roi.data[[1]]$mask
		raster::values(phase) <- NA
		for (a in 1:roi.num) {
		the.data <- x[[a]]
		quantiles <- quantile(the.data, probs, na.rm=TRUE)
		the.data[the.data<quantiles[1]] <- NA
		the.data[the.data>quantiles[2]] <- NA
		raster::values(phase)[values(roi.data.ordered[[a]]$mask) ==1] <- the.data	
		}
		suppressWarnings(plot(phase, add=TRUE, ...))
	}
}
