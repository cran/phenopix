plotSpatial <- function(data, param, roi.data.path, image.path, probs=c(0.01, 0.99), ...) {
	img <- readJPEG(image.path)
	roi.data <- NULL
	load(roi.data.path)
	if (is.data.frame(data)) {
		r <- brick(image.path)
		plotRGB(r)
		the.data <- data[,param]
		quantiles <- quantile(the.data, probs, na.rm=TRUE)
		the.data[the.data<quantiles[1]] <- NA
		the.data[the.data>quantiles[2]] <- NA	
		phase.array <- array(dim=c(nrow(r), ncol(r),1))
		phase.array[,,1][roi.data[[1]]$pixels.in.roi$pip==1] <- the.data
		phase <- brick(phase.array, xmn=0, xmx=ncol(r), 
			ymn=0, ymx=nrow(r))
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
		r <- brick(image.path)
		plotRGB(r)
		phase.array <- array(dim=c(nrow(r), ncol(r),1))
		for (a in 1:roi.num) {
		the.data <- x[[a]]
		quantiles <- quantile(the.data, probs, na.rm=TRUE)
		the.data[the.data<quantiles[1]] <- NA
		the.data[the.data>quantiles[2]] <- NA
		phase.array[,,1][roi.data.ordered[[a]]$pixels.in.roi$pip==1] <- the.data
		}
		phase <- brick(phase.array, xmn=0, xmx=ncol(r), 
			ymn=0, ymx=nrow(r))
		suppressWarnings(plot(phase, add=TRUE, ...))
	}
}
