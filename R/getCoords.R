getCoords <- function(image) {
	.plotImage <- function(image, ...) {
		ncols <- ncol(image)
		nrows <- nrow(image)
		suppressWarnings(plot(0, type='n', xlim=c(0, ncols), ylim=c(0, nrows), ...))
		suppressWarnings(rasterImage(image, xleft=0, ybottom=0, xright=ncols, ytop=nrows, ...))
	}
		.binaryConvert <- function(img) {
		grey.image <- 0.2126*img[,,1] + 0.7152*img[,,2] + 0.0722*img[,,3]
		binary <- round(grey.image, 0)
		rev.binary <- ifelse(binary==1, 0, 1)
		return(rev.binary)
    }
	image.target <- readJPEG(image)
	image.target <- .binaryConvert(image.target)
	image.width <- dim(image.target)[2]
	image.height <- dim(image.target)[1]
	.plotImage(image.target, main='Click the bottomright margin of the rectangle you want to crop')
	vertices <- locator()
	x.cuts <- c(0,round(vertices$x))
	y.cuts <- c(0, image.height-round(vertices$y))
	cropped <- image.target[y.cuts[1]:y.cuts[2], x.cuts[1]:x.cuts[2]]
	cropped.binary <- round(cropped)
	cropped.height <- dim(cropped.binary)[1]
	.plotImage(cropped.binary, interpolate=FALSE)
	vertices <- locator()
	col.cut <- sort(round(c(vertices$x[1],vertices$x[2]))) 
	row.cut <- sort(cropped.height - round(c(vertices$y[1], vertices$y[2]))) 
	neg.pos.col <- which(col.cut<0)
	if (length(neg.pos.col)!=0) col.cut[neg.pos.col] <- 0
	neg.pos.row <- which(row.cut<0)
	if (length(neg.pos.row)!=0) row.cut[neg.pos.row] <- 0
	coords <- c(col.cut, row.cut)
	names(coords) <- c('x1', 'x2', 'y1', 'y2')
	final.crop <- cropped.binary[coords['y1']:coords['y2'], coords['x1']:coords['x2']]
	.plotImage(final.crop, interpolate=FALSE,
		main='Make sure you have enough space on the right of the number for 4 digits \n and also enough space on the left for string shift \n which only occurs if Exposure is not close to the left margin!')
	abline(v=seq(0, 300, 10), col='blue')
	abline(h=0:100, col='blue')
	return(coords)
}
