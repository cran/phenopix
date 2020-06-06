trainOCR <- function(image.path, nsamples=100) {
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
	numbers <- c('E','0', '1', '2', '3', '4', 
		'5', '6', '7', '8', '9')
	number.list <- list()
	for (a in 1:length(numbers)) {
		number.list[[a]] <- NA
	}
	names(number.list) <- numbers
	image.list <- list.files(image.path, full.names=TRUE, recursive=TRUE)
	image.samples <- sample(image.list, nsamples, replace=TRUE)
	a <- 1
	while(any(is.na(number.list))) {
		image.large <- readJPEG(image.samples[a])
		image.large <- .binaryConvert(image.large)
		image.height <- dim(image.large)[1]
		a <- a+1 
		missing <- names(number.list)[which(is.na(number.list))]
		collapsed.missing <- paste(missing, collapse=', ')
		message <- paste0('You are still missing\n', collapsed.missing)
		.plotImage(image.large, main='Click topleft and bottomright to crop \nor right-click to change image')
		vertices <- locator()
		if (is.null(vertices)) next()
			col.cut <- sort(round(c(vertices$x[1],vertices$x[2]))) 
		row.cut <- sort(image.height - round(c(vertices$y[1], vertices$y[2]))) 
		neg.pos.col <- which(col.cut<0)
		if (length(neg.pos.col)!=0) col.cut[neg.pos.col] <- 0
		neg.pos.row <- which(row.cut<0)
		if (length(neg.pos.row)!=0) row.cut[neg.pos.row] <- 0
		coords <- c(col.cut, row.cut)
		names(coords) <- c('x1', 'x2', 'y1', 'y2')
		cropped <- image.large[coords['y1']:coords['y2'], coords['x1']:coords['x2']]
		cropped.binary <- round(cropped)
		cropped.height <- dim(cropped.binary)[1]
		.plotImage(cropped.binary, main=message)
		vertices <- locator()
		if (is.null(vertices)) next()
			col.cut <- sort(round(c(vertices$x[1],vertices$x[2]))) 
		row.cut <- sort(cropped.height - round(c(vertices$y[1], vertices$y[2]))) 
		neg.pos.col <- which(col.cut<0)
		if (length(neg.pos.col)!=0) col.cut[neg.pos.col] <- 0
		neg.pos.row <- which(row.cut<0)
		if (length(neg.pos.row)!=0) row.cut[neg.pos.row] <- 0
		coords <- c(col.cut, row.cut)
		names(coords) <- c('x1', 'x2', 'y1', 'y2')
		cropped.binary2 <- cropped.binary[coords['y1']:coords['y2'], coords['x1']:coords['x2']]
		cropped2.height <- dim(cropped.binary2)[1]
		.plotImage(cropped.binary2, main=message, interpolate=FALSE)
		abline(v=1:100, col='blue')
		abline(h=1:100, col='blue')
		vertices <- locator()
		if (is.null(vertices)) next()
			col.cut <- sort(round(c(vertices$x[1],vertices$x[2]))) 
		row.cut <- sort(cropped2.height - round(c(vertices$y[1], vertices$y[2]))) 
		neg.pos.col <- which(col.cut<0)
		if (length(neg.pos.col)!=0) col.cut[neg.pos.col] <- 0
		neg.pos.row <- which(row.cut<0)
		if (length(neg.pos.row)!=0) row.cut[neg.pos.row] <- 0
		coords <- c(col.cut, row.cut)
		names(coords) <- c('x1', 'x2', 'y1', 'y2')
		which.number <- ask('Which number are you drawing?')
##clean number
		cropped.number <- cropped.binary2[coords['y1']:coords['y2'], coords['x1']:coords['x2']]
		crop.ncols <- ncol(cropped.number)
		crop.nrows <- nrow(cropped.number)
		white.rows <- which(apply(cropped.number,1,sum)==crop.ncols)
		white.cols <- which(apply(cropped.number,2,sum)==crop.nrows)
		if (length(white.rows)!=0) cleaned.number1 <- cropped.number[-white.rows, ] else cleaned.number1 <- cropped.number
		if (length(white.cols)!=0) cleaned.number2 <- cleaned.number1[,-white.cols] else cleaned.number2 <- cleaned.number1
		number.pos <- which(names(number.list)==which.number)
		number.list[[number.pos]] <- cleaned.number2
		image.samples <- image.samples[-a]
	}
	check.dim <- sapply(number.list, dim)
	if (length(unique(check.dim[1,]))!=1) warning('Letters do not have same height (n pixels). \nYou will encounter problems in OCR procedure.')
	return(number.list)
}

