resizeImage <- function(image.in, image.out, roi.in, roi.out, npixels) {
	r <- brick(image.in)
	aggregated.r <- aggregate(r,npixels)
	reduced <- as.array(aggregated.r)
	reduced2 <- reduced/255
	writeJPEG(reduced2, target=image.out, quality=1)
	load(roi.in)
	roi.data <- updateROI(roi.data, reduced2)
	save(roi.data, file=roi.out)
}
