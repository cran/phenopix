## a function to convert from roi structure prior to version 2.4

convertROI <- function(path_img_ref,path_ROIs, file.type='.jpg') {
	file <- list.files(path=path_img_ref,pattern = file.type)
    # img<-readJpeg(paste(path_img_ref,file,sep=""))
    img <- raster::flip(brick(paste(path_img_ref,file,sep="/")))
    rois <- paste(path_ROIs, 'roi.data.Rdata', sep='')
    roi.data <- NULL
    load(rois)
    nrois <- length(roi.data)
    roi.names <- names(roi.data)
    roi.old <- roi.data
    roi.data <- list()
    for (a in 1:nrois) {
    	out.mask <-  img[[1]]
    	raster::values(out.mask) <- 0
    	tmp.array <- array(dim=c(nrow(out.mask), ncol(out.mask),1))
    	tmp.array[,,1] <- roi.old[[a]]$pixels.in.roi$pip
    	out.mask <- brick(tmp.array, xmn=0, xmx=ncol(out.mask),
    		ymn=0, ymx=nrow(out.mask))
    	out.mask <- out.mask[[1]]
    	x <- rasterToContour(out.mask, nlevels=1)
    	the.pol <- Polygon(as.matrix(coordinates(x)[[1]][[1]]), hole=FALSE)
    	the.pol.final <- SpatialPolygons(list(Polygons(list(the.pol), ID=1)))
    	out.list <- list(mask=out.mask, polygons=the.pol.final)
    	roi.data[[a]] <- out.list
    }
    names(roi.data) <- roi.names
    return(roi.data)
}  