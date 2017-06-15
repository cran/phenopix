PrintROI <- function(path_img_ref,path_ROIs,which='all',col, file.type='.jpg') {	      
    file<-list.files(path=path_img_ref,pattern = file.type)
    # img<-readJpeg(paste(path_img_ref,file,sep=""))
    img<-readJPEG(paste(path_img_ref,file,sep=""))
    ## convert values from 0:1 to 0:255
    ratio <- dim(img)[1]/dim(img)[2]
    #output list with ROI data
    rois <- paste(path_ROIs, 'roi.data.Rdata', sep='')
    roi.data <- NULL
    load(rois)
    nrois <- length(roi.data)
    roi.names <- names(roi.data)
      par(mar=c(1,1,4,1))
      plot(0, type='n', xlim=c(0,1), ylim=c(0,1), axes=FALSE)
      rasterImage(img, xleft=0, ybottom=0, xright=1, ytop=ratio)
    if (which=='all') {
      if (missing(col)) col <- palette()[1:nrois]
for (a in 1:nrois) {
  act.coords <- roi.data[[a]]$vertices
  polygon(act.coords, border=col[a])
}
legend('top', col=col, lty=1, legend=roi.names)
    } else {
      pos.roi <- which(roi.names %in% which == TRUE)
      polygon(roi.data[[pos.roi]]$vertices)
    }
}
 