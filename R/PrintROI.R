PrintROI <- function(path_img_ref,path_ROIs,which='all',col, file.type='.jpg') {	      
  file<-list.files(path=path_img_ref,pattern = file.type)
    # img<-readJpeg(paste(path_img_ref,file,sep=""))
    img <- brick(paste(path_img_ref,file,sep=""))
     ## convert values from 0:1 to 0:255
    #output list with ROI data
    rois <- paste(path_ROIs, 'roi.data.Rdata', sep='')
    roi.data <- NULL
    load(rois)
    nrois <- length(roi.data)
    roi.names <- names(roi.data)
    plotRGB(img)
    if (which=='all') {
      if (missing(col)) col <- palette()[1:nrois]
      for (a in 1:nrois) {
        act.polygons <- roi.data[[a]]$polygon
        raster::lines(act.polygons, col=col[a], lwd=2)
      }
      legend('top', col=col, lty=1, legend=roi.names)
      } else {
        pos.roi <- which(roi.names %in% which == TRUE)
        raster::lines(roi.data[[pos.roi]]$polygon, lwd=2)
      }
    }
