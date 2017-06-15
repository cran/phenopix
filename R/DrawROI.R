DrawROI <-
function(path_img_ref,path_ROIs,nroi=1,roi.names=NULL, file.type=".jpg")
  
{	
   if (is.null(roi.names)) roi.names <- paste0('roi', 1:nroi)
    file <- list.files(path=path_img_ref,pattern = file.type)
    # img<-readJpeg(paste(path_img_ref,file,sep=""))
    img <- readJPEG(paste(path_img_ref,file,sep=""))
    ## convert values from 0:1 to 0:255
    img.converted <- img
    img.converted[,,1] <- img[,,1]*255
    img.converted[,,2] <- img[,,2]*255
    img.converted[,,3] <- img[,,3]*255

    ratio <- dim(img)[1]/dim(img)[2]
    #output list with ROI data
    roi.data <- list()
  
    for (i in seq(as.numeric(nroi))) {
      # dev.new()
    # infos <- Sys.info()['sysname']
    #  if (infos=='Darwin') quartz() else x11()
      # plot(img)
      par(mar=c(1,1,4,1))
      plot(0, type='n', xlim=c(0,1), ylim=c(0,1), axes=FALSE)
      rasterImage(img, xleft=0, ybottom=0, xright=1, ytop=ratio)
      title(main=paste('ROI ',i,' - ',roi.names[i],' \n a) n left mouse button clicks on ROI vertexes (n>=3) \n b) 1 right mouse button click to close the ROI',sep=''))
      vertices<-locator(type="l")
      polygon(vertices,lwd=2)
      # coordinates <- data.frame(rowpos=dim(img)[1]-vertices$y,colpos=vertices$x) #rowpos=dim(img)[1]-vertices$y perch locator ha coordinata y che parte dal basso
      coordinates <- data.frame(rowpos=ratio-vertices$y,colpos=vertices$x) #rowpos=dim(img)[1]-vertices$y perch locator ha coordinata y che parte dal basso

      # image.array <- expand.grid(rowpos=seq(1:nrow(img)),colpos=seq(1:ncol(img)))
      image.array <- expand.grid(rowpos=seq(1:nrow(img))/(nrow(img)/ratio),colpos=seq(1:ncol(img))/ncol(img))

      pixels.in.roi <- pnt.in.poly(image.array,coordinates)                    
      dev.print(jpeg, file=paste(path_ROIs,"/ROI",i,'_',roi.names[i],".jpg",sep=''), width=1024, height=1024)
      dev.off()
      
      out <- list(pixels.in.roi,vertices)
      names(out) <- c('pixels.in.roi','vertices')
      
      roi.data[[i]] <- out

    }
    
    names(roi.data) <- roi.names
    save(roi.data,file=paste(path_ROIs,'roi.data.Rdata',sep=''))
    return(invisible(roi.data))    
}
