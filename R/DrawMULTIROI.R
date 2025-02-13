## 

DrawMULTIROI <- function(path_img_ref,path_ROIs,nroi=1,roi.names=NULL, file.type=".jpg")  
{

 if (is.null(roi.names)) roi.names <- paste0('roi', 1:nroi)
 file <- list.files(path=path_img_ref,pattern = file.type)
    # img<-readJpeg(paste(path_img_ref,file,sep=""))
    img <- raster::flip(brick(paste(path_img_ref,file,sep="")))
    roi.data <- list()

    for (i in seq(as.numeric(nroi))) {
      # dev.new()
    # infos <- Sys.info()['sysname']
    #  if (infos=='Darwin') quartz() else x11()
      # plot(img)
      plotRGB(img)
      mtext(paste('ROI ',i,' - ',roi.names[i],' \n a) n left mouse button clicks on ROI vertexes (n>=3) \n b) 1 right mouse button click to close the polygon',sep=''), side=3, line=-5)
      answer <- 'n'
      pol.all <- NULL
      while(answer=='n') {
        pol1 <- drawPoly(sp=TRUE, col='red', lwd=2)
        answer <- ask('are you done with your roi?\ntype y or n')
        pol.all <- c(pol.all, pol1)
      }
      if (length(pol.all) ==1) vertices <- pol.all[[1]] else {
      vertices <- do.call(bind, pol.all)
    }
      white.mask <- img[[1]]
      raster::values(white.mask) <- 1
      masked.img <- mask(white.mask, vertices)
      raster::values(masked.img)[is.na(values(masked.img))] <- 0
    # coordinates <- data.frame(rowpos=dim(img)[1]-vertices$y,colpos=vertices$x) #rowpos=dim(img)[1]-vertices$y perch locator ha coordinata y che parte dal basso
    coordinates <- data.frame(rowpos=coordinates(vertices)[,1],colpos=coordinates(vertices)[,2]) #rowpos=dim(img)[1]-vertices$y perch locator ha coordinata y che parte dal basso
    dev.print(jpeg, file=paste(path_ROIs,"/ROI",i,'_',roi.names[i],".jpg",sep=''), width=1024, height=1024)
    dev.off()
    out <- c(masked.img, vertices)
    names(out) <- c('mask', 'polygons')      
    roi.data[[i]] <- out

  }
  names(roi.data) <- roi.names
  save(roi.data,file=paste(path_ROIs,'roi.data.Rdata',sep=''))
  return(invisible(roi.data))    
}
