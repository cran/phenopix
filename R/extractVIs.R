extractVIs <- function(img.path,roi.path,vi.path=NULL,roi.name=NULL,plot=TRUE, begin=NULL, spatial=FALSE, date.code, npixels=1, 
  file.type='.jpg', bind=FALSE, ncores='all', log.file=NULL) {     
  roi.data <- NULL
  load(paste(roi.path,'/roi.data.Rdata',sep=''))
  if (is.null(roi.name)) {    
    roi.name <- names(roi.data)        
  } 
  
  roi.pos <- which(names(roi.data) %in% roi.name == TRUE)
  
  files <-list.files(path=img.path,recursive=TRUE, pattern = file.type)
  n_files <-length(files)
  dates <- as.POSIXct(sapply(files, extractDateFilename, date.code=date.code), origin='1970-01-01')
  if (any(is.na(dates))) stop(paste('Something wrong in your date!'))

    if (!is.null(begin)) {
      beg.date <- as.POSIXct(begin, origin='1970-01-01')
      pos.good <- which(dates>=beg.date)          
    } else {
      pos.good <- 1:n_files
      beg.date <- as.POSIXct('1970-01-01')
    }
    files <- files[pos.good]
    n_files <- length(files)  
    if (npixels!=1) {
      r <- brick(paste(img.path,'/',files[1],sep=''))
      aggregated.r <- aggregate(r,npixels)
      back.array <- raster::as.array(aggregated.r)
      sample.img <- back.array/255    
      roi.data <- updateROI(roi.data, sample.img)
    }  
    if (spatial==FALSE) {
      VI.data <- list()  
  #loop trough ROIs
      for (roi in roi.pos) {   
        temp.roi <- roi.data[[roi]]
        pos.pix.roi <- which(temp.roi$pixels.in.roi$pip == 1)
        VI.data.roi <- NULL
    ## loop trough images
        if (ncores=='all') cores <- detectCores() else cores <- ncores
        cl <- makeCluster(cores)
        registerDoParallel(cl)
    # cl <- makeCluster(detectCores()-1)
        # registerDoParallel(cl)
        if (!is.null(log.file)) writeLines(c(""), paste(log.file, "log.txt", sep='/'))
          img <- NULL    
        VI.data.roi <- foreach(img=1:n_files, .packages=c('raster', 'phenopix'), .combine=rbind) %dopar% {
         if (!is.null(log.file)) {
          sink(paste(log.file, "log.txt", sep='/'), append=TRUE)  
          cat(paste(round(img/n_files*100), '% done\n'))
          sink() 
        }
      ## check date and begin
        temp.date <- extractDateFilename(files[img], date.code)
        # if (is.na(temp.date)) stop('Something wrong in your date!')
        # print (files[img])      
      # temp.img <- readJpeg(paste(img.path,'/',files[img],sep=''))
        r <- brick(paste(img.path,'/',files[img],sep=''))
        if (npixels!=1) aggregated.r <- aggregate(r,npixels) else aggregated.r <- r
        red <- raster::as.array(raster(aggregated.r, 1))[,,1]
        green <- raster::as.array(raster(aggregated.r, 2))[,,1]
        blue <- raster::as.array(raster(aggregated.r,3))[,,1]
        red[temp.roi$pixels.in.roi$pip==0] <- NA
        green[temp.roi$pixels.in.roi$pip==0] <- NA
        blue[temp.roi$pixels.in.roi$pip==0] <- NA
        temp.r.av <- mean(red, na.rm=TRUE)
        temp.g.av <- mean(green, na.rm=TRUE)
        temp.b.av <- mean(blue, na.rm=TRUE)
        temp.r.sd <- sd(red, na.rm=TRUE)
        temp.g.sd <- sd(green, na.rm=TRUE)
        temp.b.sd <- sd(blue, na.rm=TRUE)
        temp.bri.av <- mean(red + green + blue, na.rm=TRUE)
        temp.bri.sd <- sd(red + green + blue, na.rm=TRUE)
        temp.gi.av <- mean(green/(red + green + blue),na.rm=TRUE)
        temp.gi.sd <- sd(green/(red + green + blue),na.rm=TRUE)
        temp.gei.av <- mean( 2*green - (red + blue),na.rm=TRUE)
        temp.gei.sd <- sd( 2*green - (red + blue),na.rm=TRUE)
        temp.ri.av <- mean(red/(red + green + blue),na.rm=TRUE)
        temp.ri.sd <- sd(red/(red + green + blue),na.rm=TRUE)
        temp.bi.av <- mean(blue/(red + green + blue),na.rm=TRUE)
        temp.bi.sd <- sd(blue/(red + green + blue),na.rm=TRUE)
        temp.doy <- as.numeric(format(temp.date,format="%j"))
        temp.VI <- data.frame(date = temp.date, doy = temp.doy, r.av = temp.r.av, g.av = temp.g.av, b.av = temp.b.av, r.sd = temp.r.sd, g.sd = temp.g.sd, b.sd = temp.b.sd, bri.av = temp.bri.av, bri.sd = temp.bri.sd,
          gi.av = temp.gi.av, gi.sd = temp.gi.sd, gei.av = temp.gei.av, gei.sd = temp.gei.sd, ri.av = temp.ri.av, ri.sd = temp.ri.sd, bi.av = temp.bi.av, bi.sd = temp.bi.sd)      
      } #endfor loop images  
      stopCluster(cl)
      end <- max(VI.data.roi$date, na.rm=TRUE)  
      if (end < beg.date) stop('Your begin date is later than last record in your timeseries')
        end <- trunc(end, 'day')
      VI.data[[roi]] <- VI.data.roi         
      if (plot == TRUE & is.null(begin)) {
        png(filename=paste(vi.path,roi.name[roi],'_roi_VI_plot.png',sep=''), width=800, height=5*400, pointsize=30)  
      #    } else {
      # png(filename=paste(vi.path,begin, '_', end, '_',roi.name[roi],'_roi_VI_plot.png',sep=''), width=800, height=5*400, pointsize=30)           
      #    }
        par(mfrow=c(5,1))
        par(mar=c(3,4,2,0.5))
        plot(VI.data.roi$date,VI.data.roi$r.av,col='red',pch=20,xlab='',ylab='R-G-B',main=paste('ROI: ',roi.name[roi],sep=''))
        points(VI.data.roi$date,VI.data.roi$g.av,col='green',pch=20)  
        points(VI.data.roi$date,VI.data.roi$b.av,col='blue',pch=20)
        par(mar=c(3,4,0.5,0.5))
        plot(VI.data.roi$date,VI.data.roi$ri.av,col='red',pch=20,xlab='',ylab='RI')
        par(mar=c(3,4,0.5,0.5))  
        plot(VI.data.roi$date,VI.data.roi$gi.av,col='green',pch=20,xlab='',ylab='GI')
        par(mar=c(3,4,0.5,0.5))  
        plot(VI.data.roi$date,VI.data.roi$bi.av,col='blue',pch=20,xlab='',ylab='BI')
        par(mar=c(4,4,0.5,0.5)) 
        plot(VI.data.roi$date,VI.data.roi$bri.av,col='grey',pch=20,xlab='doy',ylab='BRI')
        
        dev.off()
      }   
    } #endfor loop rois

    names(VI.data) <- roi.name
    if (is.null(begin)) {
      save(VI.data,file=paste(vi.path,'VI.data.Rdata',sep=''))
    } else {
      if (bind) {
        VI.data.new <- VI.data
        load(paste(vi.path,'VI.data.Rdata',sep=''))
        check <- VI.data.new[[1]]$date[1]<tail(VI.data[[1]]$date,1)
        if (check) warning('New begin date is prior to the end of the already existing records in VI.data! Check your dates')
         for (p in 1:length(VI.data)) VI.data[[p]] <- rbind(VI.data[[p]], VI.data.new[[p]])
          save(VI.data,file=paste(vi.path,'VI.data.Rdata',sep=''))      
      } else {
        save(VI.data,file=paste(vi.path,begin,'_', end, '_', 'VI.data.Rdata',sep=''))  
      }
    }
  } else {## if spatial == TRUE
  VI.data <- list()
  for (roi in roi.pos) {    
    temp.roi <- roi.data[[roi]]
    pos.pix.roi <- which(temp.roi$pixels.in.roi$pip == 1)
    #loop trough images
        if (ncores=='all') cores <- detectCores() else cores <- ncores
        cl <- makeCluster(cores)
        registerDoParallel(cl)
    # cl <- makeCluster(detectCores()-1)
        # registerDoParallel(cl)
        if (!is.null(log.file)) writeLines(c(""), paste(log.file, "log.txt", sep='/'))
          img <- NULL    
    img <- NULL
    VI.data.roi <- foreach(img=1:n_files, .packages=c('raster', 'phenopix')) %dopar% {
     if (!is.null(log.file)) {
      sink(paste(log.file, "log.txt", sep='/'), append=TRUE)  
      cat(paste(round(img/n_files*100), '% done\n'))
      sink() 
    }
      ## check date and begin
    temp.date <- extractDateFilename(files[img], date.code)
    #   if (!is.null(begin)) {
    #     beg.date <- as.POSIXct(begin, origin='1970-01-01')
    #     if (beg.date >= temp.date) next()
    #   }
    # print (files[img])
    r <- brick(paste(img.path,'/',files[img],sep=''))
    if (npixels!=1) aggregated.r <- aggregate(r,npixels) else aggregated.r <- r
      # temp.img <- readJpeg(paste(img.path,'/',files[img],sep=''))
      # temp.img <- readJPEG(paste(img.path,'/',files[img],sep=''))
    red <- raster::as.array(raster(aggregated.r, 1))[,,1]
    green <- raster::as.array(raster(aggregated.r, 2))[,,1]
    blue <- raster::as.array(raster(aggregated.r,3))[,,1]
    all.reds <- red[pos.pix.roi] 
    all.greens <- green[pos.pix.roi] 
    all.blue <- blue[pos.pix.roi] 
    pixel.df <- data.frame(red=all.reds, green=all.greens, blue=all.blue)
    # VI.data.roi[[img]] <- pixel.df
    # names(VI.data.roi)[img] <- temp.date
  }
  stopCluster(cl)
  names(VI.data.roi) <- dates
### remove unprocessed data if 
  null.pos <- which(lapply(VI.data.roi, is.null)==TRUE)
  if (length(null.pos)!=0) VI.data.roi <- VI.data.roi[-null.pos]
  end <- max(dates, na.rm=TRUE)  
  if (!is.null(begin)) if (end < beg.date) stop('Your final date is later than last record in your timeseries')
    end <- trunc(end, 'day')
  VI.data[[roi]] <- VI.data.roi
}  ## end roi loop
names(VI.data) <- roi.name
if (is.null(begin)) {
  save(VI.data,file=paste(vi.path,'VI.data.spatial.Rdata',sep=''))
} else {
  save(VI.data,file=paste(vi.path,begin,'_', end, '_', 'VI.data.spatial.Rdata',sep=''))  
}
}
invisible(VI.data)   
}
