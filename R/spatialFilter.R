spatialFilter <-
function(spatial.list, filter=c('night','spline', 'max'), filter.options=NULL, ncores='all',
    log.file=NULL, NDVI=FALSE) {
    if (length(spatial.list)==1) spatial.list <- spatial.list[[1]]
    if (is.null(filter.options)) {
        filter.options <- list(night.filter=list(threshold=0.2),
          blue.filter=list(threshold=0.05),
          mad.filter=list(z=15),
          max.filter=list(w=3, qt=0.9),
          spline.filter=list(stdup=4, stddown=4, loop_spline=20))
    }
    # if (save==FALSE & assign ==FALSE) stop('Arguments save and assign cannot be both FALSE!')
    time <- as.POSIXct(names(spatial.list))
    npixels <- dim(spatial.list[[1]])[1]
    # fitting.list <- NULL
    # for (a in 1:npixels) {
    if (ncores=='all') cores <- detectCores() else cores <- ncores
    cl <- makeCluster(cores)
    # cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
    if (!is.null(log.file)) writeLines(c(""), paste(log.file, "log.txt", sep='/'))
        a <- NULL    
    fitting.list <- foreach(a=1:npixels, .packages=c('phenopix', 'zoo'), .combine=cbind) %dopar% {
       if (!is.null(log.file)) {
        sink(paste(log.file, "log.txt", sep='/'), append=TRUE)  
        cat(paste(round(a/npixels*100), '% done\n'))
        sink() 
    }
    #   require(zoo)
    single.pixel <- NULL
    for (i in 1:length(spatial.list)) {
        single.triplet <- spatial.list[[i]][a,]
        single.pixel <- rbind(single.pixel, single.triplet)
    }    
    single.pixel <- as.data.frame(single.pixel)   
    rownames(single.pixel) <- NULL
    single.pixel$time <- time
    raw.dn <- ifelse(NDVI, 'NDVI', TRUE)
    filtered.tmp <- phenopix::autoFilter(single.pixel, raw.dn=raw.dn, dn=1:3, plot=FALSE, filter=filter, filter.options=filter.options)
    # if (length(filter)!=1) {
        ncols <- dim(filtered.tmp)[2]           
        filtered.tmp <- filtered.tmp[,ncols]            
    # }
 # filtered.tmp <- filtered.tmp               # print(round(a/npixels*100,2))
}
stopCluster(cl)
return(fitting.list)
}
