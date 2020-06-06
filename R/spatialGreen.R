spatialGreen <- function(filtered.data, fit, threshold, ncores='all', log.file=NULL) {
    # if (save==FALSE & assign ==FALSE) stop('Arguments save and assign cannot be both FALSE!')
        spatial.list <- filtered.data    
    npixels <- dim(spatial.list)[2]
    # fitting.list <- NULL
    # for (a in 1:npixels) {
    # if (parallel) {
    if (ncores=='all') cores <- detectCores() else cores <- ncores
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    if (!is.null(log.file)) writeLines(c(""), paste(log.file, "log.txt", sep='/'))
        a <- NULL
        fitting.list <- foreach(a=1:npixels, .packages=c('phenopix', 'zoo')) %dopar% {
            if (!is.null(log.file)) {
                sink(paste(log.file, "log.txt", sep='/'), append=TRUE)  
                cat(paste(round(a/npixels*100), '% done\n'))
                sink() 
            }
    #   require(zoo)
            fitted.tmp <- try(phenopix::greenProcess(na.approx(spatial.list[,a]), fit, threshold, plot=FALSE), silent=TRUE)
            if (class(fitted.tmp)=='try-error') fitted.tmp <- NA
            fitted.tmp <- fitted.tmp
    # # if (save) {
    #     filename <- paste0(path, '/fitted.tmp', a, '.Rdata')
    #     save(fitted.tmp, file=filename)
    #     if (!assign) fitted.tmp <- NULL
    # }
    # # fitting.list[[a]] <- fitted.tmp
        }
        stopCluster(cl)
# } else {
#     fitting.list <- NULL
#     #   require(zoo)
#     for (a in 1:npixels) {
#     fitted.tmp <- try(phenopix::greenProcess(na.approx(spatial.list[,a]), fit, threshold, plot=FALSE))
#     fitting.list[[a]] <- fitted.tmp
# }
# # final.container <- fitting.list
# }
    # if (assign) return(fitting.list)
        return(fitting.list)
    }
