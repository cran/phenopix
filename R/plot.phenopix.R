plot.phenopix <- function(x, y=NULL, what='all',main=NULL, ...) {
	attr.retr <- attributes(x)
    title <- paste0('fit: ', toupper(attr.retr$fit), '  -  thresholds: ', toupper(attr.retr$threshold))
    plot(x$data,...)
    if (is.null(main)) title(main=title) else title(main=main) 
    if (what=='all') {
    if (is.null(x$metrics)) PhenoPlot(x$fit, metrics=NA, add=TRUE) else {
    PhenoPlot(x$fit, x$metrics, add=TRUE)
}
} else {
    if (what=='fitting') PhenoPlot(x$fit, metrics=NA, add=TRUE)
    if (what=='params') {
        if (is.null(x$uncertainty.df)) stop('Uncertainty was not estimated. Try what=all')
    	parameters <- as.data.frame(t(extract(x, what='curve.params.uncert')))
    	true.params <- extract(x, what='curve.params')
    	par(mfrow=c(1, length(parameters)), mar=c(5,0,4,0), oma=c(0,4,0,2))
    	for (a in 1:length(parameters)) {
    		boxplot(parameters[,a], main=names(parameters)[a],...)
    		abline(h=true.params[a], lwd=2, col='red')
    	}
    } 
    if (what=='thresholds') {
     	parameters <- extract(x, what='metrics.uncert')
    	# true.params <- extract(x, what='metrics')[2,]
    	par(mfrow=c(1, length(parameters)), mar=c(5,0,4,0), oma=c(0,4,0,2))
    	for (a in 1:length(parameters)) {
    		boxplot(parameters[,a], main=names(parameters)[a], ...)
    		# abline(h=true.params[a], lwd=2, col='red')
    	}   	
    }	
}
}