PhenoPlot <-
function(data, metrics, add=FALSE, show.uncert=TRUE, ...) {
	if (class(metrics)=='list') metrics <- metrics[[1]]
		## recognise series and understand wheter uncert exists
		## data is output from [fitname]Fit functions
		## metrics is output from PhenoExtract function
		if (is.null(dim(metrics))) uncert=FALSE else uncert=TRUE
	if (!add) plot(data$fit$predicted, type='n', ...)
	if (show.uncert & !is.null(data$uncertainty)) for (a in 1:dim(data$uncertainty$predicted)[2]) lines(data$uncertainty$predicted[,a], col='grey')
		lines(data$fit$predicted, lwd=2)
			mean.y <- mean(data$fit$predicted, na.rm=T)
if (length(metrics)!=10) the.seq <- c(1:4) else the.seq <- c(1,2,4)
	if (length(the.seq)==3) { 
		ylons <- c(min(data$fit$predicted, na.rm=T)*1.01, 
		mean(data$fit$predicted, na.rm=T), 
		max(data$fit$predicted, na.rm=T)*0.95)
		colors <- palette()[2:4]
	} else {
		ylons <- c(min(data$fit$predicted, na.rm=T)*1.01, 
		mean(data$fit$predicted, na.rm=T), 
		max(data$fit$predicted, na.rm=T)*0.95, 
		min(data$fit$predicted, na.rm=T)*1.01)
colors <- palette()[2:5]
	}
if (!uncert) {
		abline(v=metrics[the.seq], col=colors)
		text(metrics[the.seq], y=ylons, labels=names(metrics)[the.seq], col=colors)
	} else {
		abline(v=metrics[2,the.seq], col=colors)
		text(metrics[2, the.seq], y=ylons*1.02, labels=names(metrics)[the.seq], col=colors)	
		for (a in the.seq) arrows(metrics[1,a], ylons[a], metrics[3,a], ylons[a], code=3, angle=90, length=0.1, 
			lwd=2, col=colors[a])
	}
}
