greenProcess <- function(ts, fit, threshold=NULL, plot=TRUE, which='light', uncert=FALSE, nrep=100, 
    envelope='quantiles', quantiles=c(0.1, 0.9), hydro=FALSE, sf=quantile(ts, na.rm=TRUE, prob=c(0.05, 0.95)), ncores='all',...) {
## doy conversion
doys <- as.numeric(format(index(ts), '%j'))	
act.year <- format(index(ts)[1], '%Y')
act.oct.first <- as.numeric(format(as.POSIXct(paste0(act.year, '-10-01')), '%j'))
# complement <- ifelse(act.oct.first==274, 92, 92)
if (hydro) doys  <- ifelse(doys >= act.oct.first,  doys - (act.oct.first - 1), doys + 92) 
pos <- which(diff(doys)<0)
if (length(pos)!=0) {
	ts <- ts[(pos[1]+1):length(ts)]
	doys <- doys[(pos[1]+1):length(doys)]
}
index(ts) <- doys	
if (missing(fit)) stop('Provide a fit name')	
if (fit=='spline') fit.fun <- SplineFit
if (fit=='beck') fit.fun <- BeckFit
if (fit=='elmore') fit.fun <- ElmoreFit
if (fit=='klosterman') fit.fun <- KlostermanFit
if (fit=='gu') fit.fun <- GuFit
if (fit=='klosterman') fitted.data <- try(fit.fun(ts, uncert=uncert, which=which, nrep=nrep, ncores=ncores, sf=sf)) else {
fitted.data <- try(fit.fun(ts, uncert=uncert, nrep=nrep, ncores=ncores, sf=sf))
}
if (is.null(threshold)) {
	metrics <- NULL 
if (plot) plot(fitted.data$fit$predicted, ...)
}else {
xlab <- ifelse(hydro, 'HydroDOY', 'DOY')	
metrics <- PhenoExtract(fitted.data, method=threshold, envelope=envelope, quantiles=quantiles, uncert=uncert, 
    plot=plot, xlab=xlab, sf=sf, ...)  
}
output <- list(fit=fitted.data, metrics=metrics$metrics, data=ts, uncertainty.df=metrics$unc.df)
## set structure and attributes to output for summary, print, etc
info <- list(fit=fit, threshold=threshold, uncert=uncert, envelope=envelope, 
	quantiles=quantiles, nrep=ifelse(uncert, nrep, 0), hydro=hydro)
info[8] <- attributes(output)
names(info)[8] <- 'names'
attributes(output) <- info
class(output) <- 'phenopix'
return(output) 
}
