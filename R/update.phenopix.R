update.phenopix <- function(object, threshold, envelope, quantiles, uncert, plot, ...) {
sf <- range(extract(object, 'data'), na.rm=TRUE)		
fitted.data <- object$fit
attr.retr <- attributes(object)
if (missing(envelope)) envelope <- attr.retr$envelope
if (missing(quantiles)) quantiles <- attr.retr$quantiles
if (missing(uncert)) uncert <- attr.retr$uncert
nrep <- attr.retr$nrep
if (missing(plot)) plot <- TRUE
metrics <- PhenoExtract(fitted.data, method=threshold, envelope=envelope, quantiles=quantiles, uncert=uncert, 
    plot=plot, sf=sf, ...)  
output <- list(fit=fitted.data, metrics=metrics$metrics, data=object$data, uncertainty.df=metrics$unc.df)
## set structure and attributes to output for summary, print, etc
info <- list(fit=attr.retr$fit, threshold=threshold, uncert=uncert, envelope=envelope, 
    quantiles=quantiles, nrep=ifelse(uncert, nrep, 0))
info[7] <- attributes(output)
names(info)[7] <- 'names'
attributes(output) <- info
class(output) <- 'phenopix'
return(output) 
}