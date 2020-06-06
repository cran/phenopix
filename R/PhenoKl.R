PhenoKl <-
function(
	##title<< 
	## Method 'Deriv' to calculate phenology metrics
	##description<<
	## This function implements the derivative method for phenology. This is rather an internal function; please use the function \code{\link{Phenology}} to apply this method.
	
	x, 
	### seasonal cycle of one year	
	uncert=FALSE, 
    fit, 
    breaks, 
	### plot results?
	
	...
	### further arguments (currently not used)
		
	##seealso<<
	## \code{\link{Phenology}}

) {
	# x <- ifelse(uncert==TRUE, x$uncertainty, x$fit)
    ## x are the parameters
    ## condition to understand if we enter the function with a Spline fitting or else. 
    if (is.null(x)) {
        x <- fit$predicted
        spline.eq <- smooth.spline(x, df=length(x))
        der1 <- predict(spline.eq, d=1)$y
        der2 <- predict(spline.eq, d=2)$y
        t <- index(x)
        values <- as.vector(x)        
    } else {
    names(x) <- names(fit$params)
	retrieved.formula <- fit$formula
	t <- index(fit$predicted)
	values <- as.vector(fit$predicted)
	D1 <- D(retrieved.formula, 't')
	D2 <- D(D1, 't')
    ## e1 <- parent.frame()
    der1 <- eval(D1, envir=as.list(x))
	der2 <- eval(D2, envir=as.list(x))
    }
    if (all(is.na(x))) metrics <- rep(NA,4) else {
    k <- der2/(1 + der1^2)^(3/2)
#     if (length(which(is.na(k)==TRUE))!=0 | length(which(is.infinite(k)==TRUE))!=0) {
#     k[is.na(k)] <- 0
#     k[is.infinite(k)] <- 0   
#     warning('Check your fitting because the rate of curvature contains NA or infinite values \n They were set at 0!')
# }
    if (length(which(is.na(k)==TRUE))!=0 | length(which(is.infinite(k)==TRUE))!=0) {metrics <- rep(NA, 4)} else {
    spline.k <- smooth.spline(k, df=0.1*length(k))
    der.k <- predict(spline.k, d=1)$y
    ## find maxima of derivative of k
    # ## split season
    half.season <- which.max(values)+20
    increasing.k <- try(der.k[1:half.season])
    increasing.k.d <- try(t[1:half.season])
    decreasing.k <- try(der.k[half.season:length(k)])
    decreasing.k.d <- try(t[half.season:length(k)])
    check.list <- list(increasing.k, decreasing.k)
    classes <- lapply(check.list, class)
    if (length(which(classes=='try-error'))!=0) metrics <- rep(NA,4) else {
    ## subset before first min
    subset1 <- increasing.k[1:which.min(increasing.k)]
    subset1.d <- increasing.k.d[1:which.min(increasing.k)]
    p1 <- subset1.d[which.max(subset1)]
    ## subset between first min and mid season
    subset2 <- increasing.k[which.min(increasing.k):length(increasing.k)]
    subset2.d <- increasing.k.d[which.min(increasing.k):length(increasing.k)]
    p2 <- subset2.d[which.max(subset2)]
    ## subset between mid season and max 
    subset3 <- decreasing.k[1:which.max(decreasing.k)]
    subset3.d <- decreasing.k.d[1:which.max(decreasing.k)]
    p3 <- subset3.d[which.min(subset3)]
## rest of the season
    subset4 <- decreasing.k[which.max(decreasing.k):length(decreasing.k)]
    subset4.d <- decreasing.k.d[which.max(decreasing.k):length(decreasing.k)]
    p4 <- subset4.d[which.min(subset4)]
    metrics <- c(p1, p2, p3, p4)
}
}
}
	## extract parameters
	names(metrics)  <- c('Greenup', 'Maturity', 'Senescence', 'Dormancy')

	return(metrics)
}
