PhenoExtract <-
function(data, 
	method='trs', 
	uncert=FALSE, 
	breaks=3, 
	envelope='quantiles', 
	quantiles=c(.1,.9), plot=TRUE, sf,...) {
	the.formula <- data$fit$formula
	if (method=='trs') {
		the.function <- PhenoTrs
		single.data <- data$fit$predicted
		uncertainty.data <- data$uncertainty$predicted
		nna <- 10
	} 	
	if (method=='derivatives') {
		the.function <- PhenoDeriv
		single.data <- data$fit$predicted
		uncertainty.data <- data$uncertainty$predicted
				nna <- 10
	} 
	if (method=='gu') {
		the.function <- PhenoGu
		single.data <- data$fit$params
		uncertainty.data <- data$uncertainty$params
				nna <- 9
	} 	
# if (method=='bayesian') {
# 		the.function <- PhenoBayes
# 	if (raw.data)  single.data <- data else { 
# 		single.data <- data$fit$predicted
# 		uncertainty.data <- data$uncertainty$predicted
# 	}
# 	} 
	if (method=='klosterman') {
		the.function <- PhenoKl
		single.data <- data$fit$params
		uncertainty.data <- data$uncertainty$params
				nna <- 4
	} 
	if (is.null(data$uncertainty) | uncert==FALSE) {
		returned <- suppressWarnings(the.function(single.data, fit=data$fit, uncert=uncert, breaks=breaks, sf=sf, ...)) 
	} else {
		thresholds <- NULL
		for (a in 1:dim(uncertainty.data)[2]) {
			tmp.column <- try(suppressWarnings(the.function(uncertainty.data[,a], fit=data$fit, uncert=uncert, breaks=breaks, sf=sf, ...)))
			if (class(tmp.column)=='try-error') tmp.column <- rep(NA, nna)
			thresholds <- cbind(thresholds, tmp.column)
		}
	# thresholds <- apply(uncertainty.data, 2, the.function, uncert=TRUE, fit=data$fit, breaks=breaks) 
		if (envelope=='quantiles') returned <- as.data.frame(apply(thresholds, 1, function(x) quantile(x, c(quantiles[1], .5, quantiles[2]), na.rm=TRUE)))
			if (envelope=='min-max') {
				returned <- as.data.frame(apply(thresholds, 1, function(x) rbind(min(x, na.rm=TRUE), mean(x, na.rm=TRUE), max(x, na.rm=TRUE))))
				rownames(returned) <- c('min', 'mean', 'max')
			}
		}
		if (plot) {
			suppressWarnings(PhenoPlot(data, returned, add=F, show.uncert=uncert, ...))
		}
		if (uncert) {
			thresholds.t <- t(thresholds)
			names(thresholds.t) <- names(returned)
			rownames(thresholds.t) <- NULL
			ret2 <- list(metrics=returned, unc.df=as.data.frame(thresholds.t))
		} else ret2 <- list(metrics=returned, unc.df=NULL)

		return(ret2)
	}
