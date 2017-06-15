SplineFit <-
function (ts, uncert=FALSE, nrep=100, df.factor=0.05, ncores='all', sf=quantile(ts, probs=c(0.05, 0.95), na.rm=TRUE)) {
		if (class(index(ts))[1]=='POSIXct') {
		doy.vector <- as.numeric(format(index(ts), '%j'))
		index(ts) <- doy.vector
	}
	       .normalize <- function(x, sf) (x-sf[1])/(sf[2]-sf[1])
        .backnormalize <- function(x, sf) (x+sf[1]/(sf[2]-sf[1]))*(sf[2]-sf[1])
    ts <- .normalize(ts, sf=sf)    
	fit <- smooth.spline(ts, df=df.factor*length(ts))

	residuals <- ts - as.vector(fit$y) 
	# res.range <- range(residuals, na.rm=TRUE)
	# mean.res <- mean(residuals, na.rm=TRUE)
	## let df factor vary between 0.005 and 0.05
	sd.res <- sd(residuals, na.rm=TRUE)
	## absolute values to compute weights
	res2 <- abs(residuals)
	## normalized between 0 and 1, get weights for random noise generation 
	res3 <- res2/max(res2)
	sign.res <- sign(residuals)
	if (uncert) {
	df.factor2 <- seq(0.01, df.factor, length.out=nrep)
		predicted.df <- data.frame(matrix(ncol=nrep, nrow=length(ts)))
		for (a in 1:nrep) {
			noise <- runif(length(ts), -sd.res, sd.res)*(res3*3)
			sign.noise <- sign(noise)
			pos.no <- which(sign.res!=sign.noise)
			if (length(pos.no)!=0) noise[pos.no] <- -noise[pos.no]
			fit.tmp <- smooth.spline(ts+noise, df=df.factor2[a]*length(ts))
			predicted.df[,a] <- .backnormalize(fit.tmp$y, sf=sf) 
			ratio <- a/nrep*100
			# print(paste('computing uncertainty: ', ratio, '% done', sep=''))
		}
		predicted.df <- zoo(predicted.df, order.by=index(ts))
		# tmp.df <- cbind(as.vector(fit$predicted), tmp.df)
		# names(tmp.df)[1] <- 'fitted'
		uncertainty.list <- list(predicted=predicted.df, params=NULL)
		## simmetry with other fittings
		pred.data <- zoo(fit$y, order.by=index(ts))
		structured.fit <- list(predicted=pred.data, params=NULL, formula=NULL)
		returned <- list(fit=structured.fit, uncertainty=uncertainty.list)
	return(returned)	
		} else {
			pred.data <- zoo(.backnormalize(fit$y, sf=sf), order.by=index(ts))
			structured.fit <- list(predicted=pred.data, params=NULL, formula=NULL)
	returned <- list(fit=structured.fit, uncertainty=NULL)
		(return(returned))
	}
}
