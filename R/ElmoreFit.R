ElmoreFit <- function (ts, uncert=FALSE, nrep=100, ncores='all', sf=quantile(ts, probs=c(0.05, 0.95), na.rm=TRUE)) {
	if (class(index(ts))[1]=='POSIXct') {
		doy.vector <- as.numeric(format(index(ts), '%j'))
		index(ts) <- doy.vector
	}
	fit <- FitDoubleLogElmore(ts, sf=sf)
	residuals <- ts - as.vector(fit$predicted) 
	# res.range <- range(residuals, na.rm=TRUE)
	# mean.res <- mean(residuals, na.rm=TRUE)
	sd.res <- sd(residuals, na.rm=TRUE)
	## absolute values to compute weights
	res2 <- abs(residuals)
	## normalized between 0 and 1, get weights for random noise generation 
	res3 <- res2/max(res2)	
	sign.res <- sign(residuals)
	if (uncert) {
		if (ncores=='all') cores <- detectCores() else cores <- ncores
		cl <- makeCluster(cores)
    # cl <- makeCluster(detectCores()-1)
		registerDoParallel(cl)
		coefs <- c(3.702, 0.00409, -0.660)		
		.pred.fun <- function(coefs, cores, nrep) {
			expected.time <- coefs[1] + coefs[2]*nrep + coefs[3]*log(cores)
			names(expected.time) <- NULL
			return(round(exp(expected.time)/60))
		}
		min.exp.time <- .pred.fun(coefs, cores, nrep)
		# expected.time <- nrep * 0.797
		# min.exp.time <- round(expected.time/60)
		print(paste0('estimated computation time (',cores,' cores): ', min.exp.time, ' mins'))
		output <- foreach(a=1:nrep, .packages=c('phenopix'), .combine=c) %dopar% {
			noise <- runif(length(ts), -sd.res, sd.res)
			sign.noise <- sign(noise)
			pos.no <- which(sign.res!=sign.noise)
			if (length(pos.no)!=0) noise[pos.no] <- -noise[pos.no]
			# randomly sample
			noised <- ts + noise
			fit.tmp <- try(FitDoubleLogElmore(noised, sf=sf))
			if (class(fit.tmp)=='try-error') out.single <- list(predicted=rep(NA, length(ts)), params=rep(NA,7)) else {
			out.single <- list(predicted=fit.tmp$predicted, params=fit.tmp$params)
		}
			# ratio <- a/nrep*100
			# print(paste('computing uncertainty: ', ratio, '% done', sep=''))
		}
		stopCluster(cl)
		pred.pos <- which(names(output)=='predicted')
		par.pos <- which(names(output)=='params')
		predicted.df <- as.data.frame(output[pred.pos])
		names(predicted.df) <- paste0('X',1:length(predicted.df))
		predicted.df <- zoo(predicted.df, order.by=index(ts))
		params.df <- as.data.frame(output[par.pos])
		names(params.df) <- names(predicted.df)
# tmp.df <- cbind(as.vector(fit$predicted), tmp.df)
# names(tmp.df)[1] <- 'fitted'
		uncertainty.list <- list(predicted=predicted.df, params=params.df)
		returned <- list(fit=fit, uncertainty=uncertainty.list)
		return(returned)	
	} else {
		returned <- list(fit=fit, uncertainty=NULL)
		(return(returned))
	}
}
