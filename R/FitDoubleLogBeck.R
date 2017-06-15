FitDoubleLogBeck <-
function(
	x,
	t = index(x),
	tout = t,
	weighting = TRUE,
	return.par = FALSE,
	plot = FALSE,
	hessian = FALSE,
	sf=quantile(x, probs=c(0.05, 0.95), na.rm=TRUE),
	...
) {
	.normalize <- function(x, sf) (x-sf[1])/(sf[2]-sf[1])
        .backnormalize <- function(x, sf) (x+sf[1]/(sf[2]-sf[1]))*(sf[2]-sf[1])
		if (any(is.na(x))) stop('NA in the time series are not allowed: fill them with e.g. na.approx()')
		if (class(index(x))[1]=='POSIXct') {
		doy.vector <- as.numeric(format(index(x), '%j'))
		index(x) <- doy.vector
		t <- index(x)
		tout <- t
    }
        x <- .normalize(x, sf=sf)	
	n <- length(x)
	avg <- mean(x, na.rm=TRUE)
	mx <- max(x, na.rm=TRUE)
	mn <- min(x, na.rm=TRUE)
	ampl <- mx - mn
	.doubleLog <- function(par, t) {
		mn <- par[1]
		mx <- par[2]
		sos <- par[3]
		rsp <- par[4]
		eos <- par[5]
		rau <- par[6]
		xpred <- mn + (mx - mn) * (1/(1+exp(-rsp * (t - sos))) + 1/(1+exp(rau * (t - eos))))
		return(xpred)
	}
	.error <- function(par, x, weights) {
		if (any(is.infinite(par))) return(99999)
		if (par[1] > par[2]) return(99999)
		xpred <- .doubleLog(par, t=t)
		sse <- sum((xpred - x)^2 * weights, na.rm=TRUE)
		return(sse)
	}
	if (weighting) {
		iter <- 1:2
	} else {
		iter <- 1
	}	
	weights <- rep(1, length(x))	# inital weights
	doy <- quantile(t, c(0.25, 0.75), na.rm=TRUE)
	prior <- rbind(
		c(mn, mx, doy[1], 0.5, doy[2], 0.5),
		c(mn, mx, doy[2], 0.5, doy[1], 0.5),
		c(mn-ampl/2, mx+ampl/2, doy[1], 0.5, doy[2], 0.5),
		c(mn-ampl/2, mx+ampl/2, doy[2], 0.5, doy[1], 0.5)
	)

	if (plot) plot(t, x)
	for (i in iter) {
		# estimate parameters for double-logistic function starting at different priors
		opt.l <- apply(prior, 1, optim, .error, x=x, weights=weights, method="BFGS", control=list(maxit=1000), hessian=hessian)	# fit from different prior values
		opt.df <- cbind(cost=unlist(llply(opt.l, function(opt) opt$value)), convergence=unlist(llply(opt.l, function(opt) opt$convergence)), ldply(opt.l, function(opt) opt$par))
		best <- which.min(opt.df$cost)
		
		# test for convergence
		if (opt.df$convergence[best] == 1) { # if maximum iterations where reached - restart from best with more iterations
			opt <- opt.l[[best]]
			# repeat with more maximum iterations if it did not converge
			opt <- optim(opt.l[[best]]$par, .error, x=x, weights=weights, method="BFGS", control=list(maxit=1500), hessian=hessian)
			prior <- rbind(prior, opt$par)
			xpred <- .doubleLog(opt$par, t)			
		} else if (opt.df$convergence[best] == 0) {
			opt <- opt.l[[best]]
			prior <- rbind(prior, opt$par)
			xpred <- .doubleLog(opt$par, t)	
		} 
	
		# plot iterations
		if (plot) {
			llply(opt.l, function(opt) {
				xpred <- .doubleLog(opt$par, t)
				lines(t, xpred, col="cyan")
			})
			lines(t, xpred, col="blue", lwd=2)
		}
		
		# get optimized parameters
		parinit <- opt$par
		mn <- opt$par[1]
		mx <- opt$par[2]
		sos <- opt$par[3]
		rsp <- opt$par[4]
		eos <- opt$par[5]
		rau <- opt$par[6]
		m <- lm(c(0, 100) ~ c(sos, eos))
		tr <- coef(m)[2] * t + coef(m)[1]
		tr[tr < 0] <- 0
		tr[tr > 100] <- 100
				
		# estimate weights
		res <- xpred - x
		weights <- 1/((tr * res + 1)^2)
		weights[res > 0 & res <= 0.01] <- 1
		weights[res < 0] <- 4
	}
	
	# return NA in case of no convergence
	if (opt$convergence != 0) {
		opt$par[] <- NA
		xpred <- rep(NA, length(tout))
	} else {
		xpred <- .doubleLog(opt$par, tout)
	}
xpred <- .backnormalize(xpred, sf=sf)	
xpred.out <- zoo(xpred, order.by=t)
names(opt$par) <- c("mn", "mx", "sos", "rsp", "eos", "rau")

    if (hessian) {
        opt.new <- optim(opt$par, .error, x=x, weights=weights, method="BFGS", hessian=TRUE
                         ## ,              
                         ## control=list('fnscale'=-1)
                         )
        .qr.solve <- function(a, b, tol = 1e-07, LAPACK=TRUE) {
        if (!is.qr(a)) 
        a <- qr(a, tol = tol, LAPACK=LAPACK)
    nc <- ncol(a$qr)
    nr <- nrow(a$qr)
    if (a$rank != min(nc, nr)) 
        stop("singular matrix 'a' in solve")
    if (missing(b)) {
        if (nc != nr) 
            stop("only square matrices can be inverted")
        b <- diag(1, nc)
    }
    res <- qr.coef(a, b)
    res[is.na(res)] <- 0
    res        
        }
        vc <- .qr.solve(opt$hessian)
        npar <- nrow(vc)
        s2 <- opt.df$cost[best]^2 / (n - npar)
        std.errors <- sqrt(diag(vc) * s2)     # standard errors
        }

fit.formula <- expression(mn + (mx - mn) * (1/(1+exp(-rsp * (t - sos))) + 1/(1+exp(rau * (t - eos)))))
output <- list(predicted=xpred.out, params=opt$par, formula=fit.formula, sf=sf)
if (hessian) output <- list(predicted = xpred.out, params = opt$par, formula = fit.formula, stdError=std.errors, sf=sf)
return(output)
}
