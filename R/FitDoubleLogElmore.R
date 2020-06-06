FitDoubleLogElmore <-
function(
	x,
	t = index(x),
	tout = t,
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
	n <- length(na.omit(x))
	if (n < 7) {
		if (return.par) return(rep(NA, 7))
		if (!return.par) return(rep(NA, length(tout)))
	}
	n <- length(x)
	        x <- .normalize(x, sf=sf)
	avg <- mean(x, na.rm=TRUE)
	mx <- max(x, na.rm=TRUE)
	mn <- min(x, na.rm=TRUE)
	ampl <- mx - mn
	.doubleLog <- function(par, t) {
		m1 <- par[1]
		m2 <- par[2]
		m3 <- par[3]
		m4 <- par[4]
		m5 <- par[5]
		m6 <- par[6]
		m7 <- par[7]		
		m3l <- m3 / m4
		m4l <- 1 / m4
		m5l <- m5 / m6
		m6l <- 1 / m6
		# xpred <- m1 + (m2 - m7 * t) * ( (1/(1 + exp(((m3/m4) - t)/(1/m4)))) - (1/(1 + exp(((m5/m6) - t)/(1/m6)))) )
		xpred <- m1 + (m2 - m7 * t) * ( (1/(1 + exp((m3l - t)/m4l))) - (1/(1 + exp((m5l - t)/m6l))) )
		return(xpred)
	}
	.error <- function(par, x, t) {
		if (any(is.infinite(par))) return(99999)
		xpred <- .doubleLog(par, t=t)
		sse <- sum((xpred - x)^2, na.rm=TRUE)
		return(sse)
	}
	doy <- quantile(t, c(0.25, 0.75), na.rm=TRUE)
    prior <- rbind(
        c(mn, mx-mn, 200, 1.5, 300, 1.5, 0.002),
        c(mn, mx-mn, 100, 0.5, 200, 0.9, 0.002),
        c(mn, mx-mn, 50, 0.5, 300, 1.2, 0.05),
        c(mn, mx-mn, 300, 2, 350, 2.5, 0.05)
    )
	if (plot) plot(t, x)
	opt.l <- apply(prior, 1, optim, .error, x=x, t=t, method="BFGS", control=list(maxit=100), hessian=hessian)	# fit from different prior values
	opt.df <- cbind(cost=unlist(llply(opt.l, function(opt) opt$value)), convergence=unlist(llply(opt.l, function(opt) opt$convergence)), ldply(opt.l, function(opt) opt$par))
	best <- which.min(opt.df$cost) 
	if (opt.df$convergence[best] == 1) { # if maximum iterations where reached - restart from best with more iterations
		opt <- opt.l[[best]]
		opt <- optim(opt.l[[best]]$par, .error, x=x, t=t, method="BFGS", control=list(maxit=700), hessian=hessian)
		prior <- rbind(prior, opt$par)
		xpred <- .doubleLog(opt$par, t)			
	} else if (opt.df$convergence[best] == 0) {
		opt <- opt.l[[best]]
		prior <- rbind(prior, opt$par)
		xpred <- .doubleLog(opt$par, t)	
	} 
	if (plot) {
		llply(opt.l, function(opt) {
			xpred <- .doubleLog(opt$par, t)
			lines(t, xpred, col="cyan")
		})
		lines(t, xpred, col="blue", lwd=2)
	}
	if (opt$convergence != 0) {
		opt$par[] <- NA
		xpred <- rep(NA, length(tout))
	} else {
		xpred <- .doubleLog(opt$par, tout)
	}
	    xpred <- .backnormalize(xpred, sf=sf)
xpred.out <- zoo(xpred, order.by=t)
names(opt$par) <- paste("m", 1:7, sep="")
    if (hessian) {
        opt.new <- optim(opt$par, .error, x=x, t=t, method="BFGS", hessian=TRUE
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

fit.formula <- expression(m1 + (m2 - m7 * t) * ( (1/(1 + exp(((m3/m4) - t)/(1/m4)))) - (1/(1 + exp(((m5/m6) - t)/(1/m6))))))
output <- list(predicted=xpred.out, params=opt$par, formula=fit.formula, sf=sf)
if (hessian) output <- list(predicted = xpred.out, params = opt$par, formula = fit.formula, stdError=std.errors, sf=sf)
return(output)	
}
