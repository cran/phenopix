FitDoubleLogGu <-
function(
    x,
    t = index(x),
    tout = t,
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
    n <- length(x)
            x <- .normalize(x, sf=sf)
    avg <- mean(x, na.rm=TRUE)
    mx <- max(x, na.rm=TRUE)
    mn <- min(x, na.rm=TRUE)
    ampl <- mx - mn
    .doubleLog <- function(par, t) {
        y0=par[1]
        a1 <- par[2]
        a2 <- par[3]
        t01 <- par[4]
        t02 <- par[5]
        b1 <- par[6]
        b2 <- par[7]
        c1 <- par[8]
        c2 <- par[9]
    xpred <- y0 + (a1/(1+exp(-(t-t01)/b1))^c1) - (a2/(1+exp(-(t-t02)/b2))^c2)
        # xpred <- (a1*t + b1) + (a2*t^2 + b2*t + c)*(1/(1+q1*exp(-B1*(t-m1)))^v1 - 1/(1+q2*exp(-B2*(t-m2)))^v2)
        return(xpred)
    }
    .error <- function(par, x, t) {
        if (any(is.infinite(par))) return(99999)
        xpred <- .doubleLog(par, t=t)
        sse <- sum((xpred - x)^2, na.rm=TRUE)
        return(sse)
    }
    doy <- quantile(t, c(0.25, 0.75), na.rm=TRUE)
    # doy2 <- diff(doy)
    y0 <- mn
    a1 <- ampl #ok
    a2 <- ampl #ok
    tmp <- smooth.spline(x, df=0.5*length(x))
    doy.max <- which.max(tmp$y)
    t01 <- doy[1] + 0.5*(doy.max-doy[1]) 
    t02 <- doy.max + 0.5*(doy[2]-doy.max) 
    b1 <- 10 #ok
    b2 <- 10 #ok
    c1 <- 1 # ok 
    c2 <- 1
  
    prior <- rbind(
        c(y0, a1, a2, t01, t02, b1, b2, c1, c2), 
        c(y0, a1, a2, t01, t02, b1, b2, 1.2, c2), 
        c(y0, 0.05, 0.05, t01, t02, 0.5, b2, c1, c2), 
        c(y0, a1, a2, doy[1], t02, b1, b2, c1, c2),         
        c(y0, a1, a2, t01, doy[2], 5, 5, c1, c2)
        )
    opt.l <- apply(prior, 1, optim, .error, x=x, t=t, method="BFGS", control=list(maxit=1000), hessian=hessian)   # fit from different prior values
    opt.df <- cbind(cost=unlist(llply(opt.l, function(opt) opt$value)), convergence=unlist(llply(opt.l, function(opt) opt$convergence)), ldply(opt.l, function(opt) opt$par))
    ## opt.df <- opt.df[-which(opt.df$V2<0),]
    best <- which.min(opt.df$cost)
    if (opt.df$convergence[best] == 1) { # if maximum iterations where reached - restart from best with more iterations
    opt <- opt.l[[best]]
    opt <- optim(opt.l[[best]]$par, .error, x=x, t=t, method="BFGS", control=list(maxit=1500), hessian=hessian)
    prior <- rbind(prior, opt$par)
    xpred <- .doubleLog(opt$par, t)
} else if (opt.df$convergence[best] == 0) {
    opt <- opt.l[[best]]
    prior <- rbind(prior, opt$par)
    xpred <- .doubleLog(opt$par, t) ## perche questo restituisce nan?
}
if (opt$convergence != 0) {
    opt$par[] <- NA
    xpred <- rep(NA, length(tout))
} else {
    xpred <- .doubleLog(opt$par, tout)
}
xpred <- .backnormalize(xpred, sf=sf)
xpred.out <- zoo(xpred, order.by=t)
names(opt$par) <- c('y0', 'a1', 'a2', 't01', 't02', 'b1', 'b2', 'c1', 'c2')
if (hessian) {
    opt.new <- optim(opt$par, .error, x=x, t=t, method="BFGS", hessian=TRUE
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
fit.formula <- expression(y0 + (a1/(1+exp(-(t-t01)/b1))^c1) - (a2/(1+exp(-(t-t02)/b2))^c2))
output <- list(predicted=xpred.out, params=opt$par, formula=fit.formula, sf=sf)
if (hessian) output <- list(predicted = xpred.out, params = opt$par, formula = fit.formula, stdError=std.errors, sf=sf)
    return(output)
}
