FitDoubleLogKlHeavy <-
function(
    ##title<<
    ## Fit a double logisitic function to a vector according to Elmore et al. (2012)
    ##description<<
    ## This function fits a double logistic curve to observed values using the function as described in Elmore et al. (2012) (equation 4).

    x,
### vector or time series to fit

    t = index(x),
### time steps

    tout = t,
### time steps of output (can be used for interpolation)

    max.iter=200,
    sf=quantile(x, probs=c(0.05, 0.95), na.rm=TRUE), 
### if TRUE the function returns parameters of the double logisitic fit, if FALSE it returns the fitted curve

### plot iterations for logistic fit?

    ...
### further arguments (currently not used)

    ##references<<
    ## Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): Landscape controls on the timing of spring, autumn, and growing season length in mid-Atlantic forests. - Global Change Biology 18, 656-674.

    ##seealso<<
    ## \code{\link{TSGFdoublelog}}, \code{\link{Phenology}}

    ) {
    .normalize <- function(x, sf) (x-sf[1])/(sf[2]-sf[1])
        .backnormalize <- function(x, sf) (x+sf[1]/(sf[2]-sf[1]))*(sf[2]-sf[1])    
        if (any(is.na(x))) stop('NA in the time series are not allowed: fill them with e.g. na.approx()')
        if (class(index(x))[1]=='POSIXct') {
        doy.vector <- as.numeric(format(index(x), '%j'))
        index(x) <- doy.vector
    }
## integrare da tmp.R per la funzione best.nls e tutte le sue dipendenze
## la teniamo strutturata cos e aggiustiamo i dati in modo che funzioni

.rss <- function(fit, gcc) {
    sum.all <- sum(abs(predict(fit)-gcc))/sqrt(length(gcc))
}

.best.nls <- function(data, comb, max.iter=NULL, params, plot=FALSE) {
    the.funct <- formula(max.filtered~ (a1*t + b1) + (a2*t^2 + b2*t + c)*(1/(1+q1*exp(-B1*(t-m1)))^v1 - 1/(1+q2*exp(-B2*(t-m2)))^v2))

    #     offset + (upper/(1+q1*exp(-b1*(t-m1)))^v1 -
    #                                             upper/(1+q2*exp(-b2*(t-m2)))^v2))
    uniqued <- comb
    if (is.null(max.iter)) {counter <- dim(uniqued)[1]} else {
        counter <- max.iter
        sample.vect <- sample(1:dim(uniqued)[1], max.iter)
        uniqued <- uniqued[sample.vect, ]
    }
    all.rss <- numeric(counter)

    for (a in 1:counter) {
        row <- uniqued[a,]
        cfc <- params[which(names(params) %in% row[1]==TRUE)]
        fit.klos1 <- try(nls(the.funct, data=data, start=cfc), silent=TRUE)
        if (class(fit.klos1)=='try-error') {
            rss.final <- 999 } else {
                rss1 <- .rss(fit.klos1, data$max.filtered)
                cfc <- coefficients(fit.klos1)
                cfc[2] <- params[which(names(params) %in% row[2]==TRUE)]
                names(cfc)[2] <- row[2]
                ##step 2 offset and b1
                fit.klos2 <- try(nls(the.funct, data=data, start=cfc), silent=TRUE)
                if (class(fit.klos2)=='try-error') {
                    last.fit <- fit.klos1
                    rss.final <- rss1
                } else {
                    rss2 <- .rss(fit.klos2, data$max.filtered)
                    cfc <- coefficients(fit.klos2)
                    cfc[3] <-  params[which(names(params) %in% row[3]==TRUE)]
                    names(cfc)[3] <- row[3]
                    ##step 3 offset b1 b2
                    fit.klos3 <- try(nls(the.funct, data=data, start=cfc), silent=TRUE)
                    if (class(fit.klos3)=='try-error') {
                        last.fit <- fit.klos2
                        rss.final <- rss2
                    } else {
                        rss3 <- .rss(fit.klos3, data$max.filtered)
                        cfc <- coefficients(fit.klos3)
                        cfc[4] <- params[which(names(params) %in% row[4]==TRUE)]
                        names(cfc)[4] <- row[4]
                        ##step 4 offset b1 b2 q1
                        fit.klos4 <- try(nls(the.funct, data=data, start=cfc), silent=TRUE)
                        if (class(fit.klos4)=='try-error') {
                            last.fit <- fit.klos3
                            rss.final <- rss3
                        } else {
                            rss4 <- .rss(fit.klos4, data$max.filtered)
                            cfc <- coefficients(fit.klos4)
                            cfc[5] <- params[which(names(params) %in% row[5]==TRUE)]
                            names(cfc)[5] <- row[5]
                            ##step 5 offset b1 b2 q1 q2
                            fit.klos5 <- try(nls(the.funct, data=data, start=cfc), silent=TRUE)
                            if (class(fit.klos5)=='try-error') {
                                last.fit <- fit.klos4
                                rss.final <- rss4
                            } else {
                                rss5 <- .rss(fit.klos5, data$max.filtered)
                                cfc <- coefficients(fit.klos5)
                                cfc[6] <- params[which(names(params) %in% row[6]==TRUE)]
                                names(cfc)[6] <- row[6]
                                ##step 6
                                fit.klos6 <- try(nls(the.funct, data=data,
                                                     start=cfc), silent=TRUE)
                                if (class(fit.klos6)=='try-error') {
                                    last.fit <- fit.klos5
                                    rss.final <- rss5
                                } else {
                                    rss6 <- .rss(fit.klos6, data$max.filtered)
                                    cfc <- coefficients(fit.klos6)
                                    cfc[7] <- params[which(names(params) %in% row[7]==TRUE)]
                                    names(cfc)[7] <- row[7]
                                    ##step 6
                                    fit.klos7 <- try(nls(the.funct, data=data,
                                                         start=cfc), silent=TRUE)
                                    if (class(fit.klos7)=='try-error') {
                                        last.fit <- fit.klos6
                                        rss.final <- rss6
                                    } else {
                                        rss7 <- .rss(fit.klos7, data$max.filtered)
                                        cfc <- coefficients(fit.klos7)
                                        cfc[8] <- params[which(names(params) %in% row[8]==TRUE)]
                                        names(cfc)[8] <- row[8]
                                        ##step 6
                                        fit.klos8 <- try(nls(the.funct, data=data,
                                                             start=cfc), silent=TRUE)
                                        if (class(fit.klos8)=='try-error') {
                                            last.fit <- fit.klos7
                                            rss.final <- rss7
                                        } else {
                                            rss8 <- .rss(fit.klos8, data$max.filtered)
                                            cfc <- coefficients(fit.klos8)
                                            cfc[9] <- params[which(names(params) %in% row[9]==TRUE)]
                                            names(cfc)[9] <- row[9]
                                            ##step 6
                                            fit.klos9 <- try(nls(the.funct, data=data,
                                                                 start=cfc), silent=TRUE)
                                            if (class(fit.klos9)=='try-error') {
                                                last.fit <- fit.klos8
                                                rss.final <- rss8
                                            } else {
                                                rss9 <- .rss(fit.klos9, data$max.filtered)
                                                cfc <- coefficients(fit.klos9)
                                                cfc[10] <- params[which(names(params) %in% row[10]==TRUE)]
                                                names(cfc)[10] <- row[10]
                                                ##step 6
                                                fit.klos10 <- try(nls(the.funct, data=data,
                                                                      start=cfc), silent=TRUE)
                                                if (class(fit.klos10)=='try-error') {
                                                    last.fit <- fit.klos9
                                                    rss.final <- rss9
                                                } else {
                                                    last.fit <- fit.klos10
                                                    rss.final <- .rss(fit.klos10, data$max.filtered)
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        ## lines(t, predict(last.fit), col=colors()[a])
        all.rss[a] <- rss.final
    #     print(paste(round(a/counter*100), '% iteractions done', sep=''))
    }

    rss.min <- all.rss[which.min(all.rss)]
    min.pos <- which.min(all.rss)
    row <- uniqued[min.pos,]
    cfc <- params[which(names(params) %in% row[1]==TRUE)]
    fit.klos1 <- try(nls(the.funct, data=data,
                         start=cfc), silent=TRUE)
    if (class(fit.klos1)=='try-error') {
        rss.final <- 999 } else {
            rss1 <- .rss(fit.klos1, gcc=data$max.filtered)
            cfc <- coefficients(fit.klos1)
            cfc[2] <- params[which(names(params) %in% row[2]==TRUE)]
            names(cfc)[2] <- row[2]
            ##step 2 offset and b1
            fit.klos2 <- try(nls(the.funct, data=data,
                                 start=cfc), silent=TRUE)
            if (class(fit.klos2)=='try-error') {
                last.fit <- fit.klos1
                rss.final <- rss1
            } else {
                rss2 <- .rss(fit.klos2, data$max.filtered)
                cfc <- coefficients(fit.klos2)
                cfc[3] <-  params[which(names(params) %in% row[3]==TRUE)]
                names(cfc)[3] <- row[3]
                ##step 3 offset b1 b2
                fit.klos3 <- try(nls(the.funct, data=data,
                                     start=cfc), silent=TRUE)
                if (class(fit.klos3)=='try-error') {
                    last.fit <- fit.klos2
                    rss.final <- rss2
                } else {
                    rss3 <- .rss(fit.klos3, data$max.filtered)
                    cfc <- coefficients(fit.klos3)
                    cfc[4] <- params[which(names(params) %in% row[4]==TRUE)]
                    names(cfc)[4] <- row[4]
                    ##step 4 offset b1 b2 q1
                    fit.klos4 <- try(nls(the.funct, data=data,
                                         start=cfc), silent=TRUE)
                    if (class(fit.klos4)=='try-error') {
                        last.fit <- fit.klos3
                        rss.final <- rss3
                    } else {
                        rss4 <- .rss(fit.klos4, data$max.filtered)
                        cfc <- coefficients(fit.klos4)
                        cfc[5] <- params[which(names(params) %in% row[5]==TRUE)]
                        names(cfc)[5] <- row[5]
                        ##step 5 offset b1 b2 q1 q2
                        fit.klos5 <- try(nls(the.funct, data=data,
                                             start=cfc), silent=TRUE)
                        if (class(fit.klos5)=='try-error') {
                            last.fit <- fit.klos4
                            rss.final <- rss4
                        } else {
                            rss5 <- .rss(fit.klos5, data$max.filtered)
                            cfc <- coefficients(fit.klos5)
                            cfc[6] <- params[which(names(params) %in% row[6]==TRUE)]
                            names(cfc)[6] <- row[6]
                            ##step 6
                            fit.klos6 <- try(nls(the.funct, data=data,
                                                 start=cfc), silent=TRUE)
                            if (class(fit.klos6)=='try-error') {
                                last.fit <- fit.klos5
                                rss.final <- rss5
                            } else {
                                rss6 <- .rss(fit.klos6, data$max.filtered)
                                cfc <- coefficients(fit.klos6)
                                cfc[7] <- params[which(names(params) %in% row[7]==TRUE)]
                                names(cfc)[7] <- row[7]
                                ##step 6
                                fit.klos7 <- try(nls(the.funct, data=data,
                                                     start=cfc), silent=TRUE)
                                if (class(fit.klos7)=='try-error') {
                                    last.fit <- fit.klos6
                                    rss.final <- rss6
                                } else {
                                    rss7 <- .rss(fit.klos7, data$max.filtered)
                                    cfc <- coefficients(fit.klos7)
                                    cfc[8] <- params[which(names(params) %in% row[8]==TRUE)]
                                    names(cfc)[8] <- row[8]
                                    ##step 6
                                    fit.klos8 <- try(nls(the.funct, data=data,
                                                         start=cfc), silent=TRUE)
                                    if (class(fit.klos8)=='try-error') {
                                        last.fit <- fit.klos7
                                        rss.final <- rss7
                                    } else {
                                        rss8 <- .rss(fit.klos8, data$max.filtered)
                                        cfc <- coefficients(fit.klos8)
                                        cfc[9] <- params[which(names(params) %in% row[9]==TRUE)]
                                        names(cfc)[9] <- row[9]
                                        ##step 6
                                        fit.klos9 <- try(nls(the.funct, data=data,
                                                             start=cfc), silent=TRUE)
                                        if (class(fit.klos9)=='try-error') {
                                            last.fit <- fit.klos8
                                            rss.final <- rss8
                                        } else {
                                            rss9 <- .rss(fit.klos9, data$max.filtered)
                                            cfc <- coefficients(fit.klos9)
                                            cfc[10] <- params[which(names(params) %in% row[10]==TRUE)]
                                            names(cfc)[10] <- row[10]
                                            ##step 6
                                            fit.klos10 <- try(nls(the.funct, data=data,
                                                                  start=cfc), silent=TRUE)
                                            if (class(fit.klos10)=='try-error') {
                                                last.fit <- fit.klos9
                                                rss.final <- rss9
                                            } else {
                                                last.fit <- fit.klos10
                                                rss.final <- .rss(fit.klos10, data$max.filtered)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    output <- list(rss=rss.final, fit=last.fit, par=params)
    ## gestire l'estrazione dei massimi e minimi con local maxima e minima
}

## format data

    n <- length(x)
    x <- .normalize(x, sf=sf)
    avg <- mean(x, na.rm=TRUE)
    mx <- max(x, na.rm=TRUE)
    mn <- min(x, na.rm=TRUE)
    ampl <- mx - mn

## get parameters
    doy <- quantile(t, c(0.25, 0.75), na.rm=TRUE)
    # doy2 <- diff(doy)
    a1 <- 0 #ok
    a2 <- 0 #ok
    b1 <- mn #ok
    b2 <- 0 #ok
    c <- 0.2*max(x) # ok 
    ## very slightly smoothed spline to get reliable maximum
    tmp <- smooth.spline(x, df=0.5*length(x))
    doy.max <- which.max(tmp$y)
    B1 <- 4/(doy.max-doy[1])
    B2 <- -4/(doy[2]-doy.max)
    m1 <- doy[1] + 0.5*(doy.max-doy[1])
    m2 <- doy.max + 0.5*(doy[2]-doy.max)
    m1.bis <- doy[1]
    m2.bis  <- doy[2]  
    q1 <- 0.5 #ok
    q2 <- 0.5 #ok
    v1 <- 2 # ok
    v2 <- 2 # ok

    # offset <- min(ts, na.rm=T)
    # upper <- diff(range(ts, na.rm=T))
    # ## offset <- 0.3
    # a1 <- 0.00003
    # b1 <- 0.05
    # a2 <- 0.001
    # b2 <- 0.08
    # c <- 0
    # q1 <- 1
    # q2 <- 1
    # m1 <- 130
    # m2 <- 250
    # v1 <- 1.3
    # v2 <- 1
    ## upper <- 0.05
    all.pars <- list(a1=a1, a2=a2, b1=b1, b2=b2, c=c, B1=B1, B2=B2, m1=m1, m2=m2, q1=q1,q2=q2, v1=v1, v2=v2)
    data <- data.frame(max.filtered=as.vector(x), t=index(x))
    ## days <- tt$days
    ## gcc <- tt$max.filtered

par.names <- c('a1', 'a2', 'b1','b2', 'c', 'B1','B2', 'm1', 'm2', 'q1','q2', 'v1', 'v2')
## get a random sample of parameters (ten times larger than actual number of max.iter so that we make sure 
## we can sample at least max.iter of unique()d parameters order)

reps <- max.iter *10

df.pars <- data.frame(matrix(nrow=reps, ncol=length(par.names)))
for (a in 1:reps) {
    df.pars[a, ] <- unlist(sample(par.names,length(par.names)))
}
## remove replicates if any
uniqued.tmp <- unique(df.pars)
## sample n=max.iter different combinations of params
uniqued <- uniqued.tmp[sample(dim(uniqued.tmp)[1], 200), ]

nls.best <- .best.nls(data, comb=uniqued, max.iter=max.iter, params=all.pars)
predicted <- predict(nls.best$fit)
predicted <- .backnormalize(predicted, sf=sf)
predicted <- zoo(predicted, order.by=t)
pars <- nls.best$par
par.choosen <- pars
for (a in 1:length(pars)) {
    act.name <- names(pars)[a]
    check <- act.name %in% names(coef(nls.best$fit))
    if (check==TRUE) {
        pos.in.fit <- names(coef(nls.best$fit)) %in% act.name 
        par.choosen[[a]]  <- coef(nls.best$fit)[pos.in.fit]
}
}
the.names <- names(par.choosen)
unlisted.pars <- as.vector(unlist(par.choosen))
names(unlisted.pars) <- the.names
fit.formula <- expression((a1*t + b1) + (a2*t^2 + b2*t + c)*(1/(1+q1*exp(-B1*(t-m1)))^v1 - 1/(1+q2*exp(-B2*(t-m2)))^v2))
output <- list(predicted=predicted, params=unlisted.pars, formula=fit.formula, sf=sf)
return(output)
}
