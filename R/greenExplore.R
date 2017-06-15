greenExplore <- function(x, ...) {
    ## a function to extract rmse
    .rmse <- function(fit) {
        if (class(fit)=='try-error' || class(fit$fit)=='try-error' || all(is.na(extract(fit, 'fitted')))) return(NA) else {
            fitted <- extract(fit, what='fitted')
            obs <- extract(fit, what='data')
            napos <- which(is.na(fitted))
            if (length(napos)!=0) {
                obs <- obs[-napos]
                fitted <- fitted[-napos]
            }
            summary(lm(fitted ~ obs))$sigma
        }
    }
    ## the fittings and their rmses
    print('Fitting spline 1/5')
    fit.spline <- try(greenProcess(x, fit='spline', plot=FALSE, ...))
    spline.rmse <- .rmse(fit.spline)
    print('Fitting Beck 2/5')
    fit.beck <- try(greenProcess(x, fit='beck', plot=FALSE, ...))
    beck.rmse <- .rmse(fit.beck)
    print('Fitting Elmore 3/5')
    fit.elmore <- try(greenProcess(x, fit='elmore', plot=FALSE, ...))
    elmore.rmse <- .rmse(fit.elmore)
    print('Fitting Klosterman 4/5')
    fit.klosterman <- try(greenProcess(x, fit='klosterman', plot=FALSE, ...))
    klosterman.rmse <- .rmse(fit.klosterman)
    print('Fitting Gu 5/5')
    fit.gu <- try(greenProcess(x, fit='gu', plot=FALSE, ...))
    gu.rmse <- .rmse(fit.gu)
    rmses <- c(spline.rmse, beck.rmse, elmore.rmse, klosterman.rmse, gu.rmse)
    names(rmses) <- c('spline', 'beck', 'elmore', 'klosterman', 'gu')
    ## update fittings with thresholds
    fit.spline.trs <- try(update(fit.spline, 'trs', plot=FALSE))
    fit.spline.der <- try(update(fit.spline, 'derivatives', plot=FALSE))
    fit.spline.kl <- try(update(fit.spline, 'klosterman', plot=FALSE))
    fit.spline.gu <- try(update(fit.spline, 'gu', plot=FALSE))
    fit.beck.trs <- try(update(fit.beck, 'trs', plot=FALSE))
    fit.beck.der <- try(update(fit.beck, 'derivatives', plot=FALSE))
    fit.beck.kl <- try(update(fit.beck, 'klosterman', plot=FALSE))
    fit.beck.gu <- try(update(fit.beck, 'gu', plot=FALSE))
    fit.elmore.trs <- try(update(fit.elmore, 'trs', plot=FALSE))
    fit.elmore.der <- try(update(fit.elmore, 'derivatives', plot=FALSE))
    fit.elmore.kl <- try(update(fit.elmore, 'klosterman', plot=FALSE))
    fit.elmore.gu <- try(update(fit.elmore, 'gu', plot=FALSE))
    fit.klosterman.trs <- try(update(fit.klosterman, 'trs', plot=FALSE))
    fit.klosterman.der <- try(update(fit.klosterman, 'derivatives', plot=FALSE))
    fit.klosterman.kl <- try(update(fit.klosterman, 'klosterman', plot=FALSE))
    fit.klosterman.gu <- try(update(fit.klosterman, 'gu', plot=FALSE))
    fit.gu.trs <- try(update(fit.gu, 'trs', plot=FALSE))
    fit.gu.der <- try(update(fit.gu, 'derivatives', plot=FALSE))
    fit.gu.kl <- try(update(fit.gu, 'klosterman', plot=FALSE))
    fit.gu.gu <- try(update(fit.gu, 'gu', plot=FALSE))
    ## build the list of fittings + rmses
    all.fit.list <- list(spline.trs=fit.spline.trs, spline.der=fit.spline.der,
                         spline.klosterman=fit.spline.kl, spline.gu=fit.spline.gu,
                         beck.trs=fit.beck.trs, beck.der=fit.beck.der,
                         beck.klosterman=fit.beck.kl, beck.gu=fit.beck.gu,
                         elmore.trs=fit.elmore.trs, elmore.der=fit.elmore.der,
                         elmore.klosterman=fit.elmore.kl, elmore.gu=fit.elmore.gu,
                         klosterman.trs=fit.klosterman.trs, klosterman.der=fit.klosterman.der,
                         klosterman.klosterman=fit.klosterman.kl, klosterman.gu=fit.klosterman.gu,
                         gu.trs=fit.gu.trs, gu.der=fit.gu.der,
                         gu.klosterman=fit.gu.kl, gu.gu=fit.gu.gu, rmses=rmses
                         )
    return(all.fit.list)
}
