combineUncertainty <- function(ts, which='all', nrep=50, ncores='all') {
    all.fits <- c('beck', 'elmore', 'klosterman', 'gu')
    if (length(which)==1) included <- all.fits else included <- all.fits[all.fits %in% which]
    all.trs <- NULL
    all.der <- NULL
    all.klos <- NULL
    all.gu <- NULL
    all.fits <- NULL
    for (a in 1:length(included)) {
        print(paste0('Fitting ',toupper(included[a]),' ', a, '/', length(included)))
        act.fit <- included[a]
        the.fit <- try(greenProcess(na.approx(ts), fit=act.fit, uncert=TRUE, nrep=nrep, plot=FALSE, ncores=ncores))
        all.fits[[a]] <- the.fit
        trs.phases <- try(extract(update(the.fit, 'trs', plot=FALSE), 'metrics.uncert'))
        if (class(trs.phases)=='try-error') {
            trs.phases <- data.frame(matrix(nrow=nrep, ncol=10))
            names(trs.phases) <- c("sos","eos","los","pop","mgs","rsp","rau","peak","msp","mau")
        }
        trs.phases$fit <- act.fit
        all.trs <- rbind(all.trs, trs.phases)
        der.phases <- try(extract(update(the.fit, 'derivatives', plot=FALSE), 'metrics.uncert'))
        if (class(der.phases)=='try-error') {
            der.phases <- data.frame(matrix(nrow=nrep, ncol=10))
            names(der.phases) <- c("sos","eos","los","pop","mgs","rsp","rau","peak","msp","mau")
        }
        der.phases$fit <- act.fit
        all.der <- rbind(all.der, der.phases)
        klos.phases <- try(extract(update(the.fit, 'klosterman', plot=FALSE), 'metrics.uncert'))
        if (class(klos.phases)=='try-error') {
            klos.phases <- data.frame(matrix(nrow=nrep, ncol=4))
            names(klos.phases) <- c("Greenup","Maturity","Senescence","Dormancy")
        }
        klos.phases$fit <- act.fit
        all.klos <- rbind(all.klos, klos.phases)
        gu.phases <- try(extract(update(the.fit, 'gu', plot=FALSE), 'metrics.uncert'))
        if (class(gu.phases)=='try-error') {
            gu.phases <- data.frame(matrix(nrow=nrep, ncol=9))
            names(gu.phases) <- c("UD","SD","DD","RD","maxline","baseline","prr","psr","plateau.slope")
        }
        gu.phases$fit <- act.fit
        all.gu <- rbind(all.gu, gu.phases)
    }
    names(all.fits) <- included
    out.list <- list(trs=all.trs, derivatives=all.der, klosterman=all.klos, gu=all.gu, fits=all.fits)
    return(out.list)
} 
