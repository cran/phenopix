summarizePhases <- function(list, quantiles=c(0.25, 0.75), across.methods=FALSE) {
    list <- list[-length(list)]
    nfits <- sapply(list, function(x) table(x$fit))
    if (length(unique(as.vector(nfits)))!=1) warning('uncertainty may be umbalanced due to different number of fits for each method')
        applyer <- function(x, FUN, ...) apply(x[,-ncol(x)], 2, FUN, ...)
    quantilizer <- function(x, ...) quantile(x, probs=quantiles, ...)
    ranges <- sapply(list, applyer, 'range', na.rm=TRUE)
    quantiles.tmp <- sapply(list, applyer, 'quantilizer', na.rm=TRUE)
    means <- sapply(list, applyer, 'mean', na.rm=TRUE)
    medians <- sapply(list, applyer, 'median', na.rm=TRUE)
    out.list <- ranges
    for (a in 1:length(ranges)) {
        out.list[[a]] <- rbind(ranges[[a]], quantiles.tmp[[a]], means[[a]], medians[[a]])
        rownames(out.list[[a]])[c(1,2,5,6)] <- c('min', 'max', 'mean', 'median')
    }
    if (across.methods) {
        ## average of gu and klosterman methods, average of trs and derivatives methods
        gu.tmp <- list$gu[,1:4]
        names(gu.tmp) <- names(list$klosterman)[1:4]
        kl.gu <- rbind(list$klosterman[,1:4], gu.tmp)
        means <- apply(kl.gu, 2, mean, na.rm=TRUE)
        medians <- apply(kl.gu, 2, median, na.rm=TRUE)
        quantiles.tmp <- apply(kl.gu, 2, quantilizer, na.rm=TRUE)
        ranges <- apply(kl.gu, 2, range, na.rm=TRUE)
        kl.gu.def <- rbind(ranges, quantiles.tmp, means, medians)
        rownames(kl.gu.def)[c(1,2,5,6)] <- c('min', 'max', 'mean', 'median')
        trs.der <- rbind(list$trs[,1:4], list$derivatives[,1:4])
        means <- apply(trs.der, 2, mean, na.rm=TRUE)
        medians <- apply(trs.der, 2, median, na.rm=TRUE)
        quantiles.tmp <- apply(trs.der, 2, quantilizer, na.rm=TRUE)
        ranges <- apply(trs.der, 2, range, na.rm=TRUE)
        trs.der.def <- rbind(ranges, quantiles.tmp, means, medians)
        rownames(trs.der.def)[c(1,2,5,6)] <- c('min', 'max', 'mean', 'median')
        out.list$kl.gu <- kl.gu.def
        out.list$trs.der <- trs.der.def
    }
    return(out.list)
}
