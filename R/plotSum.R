plotSum <- function(ts, sum, which, v=NULL, quantile=TRUE, ...) {
    if (which=='trs' | which=='derivatives' | which=='trs.der') col.pos <- c(1,2,4) else col.pos <- 1:4
    data.to.plot <- sum[[which]][,col.pos]
    col.check <- which(apply(data.to.plot, 2, function(x) all(is.na(x) | is.infinite(x)))==FALSE)
    data.to.plot <- data.to.plot[,col.check]
    if (class(index(ts))=='POSIXct') index(ts) <- as.numeric(format(index(ts), '%j'))
        plot(ts, type='n', ...)
    if (is.null(v)) v <- quantile(ts, na.rm=TRUE, probs=seq(0.2,0.8, length.out=ncol(data.to.plot)))
        for (a in 1:ncol(data.to.plot)) {
            x.coords <- data.to.plot[1:2,a]
            x.coords1 <- data.to.plot[3:4,a]
            colors <- palette()[1:ncol(data.to.plot)]
            ylims <- par('usr')[3:4]
            if (!quantile) polygon(c(x.coords, rev(x.coords)), c(ylims[1],ylims[1],ylims[2],ylims[2]), 
                border=NA, col=colors[a], density=30)
            polygon(c(x.coords1, rev(x.coords1)), c(ylims[1],ylims[1],ylims[2],ylims[2]), 
                border=NA, col=colors[a], density=60)
        }
        text(x=data.to.plot[5,], y=v, label=colnames(data.to.plot), font=2, col=colors)
        points(data.to.plot[5,], rep(quantile(ts, 0.9, na.rm=TRUE),ncol(data.to.plot)), pch=19)
        points(data.to.plot[6,], rep(quantile(ts, 0.95, na.rm=TRUE),ncol(data.to.plot)), pch=17)
        legend('topleft', pch=c(17, 19), legend=c('median', 'mean'), bty='n')
        lines(ts, lwd=3)
    }
