
PhenoBP <- function(x, breaks=3, confidence=0.95, plot=TRUE, ...) {

bp.tmp <- breakpoints(x~index(x), breaks=breaks, ...)
interval <- confint(bp.tmp, level=confidence)
interval.index <- index(x)[as.vector(interval$confint)]
interval.matrix <- matrix(interval.index, nrow=3, byrow=T)
tails <- (1-confidence)/2*100
first <- paste0(tails, '%')
last <- paste0((100-tails), '%')
rownames(interval.matrix) <- c(first, 'mean', last)
metrics <- as.data.frame(interval.matrix)
names(metrics) <- paste0('bp', 1:length(metrics))

if (plot) {
  ylons <- mean(x, na.rm=TRUE)
  plot(x, ...)
  colors <- palette()[1:length(metrics)]
    abline(v=metrics[2,], col=colors)
    text(metrics[2, ], y=ylons*1.02, labels=names(metrics), col=colors) 
    for (a in 1:length(metrics)) arrows(metrics[1,a], ylons, metrics[3,a], ylons, code=3, angle=90, length=0.1, 
      lwd=2, col=colors[a])
    lines(index(x), fitted(bp.tmp), col='blue', lwd=2)
}
return(metrics)
}

