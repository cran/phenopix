greenClusters <- function(data.clusters, data.curve=NULL, nclusters, formula=NULL, plot=TRUE) {
  params <- data.curve
  data <- data.clusters
  na.pos <- which(is.na(data[,1]))
  fit <- kmeans(scale(na.omit(data)), nclusters)
  fit.filled <- rep(NA, length(data[,1]))
  if (length(na.pos)!=0) fit.filled[-na.pos] <- fit$cluster else fit.filled <- fit$cluster
  if (!is.null(params)) {
  all.curves <- data.frame(matrix(nrow=365, ncol=nclusters))
  for (a in 1:nclusters) {
    act.subset <- params[fit.filled==a,]
    act.pars <- apply(act.subset, 2, median, na.rm=TRUE)
    t <- 1:365
    act.curve <- eval(formula, envir=as.list(act.pars))
    all.curves[,a] <- act.curve
  }
  zoo.curves <- zoo(all.curves, order.by=t)
  names(zoo.curves) <- paste0('cluster',1:nclusters)
  yrange <- range(zoo.curves, na.rm=TRUE)
  na.function <- function(x) {
    na.positions <- which(is.na(x) | is.infinite(x))
    if (length(x)==length(na.positions)) return(TRUE) else return(FALSE)
  }
  all.na.cols <- which(apply(zoo.curves,2, na.function)==TRUE) 
  if (length(all.na.cols!=0)) to.plot <- zoo.curves[,-all.na.cols] else to.plot <- zoo.curves

  if (plot) {
    colors <- palette()[1:length(to.plot)]
    # lwds <- table(fit$clusters)/max(table(fit$clusters), na.rm=TRUE)*3
    plot(to.plot, plot.type='single', col=colors, ylim=yrange)
    legend('topleft', col=colors, legend=names(to.plot), lty=1, bty='n')
}
} else {
  zoo.curves=NA
  }
  exit.list <- list(curves=zoo.curves, napos=na.pos, clusters=data.frame(clusters=fit.filled))  
    return(exit.list)
} 
