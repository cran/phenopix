extractParameters <- function(list, update=NULL, ...) {
	### look for first fitted element
	first.element <- list[[1]]
	i <- 1
	while (class(first.element)=='try-error') {
		first.element <- list[[i]]
		i <- i + 1
	}
if (!is.null(update)) {
	updated <- update(first.element, update, plot=FALSE)
		metrics.tmp <- extract(updated, 'metrics')
	} else metrics.tmp <- extract(first.element, 'metrics')
	params.tmp <- extract(first.element, 'curve.params')
	metrics.length <- length(metrics.tmp)
	params.length <- length(params.tmp)
	gl.length <- metrics.length + params.length + 1
	df.exit <- data.frame(matrix(ncol=gl.length, nrow=length(list)))
	names(df.exit) <- c(names(metrics.tmp), names(params.tmp), 'RMSE')	
	for (a in 1:length(list)) {
		act.element <- list[[a]]
	if (class(act.element)=='try-error') {
		exit.row <- rep(NA, gl.length)
	} else {
	if (!is.null(update)) {
		updated <- update(act.element, update, plot=FALSE, ...)
		metrics.tmp <- extract(updated, 'metrics')
	} else metrics.tmp <- extract(act.element, 'metrics')
	params.tmp <- extract(act.element, what='curve.params')
	fit.tmp <- try(lm(extract(act.element, what='fitted') ~ extract(act.element, what='data')))
    RMSE <- ifelse(class(fit.tmp)=='try-error', NA, summary(fit.tmp)$sigma)
	exit.row <- c(metrics.tmp, params.tmp, RMSE=RMSE) 	
}
	df.exit[a, ] <- exit.row
	print(a)
}
return(df.exit)
}
