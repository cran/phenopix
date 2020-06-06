PasteSeason <- function(x) {
	ranges.y <- NULL
	ranges.x <- NULL

	for (a in 1:length(x)) {
		ranges.y <- c(ranges.y, range(x[[a]]$data, na.rm=T))
		ranges.x <- c(ranges.x, range(index(x[[a]]$data), na.rm=T))
	}
	xlims <- range(ranges.x)
	ylims <- range(ranges.y)
	plot(0, type='n', xlim=xlims, ylim=ylims, ylab='')
	for (a in 1:length(x)) {
		PhenoPlot(x[[a]]$fit, x[[a]]$metrics, add=TRUE)
	}
}