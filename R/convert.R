convert <- function(x, year=NULL) {
	if (class(x)!='ts' |  class(x)!='zoo')
	time.new <- index(x)
	x.df <- as.data.frame(x)
	rownames(x.df) <- NULL
	x.df$doy <- time.new
	if (!is.null(year)) {
		time.posix <- as.POSIXct(strptime(paste(year, time.new, sep='-'), format='%Y-%j'))
		x.df$time <- time.posix
	}
	return(x.df)
}