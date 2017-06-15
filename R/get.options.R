get.options <- function() {
	## get filtering options
	filter.options <- list(night.filter=list(threshold=0.2),
                      blue.filter=list(threshold=0.05),
                      mad.filter=list(z=15),
                      max.filter=list(w=3, qt=0.9),
                      spline.filter=list(stdup=4, stddown=4, loop_spline=20))
                      return(filter.options)
}