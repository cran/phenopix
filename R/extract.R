extract <- function(x, what) {
if (what=='data') returned <- x$data
if (what=='fitted') returned <- fitted(x)
if (what=='metrics') returned <- x$metrics
if (what=='metrics.uncert') returned <- x$uncertainty.df
if (what=='curve.params') returned <- x$fit$fit$params
if (what=='curve.uncert') returned <- x$fit$uncertainty$predicted
if (what=='curve.params.uncert') returned <- x$fit$uncertainty$params
if (!exists('returned')) stop('wrong what: type one between "data", "fitted", "metrics",\n "metrics.uncert", "curve.params", "curve.uncert", "curve.params.uncert"')
return(returned)
}