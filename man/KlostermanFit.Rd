\name{KlostermanFit}
\alias{KlostermanFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to fit a double logisitic function to a vector according to 
Klosterman et al. (2014) 
}
\description{
This function fits a double logistic curve to observed values using the
 function as described in klosterman et al. (2014), eq 7. 
}
\usage{
KlostermanFit(ts, which = "light", uncert = FALSE, nrep = 100, 
ncores='all', sf=quantile(ts, probs=c(0.05, 0.95), na.rm=TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
   A ts or zoo object with gcc data. index(ts) must be numeric days of
    year (doys) or a POSIXct vector
}
  \item{which}{
  A character to be chosen between 'light' (default) and 'heavy'. See details. 
}
  \item{uncert}{
  Should uncertainty be estimated?
}
  \item{nrep}{
  Number of relications to estimate uncertainty, defaults to 100. 
}
  \item{ncores}{
Number of processors to be used in parallel computation, defaults to 'all' which will accidentally slow down any other activity on your computer. Otherwise set the number of processors you want to use in parallelization. 
%%     ~~Describe \code{roi.name} here~~
}
  \item{sf}{
Scaling factors required to normalize the data prior to the fitting. If the function is called by e.g. \code{\link{greenProcess}} sf is automatically calculated.
}
}
\details{
The function estimates parameters of the double logistic equation from
 Klosterman et al. 2014. Two optimization procedures are available. If 
 which='light' (the default) equation parameters are optimized using the 
 function \code{optim} and computation is faster, whereas if which='heavy' 
 the optimization procedure calls the function \code{nsl} and is based on 
 a greater number of iteractions with different first guesses for parameters. 
 This option is about ten times slower than the light one.  
}
\value{
A list containing the following items. 
\item{fit}{A list as returned by the function FitDoubleLogGu}
\item{uncertainty}{A list containing a zoo data.frame with the
  uncertainty predicted values, and a dataframe containing the
  respective uncertainty curve parameters}
}
\references{
Klosterman ST, Hufkens K, Gray JM, Melaas E, Sonnentag O, Lavine I, Mitchell L, 
Norman R, Friedl MA, Richardson A D (2014) Evaluating remote sensing of deciduous 
forest phenology at multiple spatial scales using PhenoCam imagery, Biogeosciences, 
11, 4305-4320, doi:10.5194/bg-11-4305-2014.
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}
\seealso{
\code{\link{FitDoubleLogKlLight}}
\code{\link{FitDoubleLogKlHeavy}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
library(zoo)
data(bartlett2009.filtered)
## fit without uncertainty estimation
fitted.kl1 <- KlostermanFit(bartlett2009.filtered, which='light')
fitted.kl2 <- KlostermanFit(bartlett2009.filtered, which='heavy')
## check fitting
plot(bartlett2009.filtered)
lines(fitted.kl1$fit$predicted, col='red')
lines(fitted.kl2$fit$predicted, col='blue')
legend('topleft',col=c('red', 'blue'), lty=1, 
  legend=c('light', 'heavy'), bty='n')
}
}