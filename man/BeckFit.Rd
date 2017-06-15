\name{BeckFit}
\alias{BeckFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to fit a double logisitic function to a vector according to Beck et al. (2006)  
}
\description{
 This function fits a double logistic curve to observed values using the
 function as described in Beck et al. (2006) (equation 3). It can also
 provide and uncertainty estimation. Rather internal function. 
 See \code{greenProcess}
}
\usage{
BeckFit(ts, uncert = FALSE, nrep = 100, ncores='all', 
sf=quantile(ts, probs=c(0.05, 0.95), na.rm=TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
    A ts or zoo object with gcc data. index(ts) must be numeric days of
    year (doys) or a POSIXct vector
}
  \item{uncert}{
  Should uncertainty be estimated?
}
  \item{nrep}{
  Number of relications to estimate uncertainty, defaults to 1000. 
}
  \item{ncores}{
Number of processors to be used in parallel computation, defaults to 'all' which will accidentally slow down any other activity on your computer. Otherwise set the number of processors you want to use in parallelization. 

}
  \item{sf}{
Scaling factors required to normalize the data prior to the fitting. If the function is called by e.g. \code{\link{greenProcess}} sf is automatically calculated.
}
}
\details{
 The function estimates parameters of the double logistic equation from
 Beck et al. 2006 and provides an uncertainty estimation. Parameters are
 estimated by a call to the function \code{\link{FitDoubleLogBeck}} from the 
 \code{greenbrown} package. Uncertainty is computed by adding noise to the raw
 data and by estimating again the parameters. Noise is added according
 to the standard deviation of the residuals (fitted - observed).
 The procedure is repeated nrep times.  
}

\value{
  A list containing the following items. 
\item{fit}{A list as returned by the function FitDoubleLogBeck}
\item{uncertainty}{A list containing a zoo data.frame with the
  uncertainty predicted values, and a dataframe containing the
  respective uncertainty curve parameters}
}

\references{
  Beck, P.S.A., C. Atzberger, K.A. Hodga, B. Johansen, A. Skidmore (2006): 
  Improved monitoring of vegetation dynamics at very high latitudes: A new 
  method using MODIS NDVI. - Remote Sensing of Environment 100:321-334.
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}

\examples{
\dontrun{
library(zoo)
data(bartlett2009.filtered)
## fit without uncertainty estimation
fitted.beck <- BeckFit(bartlett2009.filtered)
days <- as.numeric(format(index(bartlett2009.filtered), '%j'))
plot(days, bartlett2009.filtered)
lines(fitted.beck$fit$predicted, col='red')
## look at fitting parameters
fitted.beck$fit$params
## look at fitting equation, where t is time
fitted.beck$fit$formula
}
}

