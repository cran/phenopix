\name{GuFit}
\alias{GuFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to fit a double logisitic function to a vector according to Gu et al. (2003) 
}
\description{
This function fits a double logistic curve to observed values using the
 function as described in Gu et al. (2003). 
}
\usage{
GuFit(ts, uncert = FALSE, nrep = 100, ncores='all', 
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
 Gu et al. 2009 and provides an uncertainty estimation. Parameters are
 estimated by a call to the function \code{\link{FitDoubleLogGu}}. Uncertainty 
 is computed by adding noise to the raw
 data and by estimating again the parameters. Noise is added according
 to the standard deviation of the residuals (fitted - observed).
 The procedure is repeated nrep times.  
}
\value{
  A list containing the following items. 
\item{fit}{A list as returned by the function FitDoubleLogGu}
\item{uncertainty}{A list containing a zoo data.frame with the
  uncertainty predicted values, and a dataframe containing the
  respective uncertainty curve parameters}
}
\references{
Gu L, Post WM, Baldocchi D, Black TA, Suyker AE, Verma SB, Vesala T, 
Wofsy SC. (2009) Characterizing the Seasonal Dynamics of Plant Community 
Photosynthesis Across a Range of Vegetation Types. In: Phenology of Ecosystem 
Processes (Ed: Noormets A, Springer New York), pp 35-58. 
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}
\seealso{
\code{\link{FitDoubleLogGu}}
}
\examples{
\dontrun{
library(zoo)
data(bartlett2009.filtered)
## fit without uncertainty estimation
fitted.gu <- GuFit(bartlett2009.filtered)
## convert 
plot(bartlett2009.filtered)
lines(fitted.gu$fit$predicted, col='red')
## look at fitting parameters
fitted.gu$fit$params
## look at fitting equation, where t is time
fitted.gu$fit$formula
}
}
