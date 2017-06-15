\name{ElmoreFit}
\alias{ElmoreFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  A function to fit a double logisitic function to a vector according to Elmore et al. (2012)  
}
\description{
This function fits a double logistic curve to observed values using the
 function as described in Elmore et al. (2012) (equation 4). It can also
 provide and uncertainty estimation. 
}
\usage{
ElmoreFit(ts, uncert = FALSE, nrep = 100, ncores='all', 
sf=quantile(ts, probs=c(0.05, 0.95), na.rm=TRUE))
}
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
 Elmore et al. 2012 and provides an uncertainty estimation. Parameters are
 estimated by a call to the function \code{\link{FitDoubleLogElmore}} from the 
 greenbrown package. Uncertainty is computed by adding noise to the raw
 data and by estimating again the parameters. Noise is added according
 to the standard deviation of the residuals (fitted - observed).
 The procedure is repeated nrep times.    
}
\value{
    A list containing the following items. 
\item{fit}{A list as returned by the function FitDoubleLogElmore}
\item{uncertainty}{A list containing a zoo data.frame with the
  uncertainty predicted values, and a dataframe containing the
  respective uncertainty curve parameters}
}
\references{
  Elmore, A.J., S.M. Guinn, B.J. Minsley and A.D. Richardson (2012): 
  Landscape controls on the timing of spring, autumn, and growing season 
  length in mid-Atlantic forests. - Global Change Biology 18, 656-674.
}
\author{
  Gianluca Filippa <gian.filippa@gmail.com>
}

\seealso{
\code{\link{FitDoubleLogElmore}}
}
\examples{

\dontrun{
library(zoo)
data(bartlett2009.filtered)
## fit without uncertainty estimation
fitted.elmore <- ElmoreFit(bartlett2009.filtered)
days <- as.numeric(format(index(bartlett2009.filtered), '%j'))
plot(days, bartlett2009.filtered)
lines(fitted.elmore$fit$predicted, col='red')
## look at fitting parameters
fitted.elmore$fit$params
## look at fitting equation, where t is time
fitted.elmore$fit$formula
}
}
