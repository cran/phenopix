\name{SplineFit}
\alias{SplineFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to fit a smoothed spline to Phenological Data
}
\description{
A function to fit a smoothed spline to Phenological Data
}
\usage{
SplineFit(ts, uncert = FALSE, nrep = 100, df.factor = 0.05, 
ncores='all', sf=quantile(ts, probs=c(0.05, 0.95), na.rm=TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
A ts or zoo object with gcc data. index(ts) must be numeric days of
    year (doys)
}
  \item{uncert}{
  Should uncertainty be estimated?
}
  \item{nrep}{
  Number of relications to estimate uncertainty, defaults to 100. 
}
  \item{df.factor}{
Defaults to 0.05, it is multiplied by length(ts) to generate degrees 
of freedom for the spline fitting. The higher the number of data, the 
higher should be df factor. For a complete year of data 
(i.e. length(ts)=365) the default value is optimum.}
  \item{ncores}{
Unused argument for compatibility
%%     ~~Describe \code{roi.name} here~~
}
  \item{sf}{
Scaling factors required to normalize the data prior to the fitting. If the function is called by e.g. \code{\link{greenProcess}} sf is automatically calculated.
}
}

\details{
This function fits a smoothed spline to the data. Df for smoothing are set 
at 0.05*length(ts) by default and df.factor can be modified. 
Uncertainty is estimated by changing the degrees of freedom of the spline. 
In particular a sequence from 0.01 and df.factor, of length nrep is used as 
varying degrees of freedom for the spline fitting. 
}
\value{
  A list containing the following items. 
\item{fit}{A list with fitted values and an object named 'params' set to 
NULL, for simmetry with other fittings}
\item{uncertainty}{A list containing a zoo data.frame with the
  uncertainty predicted values, and an object  named 'params' set to 
  NULL, for simmetry with other fittings}
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\examples{
data(bartlett2009.filtered)
fitted <- SplineFit(bartlett2009.filtered, uncert=TRUE, nrep=50)
}
