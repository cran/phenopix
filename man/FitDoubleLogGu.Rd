\name{FitDoubleLogGu}
\alias{FitDoubleLogGu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 A function to fit a double logisitic function to a vector according to Gu et al. (2003)  
}
\description{
   This function fits a double logistic curve to observed values using the
 function as described in Gu et al. (2003). 
}
\usage{
FitDoubleLogGu(x, t = index(x), tout = t, hessian=FALSE, 
sf=quantile(x, probs=c(0.05, 0.95), na.rm=TRUE), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A vector or, better, an univariate ts or zoo object. 
}
  \item{t}{
  A vector of time (in numeric doys), if not provided index(x) is
  retrieved. 
}
  \item{tout}{
  For gapfilling pourposes, a vector of time steps at which the function
  can be predicted. 
}
  \item{hessian}{
  Currently unimplemented. 
}
  \item{sf}{
Scaling factors required to normalize the data prior to the fitting. If the function is called by e.g. \code{\link{greenProcess}} sf is automatically calculated.
}
  \item{\dots}{
  Further arguments currenlty unused. 
}
}
\details{
  The function estimates parameters of the double logistic equation from
 Gu et al. 2003. The wrapper function \code{GuFit} calls this function and 
 additionally allows the calculation of uncertainty. So better use \code{GuFit}. 
}
\value{
    A list containing the following items. 
\item{predicted}{Predicted values from the equation}
\item{params}{Equation parameters}
\item{formula}{The equation}
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
