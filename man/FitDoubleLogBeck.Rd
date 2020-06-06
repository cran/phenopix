\name{FitDoubleLogBeck}
\alias{FitDoubleLogBeck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A fit
%%  ~~function to do ... ~~
}
\description{
See FitDoubleLogBeck from package greenbrown
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FitDoubleLogBeck(x, t = index(x), tout = t, weighting = TRUE, 
    return.par = FALSE, plot = FALSE, hessian=FALSE, 
    sf=quantile(x, probs=c(0.05, 0.95), na.rm=TRUE), ...)
}
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
  \item{weighting}{
  Should fit be weighted? 
}
  \item{return.par}{
  Currently unused. 
}
  \item{plot}{
  Currently unused. 
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