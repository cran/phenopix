\name{summary.phenopix}
\alias{summary.phenopix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary of \code{phenopix} objects. 
}
\description{
Summary method for objects of class \code{phenopix}
}
\usage{
\method{summary}{phenopix}(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object of class \code{phenopix}
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
Prints a sintetic summary of the object.
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\examples{
\dontrun{
data(bartlett2009.filtered)
fitted <- greenProcess(bartlett2009.filtered, 'klosterman', 
  'klosterman', uncert=TRUE, plot=FALSE, nrep=5, ncores=2)
summary(fitted)
}
}
