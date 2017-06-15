\name{fitted.phenopix}
\alias{fitted.phenopix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns predicted values from \code{phenopix} objects. 
}
\description{
Returns predicted values from \code{phenopix} objects. 
}
\usage{
\method{fitted}{phenopix}(object,\dots)
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
Returns predicted values from \code{phenopix} objects. 
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\examples{
library(zoo)
data(bartlett2009.filtered)
fitted <- greenProcess(bartlett2009.filtered, 'klosterman', 
  'klosterman', plot=FALSE)
plot(fitted$data, type='p')
lines(index(fitted$data), fitted(fitted), col='red')
}

