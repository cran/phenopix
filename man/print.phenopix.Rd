\name{print.phenopix}
\alias{print.phenopix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print \code{phenopix} objects. 
}
\description{
Print method for objects of class \code{phenopix}
}
\usage{
\method{print}{phenopix}(x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
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
data(bartlett2009.filtered)
fitted <- greenProcess(bartlett2009.filtered, 'klosterman', 
  'klosterman', plot=FALSE)
print(fitted)
}

