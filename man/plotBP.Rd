\name{plotBP}
\alias{plotBP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to plot results of function \code{PhenoBP}
}
\description{
A function to plot results of function \code{PhenoBP}, with confidence intervals and annotated break points
}
\usage{
plotBP(ts, breaks, bp.y, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
An univariate zoo series with measured or modelled Gcc data  
}
  \item{breaks}{
An object as in output from PhenoBP function. 
}
  \item{bp.y}{
Optional argument indicating the y coordinate where break point names are written, defaults to quantile(ts, 0.005) 
}
  \item{\dots}{
Further arguments of \code{plot} function.
}
}
\details{
A function to rapidly plot results from \code{PhenoBP} function
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}



