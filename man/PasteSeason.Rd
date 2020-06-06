\name{PasteSeason}
\alias{PasteSeason}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  A plotting facility for seasonal data with multiple cycles 
  }
\description{
After cutting and processing a timeseries with \code{CutSeason}, this function allows
to plot the extracted thresholds in a single plot. 
} 
\usage{
PasteSeason(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A list of objects as in output from 
  \code{\link{greenProcess}}. All subseasonal fittings 
  must be listed in a list and passed to the present function. 
}
}
\details{
This is a plotting facility which allows to plot in a single graph multiple fitting
output from \code{\link{greenProcess}}.  
}
\author{
  Gianluca Filippa <gian.filippa@gmail.com>
}
\seealso{
  \code{\link{CutSeason}}
}
