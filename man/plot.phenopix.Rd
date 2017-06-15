\name{plot.phenopix}
\alias{plot.phenopix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting \code{phenopix} objects. 
}
\description{
Plotting method for objects of class \code{phenopix}
}
\usage{
\method{plot}{phenopix}(x, y, what, main, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{phenopix}
}
  \item{y}{
  It must be \code{NULL}
}
  \item{what}{
A character vector of length 1. Available options are: \code{all} (showing both fitting curve and thresholds), \code{fitting} (showing only fitting curve) \code{thresholds}, and \code{params} to show boxplots of thresholds and curve parameters (if appropriate) when the uncertainty is computed.  
}
  \item{main}{
  A main title for the plot. If not specified the title will be build with fit name and threshold name. 
}

  \item{\dots}{
  For the plotting function, a number of parameters from generic \code{plot} 
  can be specified. Note that graphic properties of fitted lines and thresholds
  cannot be modified. See examples. 
}
}
\details{
A dedicated plotting function for objects of class \code{phenopix}. The default shows observed 
values as a grey line, the fitted function in black and extracted thresholds in \code{palette()} 
colors. If available, uncertainty is also shown with a family of lightgrey curves and as error 
bars on the extracted thresholds.    
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\examples{
\dontrun{
data(bartlett2009.filtered)
fitted <- greenProcess(bartlett2009.filtered, 'klosterman', 
  'gu', plot=FALSE, uncert=TRUE, nrep=5, ncores=2)
plot(fitted) ## default
## slightly more elaborated, with suppression of default title 
plot(fitted, type='p', pch=20, ylab='GCC', xlab='DOY', col='grey', 
  what='all', main='Fit + thresholds')
## only with the fitting curve
plot(fitted, type='p', pch=20, ylab='GCC', xlab='DOY', col='grey', 
  what='fitting', main='Fit only')
## show a boxplot of thresholds extracted from the uncertainty estimation
plot(fitted, what='thresholds')
}
}
