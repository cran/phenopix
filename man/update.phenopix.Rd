\name{update.phenopix}
\alias{update.phenopix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update \code{phenopix} objects. 
}
\description{
Update method for objects of class \code{phenopix}
}
\usage{
\method{update}{phenopix}(object, threshold, envelope, quantiles, uncert, plot, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object of class \code{phenopix}
}
  \item{threshold}{
One between 'spline', 'derivatives', 'klosterman', 'gu' 
}
  \item{envelope}{
If left blank is recicled from original fitting. See \code{PhenoExtract}
}
  \item{quantiles}{
If left blank is recicled from original fitting. See \code{PhenoExtract}
}
  \item{uncert}{
If left blank is recicled from original fitting. See \code{PhenoExtract}
}
  \item{plot}{
If left blank is recicled from original fitting. See \code{PhenoExtract}
}

  \item{\dots}{  
Plotting arguments. See \code{PhenoExtract}
}
}
\details{
This function allows to update a phenopix object in output from \code{greenProcess} to extract
different thresholds withoud refitting the data (which is highly time-consuming when uncertainty
is computed). All arguments except threshold may be left blank and will be recycled from the 
original fit. But the can also be changed. See \code{PhenoExtract} where arguments are described 
in detail.
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\examples{
data(bartlett2009.filtered)
fitted <- greenProcess(bartlett2009.filtered, 'klosterman', 
  'klosterman', plot=FALSE)
fitted.gu <- update(fitted, 'gu')
}

