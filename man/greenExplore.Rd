\name{greenExplore}
\alias{greenExplore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Explore all possible fitting and theshold methods from the package phenopix
}
\description{
This function allows to fast compute all fitting and thresholding methods on a given greenness time series.   
}
\usage{
greenExplore(x, ...) 

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A zoo object with a season of filtered greeneess data
}
  \item{\dots}{
  additional arguments passed to \code{greenProcess}
}
}
\details{
This function runs all possible fitting and thresholding methods. It must be used in combination with \code{plotExplore}. The two functions were designed to give a preliminary indication of what would be the best combination of fitting curve and thresholds for your set of data. The function returns also the root mean squared error (RMSE) of the various fits, that can be used as a discriminant to choose the fit.  

}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\seealso{
\code{\link{plotExplore}}
}

\examples{
\dontrun{
data(bartlett2009.filtered)
explored.data <- greenExplore(bartlett2009.filtered)
explored.data$rmse # chech fit RMSES
plotExplore(explored.data) ## an annotated plot
}
}


