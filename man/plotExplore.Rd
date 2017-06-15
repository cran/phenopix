\name{plotExplore}
\alias{plotExplore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot an object in output from the greenExplore function
}
\description{
This function allows to plot the object in output from the \code{greenExplore()} function.   
}
\usage{
plotExplore(x) 

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  	An object (a list) in output from \code{greenExplore}
}
}
\details{
	The combination of functions \code{greenExplore} and \code{plotExplore} are intended to provide a first view of all possible fitting and thresholding methods on a season of greenness data. In particular plotExplore combines all fittings and threhsolding methods (20 plots) on an annotaded graph.   
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\seealso{
\code{\link{greenExplore}}
}

\examples{
\dontrun{
data(bartlett2009.filtered)
explored.data <- greenExplore(bartlett2009.filtered)
explored.data$rmse # chech fit RMSES
plotExplore(explored.data) ## an annotated plot
}
}



