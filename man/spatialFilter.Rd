\name{spatialFilter}
\alias{spatialFilter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Explore all possible fitting and theshold methods from the package phenopix
}
\description{
This function allows to fast compute all fitting and thresholding methods on a given greennes time series.   
}
\usage{
spatialFilter(spatial.list, filter=c('night','spline', 'max'), 
  filter.options=NULL, 
    ncores='all',log.file=NULL, NDVI=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spatial.list}{
  An object in output from function \code{extractVIs()} with \code{spatial = TRUE}. 
}
 \item{filter}{
  Character names for filters. Chose one (or more) between 'night',
  'max', 'spline', 'blue' and 'mad'. The order provided in this argument
  determines the order of application of filters. See details for further information on filters.
}
 \item{filter.options}{
  If NULL filter.options defaults to set values, otherwise they must be
  specified in a named list. Filter options are designed to work with a wide range of data, 
  the user must change them with caution.  
}
  \item{ncores}{
Number of processors to be used in parallel computation, defaults to 'all' which will accidentally slow down any other activity on your computer. Otherwise set the number of processors you want to use in parallelization. 
%%     ~~Describe \code{roi.name} here~~
}
 \item{log.file}{
  It can be NULL or a path where to generate and refresh a txt file which logs the progress of the filtering procedure  
}
 \item{NDVI}{
 If TRUE filter is applied to NDVI values, data must be formatted differently from usual filtering. 
}
}
\details{
This function performs the same task of \code{autoFilter} but in a pixel-by-pixel analysis
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\seealso{
\code{\link{autoFilter}}
}




