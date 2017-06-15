\name{spatialGreen}
\alias{spatialGreen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Process pixel-based greenness indexes
}
\description{
This function allows to filter, fit a curve and extract thresholds in a 
pixel-based analysis exactly as \code{autoFilter} and \code{greenProcess} 
do in a ROI-based analysis, except that uncertainty cannot be estimated 
(since it would be too computationally intense)
}
\usage{
spatialGreen(filtered.data, fit, threshold, ncores='all', 
  log.file=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filtered.data}{
  A list as in output from \code{spatialFilter()}.   
%%     ~~Describe \code{img.path} here~~
}
  \item{fit}{
    A character vector of length 1. Available options are: \code{spline}, \code{beck}, \code{elmore},
  \code{klosterman}, \code{gu}. 
%%     ~~Describe \code{roi.path} here~~
}
  \item{threshold}{
  A character vector of length 1. Available options are: \code{spline}, \code{derivatives}, 
\code{klosterman}, \code{gu}.  

%%     ~~Describe \code{vi.path} here~~
}
  \item{ncores}{
Number of processors to be used in parallel computation, defaults to 'all' which will accidentally slow down any other activity on your computer. Otherwise set the number of processors you want to use in parallelization. 
%%     ~~Describe \code{roi.name} here~~
}
  \item{log.file}{
    It can be NULL or a path where to generate and refresh a txt file which logs the progress of the filtering procedure   
%%     ~~Describe \code{plot} here~~
}
}
\details{
This function allows to fit a curve and extract thresholds in a 
pixel-based analysis exactly as \code{greenProcess} 
does in a ROI-based analysis, except that uncertainty cannot be estimated 
(since it would be too computationally intense). This function takes as first argument 
a list as in output from spatialFilter. For each pixel 
in the ROI the function fits a curve (according to options specified in \code{fit}) and 
extracts thresholds (as defined in \code{threshold}). This function performs the same task 
that \code{greenProcess} does in a ROI-based analysis, except that uncertainty cannot be estimated 
(since it would be too computationally intense). For pixel-based analysis, it is recommended to use 
rather low resolution images or split your region of interest into multiple subROIs (function \code{splitROI}. 
A specific vignette for spatial analysis is stored as pdf in the package folder. The user is adviced to carefully read it before
starting a spatial analysis.   
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

