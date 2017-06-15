\name{greenProcess}
\alias{greenProcess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to fit a curve and extract thresholds from Phenological Data
}
\description{
This is a wrapper function that calls fitting functions and threshold functions 
and returns an object of class \code{phenopix}. 
}
\usage{
greenProcess(ts, fit, threshold=NULL, plot=TRUE, which='light', 
  uncert=FALSE, nrep=100, 
  envelope='quantiles', quantiles=c(0.1, 0.9), hydro=FALSE, 
  sf=quantile(ts, na.rm=TRUE, prob=c(0.05, 0.95)), ncores='all', ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{
  A vector or, better, an univariate ts or zoo object. 
}
  \item{fit}{
  A character vector of length 1. Available options are: \code{spline}, \code{beck}, \code{elmore},
  \code{klosterman}, \code{gu}. 
}
  \item{threshold}{
A character vector of length 1. Available options are: \code{trs}, \code{derivatives}, 
\code{klosterman}, \code{gu}.  
}
  \item{plot}{
If TRUE a call to \code{PhenoPlot} returns fitted values and thresholds. 
}

  \item{which}{
Only relevant if fit == \code{klosterman}, available options are \code{light} or \code{heavy}. 
See \code{FitDoubleLogKlHeavy} and \code{FitDoubleLogKlLight} for details. 
}

 \item{uncert}{
Should uncertainty be estimated?
}

 \item{nrep}{
Number of replications for the uncertainty estimation. 
}

 \item{envelope}{
  One between \code{quantiles} and \code{min-max}. If \code{quantiles}, the uncertainty envelope 
  will be computed as quantiles. Quantiles reported in \code{quantiles} will be 
  computed togheter with the median. If \code{min-max} is choosen, min, max and mean of 
  the uncertainty envelope will be returned. }
 
  \item{quantiles}{
  Quantiles to be calculated if envelope='quantiles'. The notation is the same as 
  to specify quantiles in the \code{quantile} function, i.e. to get 10th and 90th 
  percentile, use c(0.1, 0.9).  
}

  \item{hydro}{
Hydro determines how days of the year are computed. If hydro = FALSE (default) January 1st is DOY 1. 
If hydro = TRUE October 1st is day 1. This option has been introduced for two purposes. 
First, for water limited or high temperature limited boreal ecosystems (with summer 
dormant season) to process a seasonal trajectory with winter peak. Second, for ecosystems 
in the australian emisphere. If hydro = TRUE all metrics concerned with a day of year must be back 
converted in order to get actual day of year. Conversions and back conversions can be performed 
with the function \code{doy2hydrodoy}.    
}
  \item{sf}{
Scaling factors used instead of min and max for data normalization.
}
  \item{ncores}{
Number of processors to be used in parallel computation, defaults to 'all' which will accidentally slow down any other activity on your computer. Otherwise set the number of processors you want to use in parallelization. 
%%     ~~Describe \code{roi.name} here~~
}
  \item{\dots}{
  For the plotting function, a number of parameters from generic \code{plot} 
  can be specified. See examples. 
}
}
\details{
This function is a wrapper function that allows to fit a curve to a yearly trajectories of greeneess 
and extract phenological thresholds according to a given criterion. Handeling this main function may 
allow the user to forget learning other, rather internal functions. The combination of \code{greenProcess} 
and \code{extract} allows to use main capabilities of this package. Virtually all other functions included
are called at some points within this function. The object of class \code{phenopix} which is created is a rather 
complex list that can be explored via the \code{extract} function. Check it out for further details.    
}
\value{
  An object of class \code{phenopix} with dedicated functions: plot(), print(), summary() and fitted(). 
The structure is actually a list. 
}
\references{
Gu L, Post WM, Baldocchi D, Black TA, Suyker AE, Verma SB, Vesala T, 
Wofsy SC. (2009) Characterizing the Seasonal Dynamics of Plant Community 
Photosynthesis Across a Range of Vegetation Types. In: Phenology of Ecosystem 
Processes (Ed: Noormets A, Springer New York), pp 35-58. 

Klosterman ST, Hufkens K, Gray JM, Melaas E, Sonnentag O, Lavine I, Mitchell L, 
Norman R, Friedl MA, Richardson A D (2014) Evaluating remote sensing of deciduous 
forest phenology at multiple spatial scales using PhenoCam imagery, Biogeosciences, 
11, 4305-4320, doi:10.5194/bg-11-4305-2014.

Zhang X, Friedl MA, Schaaf CB, Strahler AH, Hodges JCF, Gao F, Reed BC, Huete A (2003) 
Monitoring vegetation phenology using MODIS, Remote Sens. Environ., 84, 471-475.

%% ~put references to the literature/web site here ~
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\examples{
\dontrun{
data(bartlett2009.filtered)
fitted <- greenProcess(bartlett2009.filtered, 'klosterman', 
  'klosterman', uncert=TRUE, nrep=5, ncores=2)
  }
}

