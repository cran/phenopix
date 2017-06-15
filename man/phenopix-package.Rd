\name{phenopix-package}
\alias{phenopix-package}
\alias{phenopix}
\docType{package}
\title{
A package to process images of a vegetation cover   
}
\description{
The package provides functions to process digital images, depict greennes index 
trajectories and extract relevant phenological stages. 
}
\details{
\tabular{ll}{
Package: \tab phenopix\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-08-21\cr
License: \tab GPL-2\cr
}

The package provides functions to process digital images, depict greennes index 
trajectories and extract relevant phenological stages. The first step of the work 
is to set a region of interest on the images. The function \code{DrawROI} does the work. 
More than one ROI can be specified. 

Second step is extracting information from the ROI(s). The function \code{extractVIs}
computes several vegetation indexes on image pixels falling within the ROI(s). 
The function works recursively within a folder so that all images can be processed 
and a time series of the computed indexes is extracted. A specific function 
\code{extractDateFilename} retrives a timestamp from the filename of the images. 
Time series of green, red and blue chromatic coordinates and brightenss are returned 
for each ROI of the image. 

Third step is data filtering. The function \code{autoFilter} provides several filtering 
methods that can be used one at a time or in sequence.

Fourth step is fitting a curve to the data. Five methods have been included in the package. 
The function \code{SplineFit} fits a smoothed cubic spline to the data. The remaining four 
methods are based on the optimization of different double logistic equations retrieved 
from the recent literature. An uncertainty calculation is provided, based on the residuals between
observed and predicted values. The variability in the residuals is used to generate random noise, 
that is then added to raw data in a bootstrap procedure. From this procedure an ensemble of equation 
parameters and/or of curves are generated. 

Fifth step is the extraction of phenological thresholds. Five methods have been 
included to extract phenological dates. One is based on thresholds on the spline 
curve and one on breakpoints analysis, whereas other methods work on inflection points 
of the derivatives in various ways.  
Uncertainty on curve fitting is extended to threshold extraction, so that also 
for this step uncertainty is easily estimated.

The package offers a variety of fittings and thresholdings so to be as flexible 
as possible in order to apply to very different boreal (sofar) ecosystems, ranging 
from high latitude/altitude grasslands to tropical forests. 
Ecosystems that show multiple seasonal peaks can be splitted in subseason with 
the \code{CutSeason} function and processed separately. 
Dedicated plotting functions provide an easy way to look at fitting and thresholding 
with annotated graphs. 
The package is beeing tested on the PHENOCAM dataset (http://phenocam.sr.unh.edu/webcam/), 
and constantly debugged. 

In the example section the user can find a walk-through to understand main functions, 
from the filtering step forward. ROI selection and index calculations are excluded 
from the example because they require the use of locator from the user. 
}

\author{
Gianluca Filippa, Edoardo Cremonese, Mirco Migliavacca, ... 

Maintainer: Gianluca Filippa <gian.filippa@gmail.com>
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

Migliavacca et al 2011

Papale et al.

Sonnentag et al. 


Zhang X, Friedl MA, Schaaf CB, Strahler AH, Hodges JCF, Gao F, Reed BC, Huete A (2003) 
Monitoring vegetation phenology using MODIS, Remote Sens. Environ., 84, 471-475.

}
\keyword{ package }
\seealso{
greenbrown.r-forge.r-project.org
}

