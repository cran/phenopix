\name{phenopix-package}
\alias{phenopix-package}
\alias{phenopix}
\docType{package}
\title{
A package for the processing of images of a vegetation cover   
}
\description{
The package provides functions to process digital images, depict greennes index (and a number of other indices) 
trajectories and extract relevant phenological stages. 
}
\details{
\tabular{ll}{
Package: \tab phenopix\cr
Type: \tab Package\cr
Version: \tab 2.4.3\cr
Date: \tab 2023-08-09\cr
License: \tab GPL-2\cr
}

The package provides functions to process digital images, depict greennes index 
trajectories and extract relevant phenological stages. 


The first step of the work is to set a region of interest on the images. The function \code{DrawROI} does the work. 
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

The package offers a variety of fittings and phenophase methods so to be as flexible 
as possible in order to apply to very different boreal (sofar) ecosystems, ranging 
from high latitude/altitude grasslands to tropical forests. 
Ecosystems that show multiple seasonal peaks can be splitted in subseason with 
the \code{CutSeason} function and processed separately. 
Dedicated plotting functions provide an easy way to look at fitting and thresholding 
with annotated graphs. 
The package is being tested on the PHENOCAM dataset (http://phenocam.sr.unh.edu/webcam/), 
and constantly debugged. 

Detailed vignettes are available in my researchGate page, specifically a base vignette (https://www.researchgate.net/publication/289374477_phenopix_R_package_vignettes_13_base_vignette) and one dedicated to the spatial analysis (https://www.researchgate.net/publication/289355121_phenopix_R_package_vignettes_23_spatial_pixel-based_analysis_vignette). 
Here the user can find a walk-through to understand main functions, and procede step-by-step trough the ananlysis.

Recent changes:

As from version 2.4 the function drawROI was substituted by the function drawMULTIROI.
This implies few changes: the object roi.data is no longer structured as before. 
Now it is a list with an element for each ROI. For each element, there is an item 
named `mask' containing  the binary raster mask (dimensions and resolution as pixel 
size of the original image where ROI was drawn) and polygons (an object of class 
SpatialPolygons). The relevant changes in a processing chain that may occur due to 
these changes are partially accommodated with the new function \code{convertROI}. 


}

\author{
Gianluca Filippa, Edoardo Cremonese, Mirco Migliavacca, Jeroen Staab 

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

Migliavacca, M., Galvagno, M., Cremonese, E., Rossini, M., Meroni, M., Sonnentag, O., Cogliati, S., Manca, G., Diotri, F., Busetto, L., Cescatti, A., Colombo, R., Fava, F., Morra di Cella, U., Pari, E., Siniscalco, C., Richardson, A.D., 2011. Using digital repeat photography and eddy covariance data to model grassland phenology and photosynthetic CO2 uptake. Agric. Forest Meteorol. 151, 1325-1337.

Papale, D., Reichstein, M., Aubinet, M., Canfora, E., Bernhofer, C., Kutsch, W., Longdoz, B., Rambal, S., Valentini, R., Vesala, T., Yakir, D., 2006. Towards a standardized processing of Net Ecosystem Exchange measured with eddy covariance technique: algorithms and uncertainty estimation. Biogeosciences 3, 571-583.

Sonnentag, O., Hufkens, K., Teshera-Sterne, C., Young, A.M., Friedl, M., Braswell, B.H., Milliman, T., O'Keefe, J., Richardson, A.D., 2012. Digital repeat photography for phenological research in forest ecosystems. Agric. Forest Meteorol. 152, 159-177.

Zhang X, Friedl MA, Schaaf CB, Strahler AH, Hodges JCF, Gao F, Reed BC, Huete A (2003) 
Monitoring vegetation phenology using MODIS, Remote Sens. Environ., 84, 471-475.

}
\keyword{ package }
\seealso{
greenbrown.r-forge.r-project.org
}

