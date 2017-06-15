\name{PhenoExtract}
\alias{PhenoExtract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function that extracts phenological thresholds
}
\description{
This function extracts phenological thresholds according to different methods. 
Methods include 'trs', 'derivatives', 'klosterman', 'gu'. See details for the 
computation of each method.  
}
\usage{
PhenoExtract(data, method = "trs", uncert = FALSE, 
  breaks = 3, envelope = "quantiles", 
  quantiles = c(0.1, 0.9), plot = TRUE, sf, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  A list structured as in output from the fitting procedures, such as GuFit, 
  KlostermanFit, ElmoreFit, BeckFit. 
}
  \item{method}{
  One between 'trs', 'derivatives', 'klosterman', 'gu'.
}
  \item{uncert}{
  Should uncertainty on thresholds be computed? It requires that uncertainty 
  be computed in the fitting function. I.e. The function requires the element 
  'uncertainty' in data beeing non NULL. If is.null(uncertainty) in the data or 
  this item is set to FALSE, uncertainty won't be computed.  
}
  \item{breaks}{
Currently unused%%     ~~Describe \code{breaks} here~~
}
  \item{envelope}{
  One between 'quantiles' and 'min-max'. If 'quantiles', the uncertainty envelope 
  will be computed as quantiles. Quantiles reported in \code{quantiles} will be 
  computed togheter with the median. If 'min-max' is choosen, min, max and mean of 
  the uncertainty envelope will be returned. 
}
  \item{quantiles}{
  Quantiles to be calculated if envelope='quantiles'. The notation is the same as 
  to specify quantiles in the \code{quantile} function, i.e. to get 10th and 90th 
  percentile, use c(0.1, 0.9).  
}
  \item{plot}{
  Should a diagnostic plot be returned with annotated thresholds? It calls the 
  function \code{PhenoPlot}
}
  \item{sf}{
Scaling factors required to normalize the data prior to the fitting. If the function is called by e.g. \code{\link{greenProcess}} sf is automatically calculated. We suggest using quantile(ts, probs=c(0.05, 0.5)) if you need to compute it. 
}
  \item{\dots}{
  For the plotting function, a number of parameters from generic \code{plot} 
  can be specified. See examples. 
  Additionally, a further option to 'trs' method calling \code{PhenoTrs} is provided: by setting a trs argument. 
  The default for trs is 0.5, meaning that the phases sos (start of season) and eos (end of season) will be set when gcc reaches 50\% of maximum on the increasing (sos) and decreasing (eos) seasonal trajectory.  
}
}
\details{
This is a wrapper function that calls \code{\link{PhenoTrs}} for method='trs', \code{\link{PhenoDeriv}} 
for method='derivatives', (from package \code{greenbrown}) or \code{\link{PhenoGu}} for method='Gu',
 and \code{\link{PhenoKl}} for method='klosterman' from this package. Please see help of the 
 single functions for details on the calculation of thresholds.  
}
\value{
If uncertainty=FALSE a vector of phenology metrics, otherwise a dataframe. 
}
\references{

Klosterman ST, Hufkens K, Gray JM, Melaas E, Sonnentag O, Lavine I, Mitchell L, 
Norman R, Friedl MA, Richardson A D (2014) Evaluating remote sensing of deciduous 
forest phenology at multiple spatial scales using PhenoCam imagery, Biogeosciences, 
11, 4305-4320, doi:10.5194/bg-11-4305-2014.

Gu L, Post WM, Baldocchi D, Black TA, Suyker AE, Verma SB, Vesala T, Wofsy SC. (2009) 
Characterizing the Seasonal Dynamics of Plant Community Photosynthesis Across a Range 
of Vegetation Types. In: Phenology of Ecosystem Processes (Ed: Noormets A, 
Springer New York), pp 35-58.

Zhang X, Friedl MA, Schaaf CB, Strahler AH, Hodges JCF, Gao F, Reed BC, Huete A (2003) 
Monitoring vegetation phenology using MODIS, Remote Sens. Environ., 84, 471-475.
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}

\seealso{
\code{\link{PhenoGu}},
\code{\link{PhenoKl}},
\code{\link{PhenoDeriv}},
\code{\link{PhenoTrs}},
\code{\link{PhenoPlot}},
\code{\link{PhenoGu}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

