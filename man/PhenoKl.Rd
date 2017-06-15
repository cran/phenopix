\name{PhenoKl}
\alias{PhenoKl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to extract phenological thresholds according to Klosterman et al. 2014
}
\description{
A function to extract phenological thresholds according to Klosterman et al. 2014. 
This is a rather internal function. Use PhenoExtract with method='klosterman' instead. 
}
\usage{
PhenoKl(x, uncert = FALSE, fit, breaks, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A named vector with the parameters of the equation used to fit the data. 
}
  \item{uncert}{
Currently unused
}
  \item{fit}{
 A list structured as in output from the fitting procedures, such as GuFit, 
 KlostermanFit, ElmoreFit, BeckFit.
}
  \item{breaks}{
Currently unused
}
  \item{\dots}{
  Further arguments, currently not used.
}
}
\details{
Threshold extraction is performed according to Klosterman et al (2014) with a 
modification derived from Zhang et al (2003). Briefly, the rate of curvature 
(k) as defined in Klosterman et al (2014) is computed and inflection points 
are evaluated on its derivative (derK). The growing season is splitted in 
its increasing and decreasing parts around the maximum. The same happens to 
derK. Greenup date is defined as the day of maximum derK (a local maximum) 
before the first minimum in derK in the increasing part of the curve. 
Maturity is defined as the maximum in derK between the minimum of derK 
and mid season. Senescence is defined as the first local minimum in the 
decreasing part of derK. Dormancy is defined as the last local minimum 
in derK. Phases are named after Zhang et al (2003).     
}
\value{
A named vector of length 4 with the extracted thresholds: Greenup, 
Maturity, Senescence, Dormancy.  
}
\references{
Klosterman ST, Hufkens K, Gray JM, Melaas E, Sonnentag O, Lavine I, 
Mitchell L, Norman R, Friedl MA, Richardson A D (2014) Evaluating 
remote sensing of deciduous forest phenology at multiple spatial 
scales using PhenoCam imagery, Biogeosciences, 11, 4305-4320, 
doi:10.5194/bg-11-4305-2014.

Zhang X, Friedl MA, Schaaf CB, Strahler AH, Hodges JCF, Gao F, Reed BC, 
Huete A (2003) Monitoring vegetation phenology using MODIS, Remote 
Sens. Environ., 84, 471-475.
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}
\note{
Since this threshold extraction is based on a derivable function, 
it cannot be performed on raw data. 
Uncertainty estimation with this method on a fitted curve from 
SplineFit is currently not implemented. Instead you can use PhenoKl 
in a for loop cycling in  the uncertainty dataframe columns. 
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{PhenoExtract}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
data(bartlett2009.fitted)
klosterman.phenophases <- PhenoKl(
	x=bartlett2009.fitted$fit$params, 
	fit=bartlett2009.fitted$fit)
plot(bartlett2009.fitted$fit$predicted)
abline(v=klosterman.phenophases[1:4], col=palette())
mtext(names(klosterman.phenophases[1:4]), 
	at=klosterman.phenophases[1:4], line=-2:-5, 
	col=palette()[1:4])
}

