\name{PhenoGu}
\alias{PhenoGu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to extract phenological thresholds according to Gu et al. 2009
}
\description{
A function to extract phenological thresholds according to Gu et al. 2009. 
This is a rather internal function. Use PhenoExtract with method='gu' instead. 
}
\usage{
PhenoGu(x, fit, uncert = FALSE, breaks, sf, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A named vector with the parameters of the equation used to fit the data. 
}
  \item{fit}{
  A list structured as in output from the fitting procedures, such as GuFit, 
  KlostermanFit, ElmoreFit, BeckFit.
}
  \item{uncert}{
Currently unused
}
  \item{breaks}{
Currently unused
}
  \item{sf}{
Scaling factors required to normalize the data prior to the fitting. If the function is called by e.g. \code{\link{greenProcess}} sf is automatically calculated.
}
  \item{\dots}{
  Further arguments, currently not used
}
}
\details{
Threshold extraction is performed according to Gu et al (2009). Briefly, from 
the fitting equation (be it from Klosterman, Elmore, Beck or Gu fitting) and 
the correspondent parameters, the first derivative is extracted. Peak recovery 
rate (prr) is defined as the maximum of the first derivative and represent the 
maximum slope of the increasing part of the curve. Correspondingly peak 
senescence rate (psr) is the minimum of the first derivative. The recovery line 
and the senscence line are tangent to the fitting curve with slope equal to prr 
and psr respectively. Baseline and maxline are further defined as the horizontal 
lines corresponding to the minimum and the maximum of the curve. The intersection 
between recovery line and baseline defines the beginning of the growing season, 
i.e upturn date (UD). The intersection between recovery line and maxline defines 
the reacing of the plateau, i.e. stabilization date (SD). The intersection between 
senescence line and baseline defines the end of the growing season, i.e. recession 
date (RD). In the original implementation by Gu et al. the intersection between 
senescence line and maxline would define the downturn date (oldDD). To account 
for decreasing plateau typical of a number of seasonal trajectories, we have 
further defined a plateau line, which is a linear fit between SD and oldDD. 
The plateau line would roughly correspond to the maxline if the plateau is 
horizontal, whereas it will be a decreasing line in case of a decreasing plateau. 
Therefore, in our implementatio DD is defined as the intersection between the 
plateau line and the senescence line.     
}
\value{
A named vector of length 9 with upturn date (UD), stabilizazion date (SD), 
downturn date (DD), recession date (RD), maximum of the fitting curve (maxline),
 minimum of the fitting curve (baseline), peack recovery rate (prr), 
 peack senescence rate (psr) and the slope of the plateau line (plateau.slope).  
}
\references{
Gu L, Post WM, Baldocchi D, Black TA, Suyker AE, Verma SB, Vesala T, 
Wofsy SC. (2009) Characterizing the Seasonal Dynamics of Plant Community 
Photosynthesis Across a Range of Vegetation Types. In: Phenology of 
Ecosystem Processes (Ed: Noormets A, Springer New York), pp 35-58.
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}
\note{
Since this threshold extraction is based on a derivable function, it cannot 
be performed on raw data. Uncertainty estimation with this method on a fitted 
curve from SplineFit is currently not implemented. Instead you can use 
PhenoGu in a for loop cycling in the uncertainty dataframe columns. 
}

\seealso{
\code{\link{PhenoExtract}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
data(bartlett2009.fitted)
gu.phenophases <- PhenoGu(x=bartlett2009.fitted$fit$params, 
	fit=bartlett2009.fitted$fit, sf=quantile(bartlett2009.fitted$fit$predicted, c(0.1, 0.9)))
plot(bartlett2009.fitted$fit$predicted)
abline(v=gu.phenophases[1:4], col=palette())
mtext(names(gu.phenophases[1:4]), at=gu.phenophases[1:4], 
	line=-2:-5, col=palette()[1:4])
}


