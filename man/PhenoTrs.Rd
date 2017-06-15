\name{PhenoTrs}
\alias{PhenoTrs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A fitting function
%%  ~~function to do ... ~~
}
\description{
See PhenoTrs from package greenbrown for details. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PhenoTrs(x, approach = c("White", "Trs"), trs = 0.5, min.mean = 0.1, 
  formula = NULL, uncert = FALSE, params = NULL, breaks, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{seasonal cycle of one year}

  \item{approach}{approach to be used to calculate phenology metrics. 'White' (White et al. 1997) or 'Trs' for simple threshold.}

  \item{trs}{threshold to be used for approach "Trs"}

  \item{min.mean}{minimum mean annual value in order to calculate phenology metrics. Use this threshold to suppress the calculation of metrics in grid cells with low average values  }

  \item{formula}{Returned from previous steps of the process}

  \item{uncert}{Currently unused}

  \item{params}{Returned from previous steps of the process}

  \item{breaks}{Currently unused}
  
  \item{\dots}{further arguments (currently not used)}

}

\examples{
data(bartlett2009.fitted)
trs.phenophases <- PhenoTrs(
  x=bartlett2009.fitted$fit$predicted, 
  fit=bartlett2009.fitted$fit)
plot(bartlett2009.fitted$fit$predicted)
abline(v=trs.phenophases[c(1:2,4)], col=palette())
}

