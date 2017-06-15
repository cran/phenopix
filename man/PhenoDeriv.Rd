\name{PhenoDeriv}
\alias{PhenoDeriv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to extract thresholds
%%  ~~function to do ... ~~
}
\description{
See PhenoDeriv from package greenbrown
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PhenoDeriv(x, formula = NULL, uncert = FALSE, 
	params = NULL, breaks, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A named vector with the parameters of the equation used to fit the data. 
}
  \item{formula}{
  Inherited from previous steps of the process.
}
  \item{uncert}{
Currently unused
}
  \item{params}{
  Inherited from previous steps of the process.  
}

  \item{breaks}{
Currently unused
}
  \item{\dots}{
  Further arguments, currently not used
}
}

\examples{
data(bartlett2009.fitted)
derivatives.phenophases <- PhenoDeriv(
	x=bartlett2009.fitted$fit$predicted, fit=bartlett2009.fitted$fit
	)
plot(bartlett2009.fitted$fit$predicted)
abline(v=derivatives.phenophases[c(1:2,4)], col=palette())
mtext(names(derivatives.phenophases[c(1:2,4)]), 
	at=derivatives.phenophases[c(1:2,4)], 
	line=-2, 
	col=palette()[1:3])
}
