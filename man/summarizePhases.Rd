\name{summarizePhases}
\alias{summarizePhases}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An evolution of \code{\link{greenProcess}} for the combination of uncertainty after processing 
}
\description{
The \code{combineUncertainty} uses \code{\link{greenProcess}} to fit all available double logistic equations in the phenopix package and extracts thresholds with all available methods. Then uncertainties can be combined and returned by using \code{\link{summarizePhases}} and plotted with \code{\link{plotSum}}. 
See \code{\link{greenProcess}}. 
}
\usage{
summarizePhases(list, quantiles=c(0.25, 0.75), across.methods=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{list}{
An object in output from \code{combineUncertainty}
}
\item{quantiles}{
The envelope used for the uncertainty
}
\item{across.methods}{
If TRUE, gu and klosterman phenophase methods are combined in a single phenophase estimation, and the same happens for derivatives and trs. 
}
}
\details{
This function uses \code{\link{greenProcess}} to fit all available double logistic equations in the phenopix package and extracts thresholds with all available methods. Then uncertainties can be combined and returned by using \code{\link{summarizePhases}} and plotted with \code{\link{plotSum}}. 
See \code{\link{greenProcess}}, \code{\link{summarizePhases}}, \code{\link{plotSum}}. This function uses a modellistic approach to combine all uncertainties from all available phenopix fittings, as to get an ensemble of phases with different methods, without necessarily choosing any of them. 
}

\value{
A named list with dataframes for each phenophase method with all replication for each of the included fitting methods. These data can then be combined with the companion functions \code{\link{summarizePhases}} and \code{\link{plotSum}}. See examples for details. 
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
}

\examples{
\dontrun{
  require(zoo) 
  data(bartlett2009.filtered)
  combined.fit <- combineUncertainty(na.approx(filtered.tmp$max.filtered), nrep=100)
# 100 replications for each fitting
  names(combined.fit) # a dataframe for each phenoMethod + a list with all fittings
  fit.summary <- summarizePhases(combined.fit, across.methods=TRUE)
## again a list with one element for each fitting method + two additional items 
## if across.methods is TRUE, which combines gu + klosterman phenophase methods 
## in a single method, and the same happens for trs and derivatives
  plotSum(bartlett2009.filtered, fit.summary, which='klosterman')
## a plot with original timeseries + phenophases and their uncertainty
  }
  }

