\name{combineUncertainty}
\alias{combineUncertainty}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An evolution of \code{\link{greenProcess}} for the combination of uncertainty after processing 
}
\description{
The \code{combineUncertainty} uses \code{\link{greenProcess}} to fit all available double logistic equations in the phenopix package and extracts thresholds with all available methods. Then uncertainties can be combined and returned by using \code{\link{summarizePhases}} and plotted with \code{\link{plotSum}}. 
See \code{\link{greenProcess}}. 
}
\usage{
combineUncertainty(ts, which='all', nrep=50, ncores='all')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{ts}{
A ts or zoo object with gcc data. index(ts) must be numeric days of
year (doys) or a POSIXct vector
}
\item{which}{
It can be 'all' (default) and all 4 double logistic fits will be calculated (beck, elmore, elosterman, gu), or a vector of subsets of the four fits
}
\item{nrep}{
Number of relications to estimate uncertainty for each single fitting, defaults to 50. 
}
\item{ncores}{
Number of processors to be used in parallel computation, defaults to 'all' which will accidentally slow down any other activity on your computer. Otherwise set the number of processors you want to use in parallelization. 
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

