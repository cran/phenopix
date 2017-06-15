\name{NDVI}
\alias{NDVI}

\title{
Compute NDVI
%%  ~~function to do ... ~~
}
\description{
This function computes camera NDVI as in Petach et al. (2014)  
}
\usage{
NDVI(exposure.matched, RGB.VI, IR.VI, spatial=FALSE)
  }

\arguments{
  \item{exposure.matched}{
  A dataframe as in output from \code{matchExposure()} 
}
  \item{RGB.VI}{
  A dataframe for RGB VI extracted as in output from \code{extractVIs()}
}
  \item{IR.VI}{
  A dataframe for IR VI extracted as in output from \code{extractVIs()}
}
  \item{spatial}{
  Set this flag to TRUE if you have extracted VIs with spatial == TRUE. 
}
}
\details{
Compute NDVI as in Petach et al. (2014)
}
\value{
A data.frame with NDVI computed after Petach et al. (2014).  
}

\seealso{
\code{\link{getExposure}}
\code{\link{NDVI}}
}

\author{
Jeroen Staab <jeroen.staab@posteo.de>, Gianluca Filippa <gian.filippa@gmail.com>
}