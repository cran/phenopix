\name{editExposure}
\alias{editExposure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Change erroneous exposure values by hand  
}
\description{
 This function allows to fix wrong exposure values by hand. Use in combination with \code{\link{getExposure}}.  
 See \code{\link{getExposure}}
}
\usage{
editExposure(exposures, image.path, which)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{exposures}{
A \code{data.frame} with the exposures as in output from \code{\link{getExposure}}
}
  \item{image.path}{
  The path to the folder where the images are stored, from which exposures were extracted
}
  \item{which}{
A vector with row positions in your \code{data.frame} where exposures must be converted. 
}
}
\details{
After the extraction of exposure via the \code{\link{getExposure}} function, some of them will likely require manual correction. This function makes this process easy by plotting on screen the actual exposure and the estimated. If they don't agree you can manually correct it by typing into the R console.   
}

\value{
 The corrected exposures \code{data.frame}.
 }

\author{
Gianluca Filippa <gian.filippa@gmail.com>
}



