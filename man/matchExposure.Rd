\name{matchExposure}
\alias{matchExposure}

\title{
Match exposures retrieved from the header of (stardot) digital images
%%  ~~function to do ... ~~
}
\description{
This function allows to match exposure values extracted from RGB and IR images based on image timestamp  
}
\usage{
matchExposure(exposure.df=NULL, 
  exposure.RGB=NULL, exposure.IR=NULL, pattern.RGB="RGB", 
  pattern.IR="IR", tol=1, 
  matching.column='timestamp')
  }
\arguments{
  \item{exposure.df}{
  A dataframe where exposures from RGB and IR images are in the same data.frame. It is however suggested to provide separate data.frames for RGB and IR images. The format of the data.frame must be as in ooutput from the \code{getExposure()} function. 
}
  \item{exposure.RGB}{
When \code{exposure.df} is NULL this data.frame, as in output from the function \code{getExposure()}, will be used to retrieve RGB exposures 
}
  \item{exposure.IR}{
When \code{exposure.df} is NULL this data.frame, as in output from the function \code{getExposure()}, will be used to retrieve IR exposures 
}
  \item{pattern.RGB}{
When \code{exposure.df} is non NULL the pattern to be matched in order to subset RGB image rows. The pattern is matched by a call to \code{grepl()} 
}
  \item{pattern.IR}{
When \code{exposure.df} is non NULL the pattern to be matched in order to subset IR image rows. The pattern is matched by a call to \code{grepl()} 
}
  \item{tol}{
  A tolerance value to be used to match timestamps of RGB and IR images. Available values include 1 (the default; perfect matching, i.e. the RGB image and the associated IR image has the same timestamp in the filename, rounded to minutes), 10, which means that images recorded within 10 minutes between each other are considered as matched. Higher values are allowed but it is suggested to avoid them 
}
  \item{matching.column}{
  A character giving the name of the column that must be used for the matching. 
}
}
\details{
This function is designed to receive in input two data.frames as in output from \code{getExposure()}, one with RGB exposures and one with IR exposures. Alternatively, one can provide a single data.frame with both RGB and IR exposures, and specify patterns of the image name (column 'images' as output from \code{getExposure()}) to match the two image types. Duplicates in image names are removed prior to the matching. 
}
\value{
A data.frame with expousre values for both RGB and IR images, the original timestamps and the rounded time.stamp. 
The format is suited to enter \code{NDVI()} function.  
}

\seealso{
\code{\link{getExposure}}
\code{\link{NDVI}}
}


\author{
Jeroen Staab <jeroen.staab@posteo.de>, Gianluca Filippa <gian.filippa@gmail.com>
}