\name{getCoords}
\alias{getCoords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract given coordinates (in pixels) from a given JPEG image
}
\description{
This function allows to extract given coordinates (in pixels) from a given JPEG image   
}
\usage{
getCoords(image)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image}{
  The absolute path to the JPEG image you want to use.  
}
}
\details{
This function will first plot the JPEG image on screen and call \code{locator()}. You then have to click first on bottomright corner of the rectangle you want to extract, and then close the polygon (see ?locator() for details). A second plot with the cropped image is then printed on screen, where you have to click on topleft and bottomright corner around the string "Exposure: xxx", where xxx is the value of exposure. Be also sure to make your rectangle large enough to include possibly four digits exposure values (keep the right margin larger than the actual number). Coords will be returned in x and y pixel positions counted from the topleft corner of the image. These coords will be used to crop the image to extract exactly the Exposure string within the function \code{getExposure()}. Based on the results of \code{getExposure()} run on the images, you will evaluate whether to adjust the coordinates and run getExposure again, based on how well exposure was recognised from the OCR procedure.   
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\seealso{
\code{\link{getExposure}}
}


