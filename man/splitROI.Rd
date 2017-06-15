\name{splitROI}
\alias{splitROI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Splits a ROI into multiple subrois for spatial analysis
}
\description{
This function allows to split a roi into different subrois for a pixel-based analysis.   
}
\usage{
splitROI(roi.data, nsplit, names=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{roi.data}{
An object with roi data as in output from DrawROI() function 
}
  \item{nsplit}{
The number of sub-rois in which your ROI must be divided into
}
  \item{names}{
Optional names for roi. If omitted rois will be progressively numbered (i.e, roi1, roi2, roi3, etc.) 
}
}
\details{
This function allows to split a large ROI into smaller ones. This is needed to make computationally 
feasible the pixel-based analysis with large ROIs and/or high resolution images. As a general indication, 
approx 10000 pixels per sub-ROI is the upper limit for a computationally feasible spatial analysis, 
so set nsplit argument accordingly. The number of pixels in a ROI essentially controls the dimension of the VI.data
object you generate with \code{extractVIs} function. A large VI.data object (typically larger than 200 Mb) will fast 
saturate your RAM and slow down the spatial analysis.
}
\value{
A names list of length = nsplit and where each element has the same structure as a roi.data extracted by \code{DrawROI}. 
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}



