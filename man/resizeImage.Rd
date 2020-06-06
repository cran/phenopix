\name{resizeImage}
\alias{resizeImage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Resize an image (and a tROI) to a given pixel resolution
%%  ~~function to do ... ~~
}
\description{
This function allows to resize a sample image and a correspondent ROI to a given pixel resolution to be used as background to spatila analysis plots. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
resizeImage(image.in, image.out, roi.in, roi.out, npixels)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image.in}{
  The complete path to your original image

}
  \item{image.out}{
The complete path (filename with extension included) where the new, resized image will be saved. %%     ~~Describe \code{path_ROIs} here~~
}
  \item{roi.in}{
  The complete path to your original roi.data
 }
  \item{roi.out}{
The complete path (filename with extension included) where the new, resized roi.data.RData will be saved
%%     ~~Describe \code{roi.names} here~~
}
  \item{npixels}{
  As in \code{extractVIs} to aggregate more than one pixel
  
%%     ~~Describe \code{roi.names} here~~
}
}
\details{
Coupled with spatial analysis and image resize (see \code{extractVIs()} and specifically argument \code{npixels} for details), this function allows to update a selected image and the correspondent ROI to a smaller size. This is exactly what is done internally when \code{extractVIs()} is called with npixels different from 1. The resized image can be used (together with the roi.data object) to plot results from spatially explicit phase extraction. See the vignette `spatial' for illustrated examples.  
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}



