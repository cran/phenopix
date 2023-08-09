\name{DrawMULTIROI}
\alias{DrawMULTIROI}

\title{
Draw a Region of Interest
}
\description{
This function allows to draw one (or more) region(s) of interest (ROI) 
on a jpeg image. After \code{phenopix} version 2.4 this function allows 
to draw a region of interest potentially consisting of different polygons.  
}

\usage{
DrawMULTIROI(path_img_ref, path_ROIs, nroi = 1, 
	roi.names, file.type='.jpg')
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{path_img_ref}{
  Path in your folder for the reference image
%%     ~~Describe \code{path_img_ref} here~~
}
  \item{path_ROIs}{
  Path where to store image with ROI and ROI coordinates.
%%     ~~Describe \code{path_ROIs} here~~
}
  \item{nroi}{
The number of ROIs you want to draw. 
%%     ~~Describe \code{nroi} here~~
}
  \item{roi.names}{
  A character vector with ROI names.
%%     ~~Describe \code{roi.names} here~~
}
  \item{file.type}{
  It must match the syntax of your file extension (e.g. .jpg, .JPG, .JPEG). Multiple types are allowed by concatenation with c().  
%%     ~~Describe \code{roi.names} here~~
}
}
\details{
The function allows to draw one or more ROIs on an image or to load saved ROIs. 
The function uses \code{locator} to locate points, closes the polygon and stores an RData with coordinates. 
The use of locator is restricted to only some graphic devices. The function attempts to open an \code{X11()} device. 
In Mac OS the polygon is closed by typing ESC key. 
See \code{locator} for details. 
After package version 2.4, this function allows to build a ROI based on multiple polygons. As from this version 
of the package the way the roi.data object is structured changes substantially compared to previous versions. The old drawROI() 
function is no longer present. The new function \code{\link{convertROI}} allows to convert \code{roi.data} objects generated with 
former \code{DrawROI} function (phenopix < 2.4) into the new roi.data structure.  
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list containing the following: 
\item{mask}{A binary mask (a raster object)}
\item{polygons}{An object of class SpatialPolygons}
Additionally, a jpeg image is returned with the ROI(s) drawn. 
The same object that is returned is saved in the path specified in \code{path_ROIs}
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}



