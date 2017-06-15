\name{DrawROI}
\alias{DrawROI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw Region of Interest
%%  ~~function to do ... ~~
}
\description{
This function allows to draw one (or more) region(s) of interest (ROI) 
on a jpeg image. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DrawROI(path_img_ref, path_ROIs, nroi = 1, 
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
The function uses \code{\link{locator}} to locate points, closes the polygon and stores an RData with coordinates. 
The use of locator is restricted to only some graphic devices. Tne function attempts to open an X11 device. 
In Mac OS the poligon is closed by typing ESC key. 
See \code{\link{locator}} for details. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list containing the following: 
\item{pixels.in.roi}{Data for pixels contained in the ROI}
\item{vertices}{Vertices of the ROI}
Additionally, a jpeg image is returned with the ROI(s) drawn. 
The same object that is returned is saved in the path specified in \code{path_ROIs}
}
\author{
Edoardo Cremonese <e.cremonese@arpa.vda.it>
}



