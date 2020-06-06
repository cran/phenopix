\name{convertROI}
\alias{convertROI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a region of interest object from the old structure to the new (phenopix > 2.4)
%%  ~~function to do ... ~~
}

\description{
This function allows to convert the \code{roi.data} stored  in the \code{roi.data.Rdata} object built with 
\code{phenopix} older than version 2.4 into the new structure better described in the details section. The 
changed was needed because a new method for drawing a region of interest was conceived, which includes the 
possibility of drawing multiple polygons for a single ROI. The new function that does the job is 
\code{\link{DrawMULTIROI}}, which fully substitutes the old \code{DrawROI}.  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
convertROI(path_img_ref, path_ROIs, file.type='.jpg')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_img_ref}{
  Path in your folder for the reference image
%%     ~~Describe \code{path_img_ref} here~~
}
  \item{path_ROIs}{
  Path where \code{roi.data} object created with \code{phenopix} < 2.4 was built.
%%     ~~Describe \code{path_ROIs} here~~
}
  \item{file.type}{
  It must match the syntax of your file extension (e.g. .jpg, .JPG, .JPEG). Multiple types are allowed by concatenation with c().  
%%     ~~Describe \code{roi.names} here~~
}
}
\details{
This function allows to convert the \code{roi.data} stored  in the \code{roi.data.Rdata} object built with 
phenopix older than version 2.4 into the new structure better described in the details section. The changed 
was needed because a new method for drawing a region of interest was conceived, which includes the possibility 
of drawing multiple polygons for a single ROI. There is a second reason why DrawROI had to disappear: it 
included C++ code from the package SDMTools which is currently orphaned.  The new function that allows multiple 
polygons in a single ROI is \code{DrawMULTIROI}, which fully substitutes the old \code{DrawROI}.
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



