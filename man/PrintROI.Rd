\name{PrintROI}
\alias{PrintROI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print an already drawn region of interest
%%  ~~function to do ... ~~
}
\description{
This function allows to re-draw one (or more) region(s) of interest (ROI) 
on a jpeg image. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PrintROI(path_img_ref,path_ROIs,which='all',col, file.type='.jpg')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path_img_ref}{
  Path in your folder for the reference image
%%     ~~Describe \code{path_img_ref} here~~
}
  \item{path_ROIs}{
  Path where ROI coordinates are stored in a structured list called ROI.Rdata.
%%     ~~Describe \code{path_ROIs} here~~
}
  \item{which}{
Which one of your ROIs you want to draw, defaults to `'all' and draws all rois.    
%%     ~~Describe \code{nroi} here~~
}
  \item{col}{
  A character vector with colors. If missing it defaults to color \code{palette}
%%     ~~Describe \code{roi.names} here~~
}
  \item{file.type}{
  It must match the syntax of your file extension (e.g. .jpg, .JPG, .JPEG). Multiple types are allowed by concatenation with c().  
%%     ~~Describe \code{roi.names} here~~
}

}
\details{
This utility allows to draw your ROIs on your reference image, with different colors for different ROIS and 
a dedicated legend
See \code{DrawROI} for details. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns a plot that can be saved with your favorite device.
}
\author{
Gianluca Filippa <g.filippa@arpa.vda.it>
}



