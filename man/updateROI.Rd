\name{updateROI}
\alias{updateROI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Update pixels included in a ROI of different pixel size
}
\description{
This function allows to apply an already drawn ROI to the same image but with different resolution.   
}
\usage{
updateROI(old.roi, new.img)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{old.roi}{
The roi object
}
  \item{new.img}{
An image array as read by \code{readJPEG} 
}
}
\details{
This function allows to extract pixel information in a ROI on images of different 
quality with respect to the one where the ROI was first drawn. The typical use of 
this function is to work on resized (smaller) images. Typically you draw a ROI on 
your best quality image (say 640 x 480 pixels) and process your images to extract 
vegetation indexes. But if you then want to use the function \code{spatialGreen} to 
perform a pixel based analysis you have to reduce image size because the analysis is 
computationally extremely intense. See \code{spatialGreen} for more details on 
computation time. Hence, you typically degradate your image (say to 320 x 240 pixels). 
The function applies ROI vertices to the new, resized image and returns pixels in the 
ROI of the new image size. 
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}


