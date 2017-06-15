\name{getExposure}
\alias{getExposure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract exposure from stardot (or similar) images
}
\description{
This function allows to extract exposure from a set of binary images. It is currently optimized for stardot cameras.   
}
\usage{
getExposure(ipath, coords, train.data=FALSE, date.code, sample=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ipath}{
  The absolute path to a folder of JPEG binary images, as converted from RGB with the function \code{binaryConvert()}  
}
  \item{coords}{
  A named vector with 4 coordinates (x1, x2, y1, y2), as obtained from 
  \code{getCoords()}.  
}
  \item{train.data}{
  A named list with ten sample numbers and letter E, as obtained from \code{trainOCR()}.  
}
  \item{date.code}{
  As in function \code{extractDateFilename()} in this package, to convert image names in POSIX vector.  
}
  \item{sample}{
  If NULL (default) all images in the folder are processed, otherwise an integer specifying how many files must be processed. 
  This is useful if you want to check some extractions without processing the whole folder.   
}
}
\details{
This function performs a simplified OCR procedure to recognize numbers in a binary image. To do so, a full RGB image is first converted in binary (b/w) internally. Then the coordinates to crop the Exposure record are retrieved with \code{getCoords()} and, finally, the computation is done with \code{getExposure}. Before doing so you have to train the OCR with sample numbers. See \code{trainOCR()} for details. The procedure is based on simple matrix matching. The Exposure string is splitted into the digits that constitute it and then each digit is compared to the samples from the training. Finally, a dataframe is extracted with filenames in the first column, exposure in the second one and a POSIX vector in the third. 
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\seealso{
\code{\link{getCoords}}
}


