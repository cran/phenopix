\name{trainOCR}
\alias{trainOCR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract a training dataset for OCR procedure
}
\description{
This function allows to extract a training dataset for OCR procedure performed by \code{getExposure}. It is currently optimized for stardot cameras.   
}
\usage{
trainOCR(image.path, nsamples=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{image.path}{
  The absolute path to a folder of JPEG binary images, as converted from RGB with the function \code{binaryConvert()}  
}
  \item{nsamples}{
The maximum number of sampled images to be used. No need to change it.
}
}

\details{
This function allows to prepare a training dataset that will be used in function \code{getExposure()}. You need to identify 0-9 numbers and the capital letter E (Exposure), which are then used in \code{getExposure()}. 
The procedure makes use of locator to subsequently crop your image, so make sure you know how this function works in your OS. When you run the function a first image pops up on your graphic device. You have to click with the mouse on topleft and bottom right of the rectangle you want to crop. I suggest to crop to the entire string of text with all picture information, so to have the largest sample of numbers in it. When you close locator (right-click in Linux-OS, but likely also GUI-dependent), the cropped image will show up providing a zoom to the selection. The title in the plot helps you to remember which numbers you still have to define. Choose a number (the order you choose numbers does not matter) and make a second crop around it (always topleft, bottomright). Close locator. A third zoom will show up, gridded pixel by pixel. Again, crop your number topleft bottomright with a rectangle that exactly includes all pixels of your number. Close locator. In R command line you will be asked to type the number you have just drawn, or letter E. Type the number and press Enter. You will be prompted to a new image where you follow the procedure again to identify other numbers. When you will ne done with all numbers and E letter, you will get a named list with 11 elements. Each element will be a binary matrix for each of your numbers, and letter E.   
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}

\seealso{
\code{\link{getCoords}}
}


