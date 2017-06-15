\name{extractVIs}
\alias{extractVIs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracts various vegetation indexes in a ROI
%%  ~~function to do ... ~~
}
\description{
This function allows to extract various vegetation indexes in a 
region of interest of a jpeg image. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extractVIs(img.path, roi.path, vi.path = NULL, roi.name = NULL, 
  plot = TRUE, begin = NULL, spatial = FALSE, date.code, 
  npixels=1, file.type='.jpg', bind=FALSE, ncores='all', log.file=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{img.path}{
  Path to images
%%     ~~Describe \code{img.path} here~~
}
  \item{roi.path}{
  Path to stored RData with ROI, see \code{DrawROI}
%%     ~~Describe \code{roi.path} here~~
}
  \item{vi.path}{
  Where to save the output. If NULL data are saved in the working directory. 
%%     ~~Describe \code{vi.path} here~~
}
  \item{roi.name}{
  The name of the ROI
%%     ~~Describe \code{roi.name} here~~
}
  \item{plot}{
  Should a diagnostic plot with red, green and blue indexes be returned?
%%     ~~Describe \code{plot} here~~
}
  \item{begin}{
  The user can provide a beginning date as a character string in the form 'YYYY-MM-DD'. Images 
  with a time stamp earlier than the provided date will be discarded. Default is NULL and the whole
  time series will be processed. Useful when updating an existing time series.
%%     ~~Describe \code{plot} here~~
}
  \item{spatial}{
  If spatial = FALSE (the default) vegetation indexes are averaged over the entire ROI. If set to FALSE 
  red green and blue digital numbers are extracted for each pixel in the ROI. Since this option builds large 
  objects, it is recommended to use it on resized images, so to have to more than 10k pixels in the ROI.  
 %     ~~Describe \code{plot} here~~
}
  \item{date.code}{
  Provide the (quoted) exact format of the date embedded in your image names. Example: your image file picked on June 14th, 2012 at 12 pm is named 
  "site_12.06_14.1200.png", than your date.code will be "yy.mm_dd.HHMM". 
 %     ~~Describe \code{plot} here~~
}
  \item{npixels}{
  This argument allows to sample the images at lower resolution. Specify by an integer the number of pixels you want to aggregate (e.g. if npixels=2), a square of 2 pixels per side is aggregated in one pixel.  
}
  \item{file.type}{
  It must match the syntax of your file extension (e.g. .jpg, .JPG, .JPEG). Multiple types are allowed by concatenation with c().  
%%     ~~Describe \code{roi.names} here~~
}
  \item{bind}{
If TRUE and argument begin is defined, then new VI.data are binded to already existing data and overwritten, after checking for duplicated records 
(and returning a warning in case there are).  
}
  \item{ncores}{
Number of processors to be used in parallel computation, defaults to 'all' which will accidentally slow down any other activity on your computer. Otherwise set the number of processors you want to use in parallelization. 
%%     ~~Describe \code{roi.name} here~~
}
 \item{log.file}{
  It can be NULL or a path where to generate and refresh a txt file which logs the progress of the filtering procedure  
}

}
\details{
In the pixels contained in a ROI (which are loaded from the given folder), 
various vegetation indexes are calculated. Raw red, green and blue dn are averaged 
over the roi. The standard deviation is also calculated. For each pixel the 
brightness is computed as the sum of r + g + b. Then, for each pixel red, green 
and blue chromatic coordinates (aka relative indices) are calculated as 
r/brightness. After the calculation, all pixel data are averaged over the ROI. 
ExtractDateFilename is then called to get a time vector and a doy. The function 
applies recursively for all images in a folder and allows to get a multivariate 
time series of the various indexes. 
A plot is also generated and saved as .png if plot=TRUE
Option \code{begin} allows to update the process from a given date without reprocessing
the whole time series of images. In this case a separate Rdata will be saved (and not overwritten
to an already existing one) with the date of begin in the filename. 
Option \code{spatial} allows to compute digital numbers for each pixel instead of averaging them 
over the entire ROI.    
Note: Brightness and relative color indices are calculated for each pixel and THEN 
averaged over the ROI. This is different from averaging the brightness and raw colors 
over the entire ROI and THEN calculate a ROI-based relative color index. This second 
approach is used within the function \code{autoFilter} in the case when ROI aggregated 
raw dn are provided and brightness is calculated from them. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list with one data.frame for each ROI containing the multivariate time series 
of various vegetation indices if \code{spatial} = FALSE. Else, a list with one list for each ROI. Each 
sublist will be a named list that can be handled with appropriate functions, different from the one used 
if \code{spatial} = FALSE. 
}
\author{
Edoardo Cremonese <e.cremonese@arpa.vda.it>, Gianluca Filippa <gian.filippa@gmail.com>
}