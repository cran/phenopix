\name{extractDateFilename}
\alias{extractDateFilename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estract dates from filenames 
%%  ~~function to do ... ~~
}
\description{
This function extracts dates from filenames. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extractDateFilename(filename, date.code)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
  The filename where to retrieve time stamp 
%%     ~~Describe \code{path_img} here~~
}
  \item{date.code}{
  The format of your date in filename, see details.  
%%     ~~Describe \code{path_img} here~~
}
}
\details{
This function allows the extraction of the date (hour, doy, dayfract) 
from the filename. The only mandatory rules are (1) that site name come 
first and date after and (2) sitename and date must be separated by an underscore. 
In date.code provide the format of your date, using lower letters for year (y) 
month (m) and day (d) and upper letters for hour (H) and minute (M). As an example: 
If your file is named: 'sitename_2012_03_03_15-30.jpg' than your 
date.code is "yyyy_mm_dd_HH-MM".
If your file is named 'sitename_12.03.03.1530.jpg' than your date.code is 
"yy.mm.dd.HHMM"
If hours and minutes are missing in your filename, convertion defaults to 12:00. 
}
\value{
A POSIX string containing date,Hour,DOY,DOY.dayfract of the entire images time series
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Edoardo Cremonese <e.cremonese@arpa.vda.it>
}

