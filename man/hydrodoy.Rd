\name{hydrodoy}
\alias{hydrodoy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts from and to hydrological day of year 
}
\description{
Converts from and to hydrological day of year
}
\usage{
hydrodoy(x, year, reverse=FALSE)
}

\arguments{
  \item{x}{
    A numeric vector of doys (if reverse = FALSE) or hydrodoys
}
  \item{year}{
  A character or numeric with a year  
}
  \item{reverse}{
  If FALSE (default) a doy vector is expected and an hydrodoy vector is returned, if TRUE, the other way around.
}
}
%- maybe also 'usage' for other objects documented here.
\details{
This function converts a vector of doys into hydrodoys, i.e. setting Oct 1st as doy 1. The argument year is used to understand if you are computing hydrodoys in a leap year or not. The back conversion is also possible by setting reverse = TRUE. 
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}
