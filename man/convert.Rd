\name{convert}
\alias{convert}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to convert in data.frame a \code{zoo} or \code{ts} objct  
}
\description{
This function converts an object of class \code{ts} or \code{zoo} in a dataframe with a column 
named \code{time} retrived from index(x). It is designed for those unfamiliar with time series
objects. 
}
\usage{
convert(x, year=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{ts} or \code{zoo}
}
  \item{year}{
A vector of length one with year of observation. If provided, a column with time in POSIX format is also returned
}
} 
\details{
An object of class \code{data.frame} is returned with a column \code{doy} and optionally \code{time} at the end of the table.  
}

\value{
A dataframe
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
}
