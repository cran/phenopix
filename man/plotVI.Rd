\name{plotVI}
\alias{plotVI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot RGB DN, RGB indices and brightness
%%  ~~function to do ... ~~
}
\description{
This function allows to reproduce the same plot as returned from \code{extractVIs} 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotVI(VI.data, VI.path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{VI.data}{
  An object as in output from extractVIs (with spatial == FALSE)
%%     ~~Describe \code{img.path} here~~
}
  \item{VI.path}{
The path where plots will be saved
%%     ~~Describe \code{roi.path} here~~
}
}
\details{
Using in input a VI.data object as in output from extractVIs, this function returns a plot similar to the default plot of extractVIs. This function makes it easy to update VI plots after using extractVIs with option begin==TRUE. 

%%  ~~ If necessary, more details than the description above ~~
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}