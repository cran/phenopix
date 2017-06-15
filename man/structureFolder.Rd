\name{structureFolder}
\alias{structureFolder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fast structuration of a folder for phenopix process
}
\description{
This function allows to create subfolders to a folder optimized for the 
phenopix processing. If assigned to an object, all subfolders can be 
accessed in a simple and straightforward way.   
}
\usage{
structureFolder(path, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
The main path where the subdirectories will be created 
}
  \item{\dots}{
further arguments passed to \code{dir.create}
}
}
\details{
This function creates 4 subfolders to a given folder. In IMG you will store images to be processed. 
In REF you will put your reference image, i.e. the one that you will use to draw your ROI(s). 
In ROI the RData containing ROI data and one plot for each ROI will be saved. In folder VI
you can save your VI.data.Rdata after have run \code{extractVIs}.   
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}



