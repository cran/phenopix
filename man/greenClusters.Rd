\name{greenClusters}
\alias{greenClusters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to perform a cluster analysis on data in output from pixel-based phenology
}
\description{
A function to perform a cluster analysis using function \code{kmeans} on data in output from pixel-based phenology
}
\usage{
greenClusters(data.clusters, data.curve=NULL, nclusters, formula=NULL, 
	plot=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.clusters}{
A numeric data.frame on which the k-means clustering has to be run. Each row stores a various number of phenophases for 
a given pixel.       
}
  \item{data.curve}{
A numeric data.frame with nrows equal to nrows of data.clusters and ncol equal to the number of parameters in the eaquation in formula. 
Each row contains the set of parameters to fit the equation in formula, for a given pixel. Or NULL in case of processing with method 'spline'
}
  \item{nclusters}{
The number of clusters, as in \code{centers} in function \code{kmeans}. 
}
  \item{formula}{
An expression of the equation used to fit the data in your spatial analysis. It can be retrieved from the object in output from greenProcess() function. Or NULL in case of processing with method 'spline'
}
  \item{plot}{
If true a plot with the average trajectories for each cluster is returned. Avaiable only with fitted curves
}
}
\details{
This function allows to perform a k-means clustering based on an input data.frame containing one or more phenophases extracted from a pixel-based analysis (in the columns) for each pixel analysed by the spatial analysis (each row must have phenophases for each pixel). A number of clusters is identified and each pixel is assigned to one cluster. An approximate average curve for each of the identified clusters is also built by averaging all curve parameters belonging to the same cluster. 
}
\value{
	A named list with components curves, napos and clusters. 
	"curves" contains a zoo object with the average curves, one curve for each of the identified clusters, or NA for spline fitting. 
	"napos" is a vector with na positions, i.e. pixels where the phenophases in the input matrix are missing. 
	"clusters" is a numeric vector of length equal to the number of pixels indicating to which cluster each pixel belongs.  
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}



