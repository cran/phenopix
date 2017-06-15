\name{extractParameters}
\alias{extractParameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract fitting parameters from a pixel-based analysis 
}
\description{
This function allows to extract fitting parameters from pixel-based analysis of a 
seasonal ensemble of images in one or multiple rois.   
}
\usage{
extractParameters(list, update=NULL, ...) 
}
\arguments{
  \item{list}{
  A list as in output from \code{spatialGreen}.  
}
  \item{update}{
  One between 'trs', 'derivatives', 'klosterman', 'gu' or NULL (the default). 
  See \code{update} from this package for further details. 
}

  \item{\dots}{
Further arguments to the update function. Currently, you can specify a \code{trs} argument for update method \code{trs} for specific thresholds. See \code{PhenoExtract} for further details.  
}

}
\details{
	This function allows to extract curve parameters and thresholds from pixel based analyisis. Depending on how you have 
  run \code{spatialGreen} function, \code{extractParameters} behaves differently (but returns the same results!). In case you haven't 
  saved each pixel's fit with \code{spatialGreen} (i.e., with \code{save} == FALSE and \code{assign} == TRUE), this function will
  extract parameters from a list as in output from \code{spatialGreen}. IN case you HAVE saved each pixel's fit (i.e. \code{save} == TRUE in function \code{spatialGreen}) \code{extractParameters} loads recursively in a folder (\code{path}) and builds a data.frame 
  with extracted curve parameters and thresholds. 
  In both cases a \code{data.frame} is returned, with all curve parameters and thresholds for each pixel (one row for each pixel). Additionally, RMSE (root mean square error) for each fitting is computed and returned as well. The dataframe in output is suitable 
  to be passed to the function \code{plotSpatial}.  
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
%%  ~~who you are~~
}



