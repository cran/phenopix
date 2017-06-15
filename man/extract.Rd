\name{extract}
\alias{extract}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to extract items from an object of class phenopix  
}
\description{
 This function allows to extract items from an object of class phenopix. 
}
\usage{
extract(x, what)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class \code{phenopix}
} 
  \item{what}{
 One between "data", "fitted", "metrics", "metrics.uncert", "curve.params", 
 "curve.uncert", "curve.params.uncert"
}
}
\details{
 This function allows to extract items from an object of class phenopix. 
 "data" extracts row data
 "fitted" extracts fitted data
 "metrics" extracts metrics data.frame
 "metrics.uncert" extracts metrics uncertainty dataframe 
 "curve.params" extracts curve parameters 
 "curve.uncert" extracts a family of fitted curves estimated by the uncertainty procedure
 "curve.params.uncert" extracts a family of curve parameters estimated by the uncertainty procedure 
}

\value{
A dataframe
}

\author{
Gianluca Filippa <gian.filippa@gmail.com>
}
