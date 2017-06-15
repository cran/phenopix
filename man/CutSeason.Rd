\name{CutSeason}
\alias{CutSeason}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to cut time series by visual estimation 
}
\description{
This function allows to cut a yearly time series where multiple
cycles are apparent into separated time series. This is done by visual
estimation using \code{\link{locator}} function.  
}
\usage{
CutSeason(data, plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{
A data.frame, ideally a yearly time series of daily values. 
}
\item{plot}{
Should a plot be returned? If so different colors for different splits
are returned. 
}
}
\details{
This function allows to split a season of data according to visual
estimation. The required time series is plotted and a call to locator()
enables the user to click on the split. A list is returned with a number
of dataframes. See \code{\link{locator}} for details on how to switch off the
on-screen locator, which depends upon the graphic device. Note that all breaks must be provided, i.e. at the beginning and end of the subseasonal cycle(s).     
}
\value{
A list with n dataframes, where n is the number of splits
}
\author{
Gianluca Filippa <gian.filippa@gmail.com>
}
\seealso{
\code{\link{PasteSeason}}
}
