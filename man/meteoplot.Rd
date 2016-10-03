\encoding{UTF-8}
\name{meteoplot}
\alias{meteoplot}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots meteorological series
}
\description{
Simple plotting of meteorological series from an object of class \code{\link{SpatialPointsMeteorology-class}} or \code{\link{SpatialGridMeteorology-class}}.
}
\usage{
meteoplot(object, index, variable="MeanTemperature", add = FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class \code{\link{SpatialPointsMeteorology-class}} or \code{\link{SpatialGridMeteorology-class}}.}
  \item{index}{An integer to indicate the point in a \code{\link{SpatialPointsMeteorology-class}} object or the grid cell in a \code{\link{SpatialGridMeteorology-class}} object.}
  \item{variable}{The meteorological variable to be plotted.}
  \item{add}{A flag to indicate wether drawing should be done on the current plot (using function \code{lines}).}
  \item{...}{Additional parameters for functions \code{plot} or \code{lines}.}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
