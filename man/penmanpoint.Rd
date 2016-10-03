\encoding{UTF-8}
\name{penmanpoint}
\alias{penmanpoint}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Penman's potential evapotranspiration
}
\description{
Calculates Penman's potential evapotranspiration for a single point.
}
\usage{
penmanpoint(latitude, elevation, J,
            Tmin, Tmax, RHmin, RHmax, R_s,  u,
            z=2.0, z0 = 0.001, alpha = 0.08, windfun="1956")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{latitude}{Latitude in degrees.}
  \item{elevation}{Elevation (in m).}
  \item{J}{An integer vector with Julian days.}
  \item{Tmax}{Numeric vector with maximum temperature (degrees Celsius).}
  \item{Tmin}{Numeric vector with minimum temperature (degrees Celsius).}
  \item{RHmin}{Numeric vector with minimum relative humidity (percent).}
  \item{RHmax}{Numeric vector with maximum relative humidity (percent).}
  \item{R_s}{Numeric vector with solar radiation (MJ/m2).}
  \item{u}{Numeric vector with wind speed (m/s).}
  \item{z}{Wind measuring height (m).}
  \item{z0}{Roughness height (m).}
  \item{alpha}{Albedo.}
  \item{windfun}{Wind speed function version, either "1948" or "1956".}
}
\details{
The code  was taken from package `Evapotranspiration', which follows McMahon et al. (2013). If wind speed is not available, an alternative formulation for potential evapotranspiration is used as an approximation (Valiantzas 2006)
}
\references{
Penman, H. L. 1948. Natural evaporation from open water, bare soil and grass. Proceedings of the Royal Society of London. Series A. Mathematical and Physical Sciences, 193, 120-145.

Penman, H. L. 1956. Evaporation: An introductory survey. Netherlands Journal of Agricultural Science, 4, 9-29.

McMahon, T.A., Peel, M.C., Lowe, L., Srikanthan, R., McVicar, T.R. 2013. Estimating actual, potential, reference crop and pan evaporation using standard meteorological data: a pragmatic synthesis. Hydrology \& Earth System Sciences 17, 1331–1363. doi:10.5194/hess-17-1331-2013.
}
\value{
A numeric vector with potential evapotranspiration (in mm of water).
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{\code{\link{interpolationpoints}}}
