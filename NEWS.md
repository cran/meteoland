-------------------------------
 NEWS for R Package "meteoland"
-------------------------------

# Version 0.8.3
- Remove dependencies of ncdf4.helpers

# Version 0.8.2
- Documentation converted to bookdown reference book.
- Bug correction in function 'doQmapDeque'

# Version 0.8.1
- Modification of 'radiation_directDiffuseInstant' and 'radiation_directDiffuseDay' to account for topographic effects
- New function 'summarypoint'

# Version 0.8.0
- New functions 'downloadSMCstationlist', 'downloadSMCcurrentday', 'downloadSMChistorical' and 'downloadSMCvarmetadata'.
- Update of vignette 'Meteorology'.

# Version 0.7.9
- New function reshapeworldmet to reshape data downloaded using package 'worldmet'
- New function reshapeweathercan to reshape data downloaded using package 'weathercan'
- Transform point coordinate system in 'interpolationpoints', 'interpolationpixels' and 'interpolationgrid' if necessary
- Check for infinite values in 'MeteorologyInterpolationData'
- New function 'correctionpoint'.
- Bug corrected in 'meteocomplete'. 
- Added conversion from specific humidity to relative humidity in meteocomplete.

# Version 0.7.8
- Corrected bug MeteorologyInterpolationData with SpatialPointsTopography as input
- Updated user guide documentation

# Version 0.7.7
- Empirical quantile mapping according to original method (Déqué)
- Dependency from package 'qmap' removed

# Version 0.7.6
- Methods 'print/head/tail' updated for objects SpatialPointsTopography and SpatialPointsMeteorology
- Subsetting for objects SpatialPointsMeteorology, SpatialGridMeteorology and SpatialPixelsMeteorology.
- Coercing objects of Spatial...Topography
- New vignette 'user guide' (old one renamed)
- Adapt to Rcpp changes

# Version 0.7.5
- AEMET download using packages httr and jsonlite (code adapted from https://github.com/SevillaR/aemet)
- Methods 'print/show' added for objects of class SpatialPointsMeteorology, SpatialGridMeteorology and SpatialPixelsMeteorology.
- Methods 'print/show' added for objects of class SpatialPointsTopography, SpatialGridTopography and SpatialPixelsTopography.
- Methods 'head/tail' added for objects SpatialPointsTopography and SpatialPointsMeteorology
- Bug correction: Historical download of AEMET data returns SunshineHours

# Version 0.7.4
- Added reference to publication
- Added citation reference.
- Bug correction: Over-dimensioned vectors for temperature differences (same for precipitation).
- Low-level interpolation routines made accessible to the user
- Bug correction: builder for MeteorologyInterpolationData
- Bug correction: Missing values in downloadAEMEThistorical

# Version 0.7.3
- New function 'meteocomplete'.
- Improvement of function 'meteoplot' to accept data frames with daily meteorological data as input

# Version 0.7.2
- Update of functions to download data from AEMET to deal with encoding
- Update of function 'SpatialPointsMeteorology' to build objects from sets of data frames, one per date
- Update of function 'MeteorologyInterpolationData' to build objects from different data structures
- New function 'summaryinterpolationdata' to summarize objects of class 'MeteorologyInterpolationData'

# Version 0.7.1
- Bug correction in interpolationpixels (with export=TRUE)
- Bug correction in correctionpoints.errors (with rds input)

# Version 0.7.0
- New S4 structures: SpatialPixelsTopography and SpatialPixelsMeteorology
- New function interpolationpixels
- Update of functions to download data from the Spanish Agencia Estatal de Meteorologia (AEMET)
- New function writemeteorologypixels
- Function correctiongrid removed (correction of weather series makes sense for point data)
- New function summarypixels

# Version 0.6.9
- Allow saturated (> 100% values) in relative humidity when extracting from NetCDF and when performing bias correction

# Version 0.6.8

- Modification of the precipitation interpolation (kernels)
- Modification of radiation calculation (diffuse light for slopes where direct light is too low)
- Modification of partitioning between direct and diffuse radiation

# Version 0.6.7

- Improvements in 'subsample' function
- New function 'radiation_skyLongwaveRadiation'

# Version 0.6.6

- Export format txt/rds
- Changed description

# Version 0.6.5

- Default correction method for wind switched to 'quantmap'.
- Unbias method for Tmin and Tmax applies bias from Tmean (if also unbiasing).
- Quantile mapping method for Tmin and Tmax is applied to (Tmin-Tmean) and (Tmax-Tmean).

# Version 0.6.4

- Corrected a bug in the correction of relative humidity.
- New function 'correctionpoints.errors' to evaluate correction errors for the reference period.

