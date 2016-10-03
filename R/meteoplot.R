meteoplot<-function(object, index, variable="MeanTemperature", add = FALSE, ...){
  if(!inherits(object,"SpatialPointsMeteorology") && !inherits(object,"SpatialGridMeteorology")) stop("'object' should be of class 'SpatialPointsMeteorology' or 'SpatialGridMeteorology'.")
  dates = object@dates
  if(inherits(object,"SpatialPointsMeteorology")) {
    vec = object@data[[index]][,variable]
  } else {
    vec = numeric(length(dates))
    for(i in 1:length(dates)) {
      vec[i] = object@data[[i]][index,variable]
    }
  }
  if(add) {
    lines(dates,vec, ...)
  } else {
    if(variable=="Precipitation") plot(dates,vec, type="h",...)
    else plot(dates,vec, type="l",...)
  }
}
