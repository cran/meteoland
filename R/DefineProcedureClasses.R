setClass("MeteorologyProcedureData", slots = list(dates = "Date"), contains="Spatial")
setClass("MeteorologyDownscalingData",
         slots = list(coords="matrix", historicdata = "ANY", futuredata = "ANY",
                      params = "list"),
         contains="MeteorologyProcedureData")
setClass("MeteorologyInterpolationData",
         slots=list(
           coords = "matrix",
           elevation = "numeric",
           slope = "numeric",
           aspect = "numeric",
           MinTemperature = "matrix",
           MaxTemperature = "matrix",
           SmoothedPrecipitation = "matrix",
           Precipitation = "matrix",
           SmoothedTemperatureRange = "matrix",
           RelativeHumidity = "matrix",
           Radiation = "ANY", #to allow null values, should be matrix
           WindSpeed = "ANY", #to allow null values, should be matrix
           WindDirection = "ANY", #to allow null values, should be matrix
           WindFields = "ANY",
           WFIndex = "ANY",
           WFFactor = "ANY",
           params = "list"),
         contains="MeteorologyProcedureData")
# setGeneric("meteopoints", valueClass =c("SpatialPointsMeteorology","SpatialPointsDataFrame", "NULL"), function(object, points, dates=NULL, ...){
#   standardGeneric("meteopoints")
# })
# setGeneric("meteogrid", valueClass =c("SpatialGridMeteorology","SpatialGridDataFrame","NULL"), function(object, landTopo, dates, ...){
#   standardGeneric("meteogrid")
# })
