setClass("SpatialPointsMeteorology", slots = list(dates = "Date", data="vector"), contains="SpatialPoints")
setClass("SpatialPointsTopography", contains="SpatialPointsDataFrame")
setClass("SpatialGridMeteorology", slots = list(dates = "Date", data="vector"), contains="SpatialGrid")
setClass("SpatialGridTopography", contains="SpatialGridDataFrame")

setMethod("spplot", signature("SpatialGridMeteorology"), definition=
            function(obj, date, variable="MeanTemperature", ...) {
              sgd = SpatialGridDataFrame(obj@grid, obj@data[[date]], obj@proj4string)
              spplot(sgd, variable, ...)
            }
)
setMethod("spplot", signature("SpatialGridTopography"), definition =
            function(obj, variable="elevation",...) {
              if(variable=="elevation") {
                spplot(as(obj,"SpatialGridDataFrame"), zcol = "elevation",
                       col.regions = topo.colors,...)
              } else if(variable=="slope") {
                spplot(as(obj,"SpatialGridDataFrame"), zcol = "slope",
                       ...)
              } else if(variable=="aspect") {
                spplot(as(obj,"SpatialGridDataFrame"), zcol = "aspect",
                       col.regions = colorRampPalette(c("black","green","red","blue", "black")),
                       at = seq(0,360, by=5),...)
              } else if(variable=="N-S") {
                spplot(SpatialGridDataFrame(obj@grid, data.frame(NS = cos(pi*obj@data$aspect/180))),...)
              } else if(variable=="E-W") {
                spplot(SpatialGridDataFrame(obj@grid, data.frame(EW = sin(pi*obj@data$aspect/180))),...)
              }
            }
)
