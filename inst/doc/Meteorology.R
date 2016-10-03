### R code from vignette source 'Meteorology.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Meteorology.Rnw:25-27
###################################################
options(width=67)
library(meteoland)


###################################################
### code chunk number 2: Meteorology.Rnw:62-63
###################################################
showClass("SpatialPointsTopography")


###################################################
### code chunk number 3: Meteorology.Rnw:67-68
###################################################
showClass("SpatialGridTopography")


###################################################
### code chunk number 4: Meteorology.Rnw:77-78
###################################################
showClass("SpatialPointsMeteorology")


###################################################
### code chunk number 5: Meteorology.Rnw:82-83
###################################################
showClass("SpatialGridMeteorology")


###################################################
### code chunk number 6: Meteorology.Rnw:118-119
###################################################
showClass("MeteorologyInterpolationData")


###################################################
### code chunk number 7: Meteorology.Rnw:130-131
###################################################
showClass("MeteorologyDownscalingData")


###################################################
### code chunk number 8: Meteorology.Rnw:149-157
###################################################
r = 0:1000
R_p = 500
gf1 = exp(-3.0*((r/R_p)^2.0)) - exp(-3.0)
gf2 = exp(-6.25*((r/R_p)^2.0)) - exp(-6.25)
gf1[r>R_p] = 0
gf2[r>R_p] = 0
plot(r, gf1, type="l", ylab = "W(r)", xlab ="r")
lines(r, gf2, lty=2)


