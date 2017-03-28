#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP meteoland_airDensity(SEXP, SEXP);
extern SEXP meteoland_aspect(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_atmosphericPressure(SEXP);
extern SEXP meteoland_averageDailyVapourPressure(SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_averageDaylightTemperature(SEXP, SEXP);
extern SEXP meteoland_dailyEquilibriumPET(SEXP, SEXP);
extern SEXP meteoland_dateStringToJulianDays(SEXP);
extern SEXP meteoland_daylength(SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_daylengthseconds(SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_dewpointTemperatureFromRH(SEXP, SEXP);
extern SEXP meteoland_directDiffuseDay(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_directDiffuseInstant(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_getWindFieldIndexAndFactor(SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_interpolatePrecipitationEventSeriesPoints(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_interpolatePrecipitationSeriesPoints(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_interpolateTdewSeriesPoints(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_interpolateTemperatureSeriesPoints(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_interpolateWindFieldSeriesPoints(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_interpolateWindStationSeriesPoints(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_julianDay(SEXP, SEXP, SEXP);
extern SEXP meteoland_latentHeatVaporisation(SEXP);
extern SEXP meteoland_latentHeatVaporisationMol(SEXP);
extern SEXP meteoland_meteo(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_netRadiation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_outgoingLongwaveRadiation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_PenmanMonteithPET(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_PenmanMonteithPETPointSeries(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_PenmanPET(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_PenmanPETPointsDay(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_PenmanPETPointSeries(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_potentialRadiationPoints(SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_potentialRadiationSeries(SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_pseudoRainfall(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_psychrometricConstant(SEXP, SEXP);
extern SEXP meteoland_radiationPoints(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_radiationSeries(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_RcppExport_registerCCallable();
extern SEXP meteoland_RDay(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_relativeHumidityFromDewpointTemp(SEXP, SEXP);
extern SEXP meteoland_relativeHumidityFromMinMaxTemp(SEXP, SEXP);
extern SEXP meteoland_RpotDay(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_saturationVaporPressureCurveSlope(SEXP);
extern SEXP meteoland_saturationVapourPressure(SEXP);
extern SEXP meteoland_slope(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_solarConstant(SEXP);
extern SEXP meteoland_solarDeclination(SEXP);
extern SEXP meteoland_solarElevation(SEXP, SEXP, SEXP);
extern SEXP meteoland_sunRiseSet(SEXP, SEXP, SEXP, SEXP);
extern SEXP meteoland_temp2SVP(SEXP);
extern SEXP meteoland_temporalSmoothing(SEXP, SEXP, SEXP);
extern SEXP meteoland_vapourPressureFromRH(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"meteoland_airDensity",                                (DL_FUNC) &meteoland_airDensity,                                 2},
  {"meteoland_aspect",                                    (DL_FUNC) &meteoland_aspect,                                     5},
  {"meteoland_atmosphericPressure",                       (DL_FUNC) &meteoland_atmosphericPressure,                        1},
  {"meteoland_averageDailyVapourPressure",                (DL_FUNC) &meteoland_averageDailyVapourPressure,                 4},
  {"meteoland_averageDaylightTemperature",                (DL_FUNC) &meteoland_averageDaylightTemperature,                 2},
  {"meteoland_dailyEquilibriumPET",                       (DL_FUNC) &meteoland_dailyEquilibriumPET,                        2},
  {"meteoland_dateStringToJulianDays",                    (DL_FUNC) &meteoland_dateStringToJulianDays,                     1},
  {"meteoland_daylength",                                 (DL_FUNC) &meteoland_daylength,                                  4},
  {"meteoland_daylengthseconds",                          (DL_FUNC) &meteoland_daylengthseconds,                           4},
  {"meteoland_dewpointTemperatureFromRH",                 (DL_FUNC) &meteoland_dewpointTemperatureFromRH,                  2},
  {"meteoland_directDiffuseDay",                          (DL_FUNC) &meteoland_directDiffuseDay,                           8},
  {"meteoland_directDiffuseInstant",                      (DL_FUNC) &meteoland_directDiffuseInstant,                       9},
  {"meteoland_getWindFieldIndexAndFactor",                (DL_FUNC) &meteoland_getWindFieldIndexAndFactor,                 4},
  {"meteoland_interpolatePrecipitationEventSeriesPoints", (DL_FUNC) &meteoland_interpolatePrecipitationEventSeriesPoints, 12},
  {"meteoland_interpolatePrecipitationSeriesPoints",      (DL_FUNC) &meteoland_interpolatePrecipitationSeriesPoints,      16},
  {"meteoland_interpolateTdewSeriesPoints",               (DL_FUNC) &meteoland_interpolateTdewSeriesPoints,               11},
  {"meteoland_interpolateTemperatureSeriesPoints",        (DL_FUNC) &meteoland_interpolateTemperatureSeriesPoints,        11},
  {"meteoland_interpolateWindFieldSeriesPoints",          (DL_FUNC) &meteoland_interpolateWindFieldSeriesPoints,          12},
  {"meteoland_interpolateWindStationSeriesPoints",        (DL_FUNC) &meteoland_interpolateWindStationSeriesPoints,        10},
  {"meteoland_julianDay",                                 (DL_FUNC) &meteoland_julianDay,                                  3},
  {"meteoland_latentHeatVaporisation",                    (DL_FUNC) &meteoland_latentHeatVaporisation,                     1},
  {"meteoland_latentHeatVaporisationMol",                 (DL_FUNC) &meteoland_latentHeatVaporisationMol,                  1},
  {"meteoland_meteo",                                     (DL_FUNC) &meteoland_meteo,                                      9},
  {"meteoland_netRadiation",                              (DL_FUNC) &meteoland_netRadiation,                              11},
  {"meteoland_outgoingLongwaveRadiation",                 (DL_FUNC) &meteoland_outgoingLongwaveRadiation,                 10},
  {"meteoland_PenmanMonteithPET",                         (DL_FUNC) &meteoland_PenmanMonteithPET,                          8},
  {"meteoland_PenmanMonteithPETPointSeries",              (DL_FUNC) &meteoland_PenmanMonteithPETPointSeries,               8},
  {"meteoland_PenmanPET",                                 (DL_FUNC) &meteoland_PenmanPET,                                 15},
  {"meteoland_PenmanPETPointsDay",                        (DL_FUNC) &meteoland_PenmanPETPointsDay,                        15},
  {"meteoland_PenmanPETPointSeries",                      (DL_FUNC) &meteoland_PenmanPETPointSeries,                      15},
  {"meteoland_potentialRadiationPoints",                  (DL_FUNC) &meteoland_potentialRadiationPoints,                   4},
  {"meteoland_potentialRadiationSeries",                  (DL_FUNC) &meteoland_potentialRadiationSeries,                   4},
  {"meteoland_pseudoRainfall",                            (DL_FUNC) &meteoland_pseudoRainfall,                             5},
  {"meteoland_psychrometricConstant",                     (DL_FUNC) &meteoland_psychrometricConstant,                      2},
  {"meteoland_radiationPoints",                           (DL_FUNC) &meteoland_radiationPoints,                            9},
  {"meteoland_radiationSeries",                           (DL_FUNC) &meteoland_radiationSeries,                            9},
  {"meteoland_RcppExport_registerCCallable",              (DL_FUNC) &meteoland_RcppExport_registerCCallable,               0},
  {"meteoland_RDay",                                      (DL_FUNC) &meteoland_RDay,                                      10},
  {"meteoland_relativeHumidityFromDewpointTemp",          (DL_FUNC) &meteoland_relativeHumidityFromDewpointTemp,           2},
  {"meteoland_relativeHumidityFromMinMaxTemp",            (DL_FUNC) &meteoland_relativeHumidityFromMinMaxTemp,             2},
  {"meteoland_RpotDay",                                   (DL_FUNC) &meteoland_RpotDay,                                    5},
  {"meteoland_saturationVaporPressureCurveSlope",         (DL_FUNC) &meteoland_saturationVaporPressureCurveSlope,          1},
  {"meteoland_saturationVapourPressure",                  (DL_FUNC) &meteoland_saturationVapourPressure,                   1},
  {"meteoland_slope",                                     (DL_FUNC) &meteoland_slope,                                      5},
  {"meteoland_solarConstant",                             (DL_FUNC) &meteoland_solarConstant,                              1},
  {"meteoland_solarDeclination",                          (DL_FUNC) &meteoland_solarDeclination,                           1},
  {"meteoland_solarElevation",                            (DL_FUNC) &meteoland_solarElevation,                             3},
  {"meteoland_sunRiseSet",                                (DL_FUNC) &meteoland_sunRiseSet,                                 4},
  {"meteoland_temp2SVP",                                  (DL_FUNC) &meteoland_temp2SVP,                                   1},
  {"meteoland_temporalSmoothing",                         (DL_FUNC) &meteoland_temporalSmoothing,                          3},
  {"meteoland_vapourPressureFromRH",                      (DL_FUNC) &meteoland_vapourPressureFromRH,                       2},
  {NULL, NULL, 0}
};

void R_init_meteoland(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
