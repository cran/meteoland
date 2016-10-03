#include <Rcpp.h>
using namespace Rcpp;

const double GSC = 0.0820; //solar constant in MJ/m2/min
const double SIGMA = 4.903*pow(10,-9.0); //Stefan-Boltzmann constant MJ/K^4/m2/day
const double LAMBDA = 2.45; //latent head of vaporisation MJ.kg^-1
/*
 Daily equilibrium evapotranspiration
The equation requires the daily radiation, and neglects
effects of diurnal temperature variations on s, g and L
We also neglect long-wave flux (but see Linacre 1968).
Long-wave radiation (Linacre 1968)
R_longwave = 32*10^(-5)*(1+4*(n/N))*(100-T) (cal/cm2.min)
Cal2Joules = 4.18400 cal/J
min2day = 24*60 min/day
*/
// [[Rcpp::export(".dailyEquilibriumPET")]]
NumericVector dailyEquilibriumPET(NumericVector Temp, NumericVector Rn){
  int n = Temp.size();
  NumericVector D(n);
  double s = 0.0;
  for(int i=0;i<n;i++){
    s = 2503000*exp(17.269*Temp[i]/(237.3+Temp[i]))/pow(237.3+Temp[i],2.0);
    D[i] = 10000*(s/(s+65))*Rn[i]/2500000; // L = 2.5*10^6
  }
  return(D);
}

/**
 * Daily PET
 * Recoded from ET.Penman function in package 'Evapotranspiration'
 *
 * latitude - Latitude (degrees)
 * elevation - Elevation (m)
 * J - Julian day
 * Tmax - Maximum temperature (Celsius)
 * Tmin - Minimum temperature (Celsius)
 * RHmax - Maximum relative humidity (%)
 * RHmin - Minimum relative humidity (%)
 * R_s - Incident solar radiation (MJ/m2)
 * u - wind speed (m/s)
 * z - wind measuring height (m)
 * z0 - Roughness height (m)
 * alpha - Albedo (from 0 to 1)
 * windfun - Wind function version of Penman "1956" or "1948"
 */
double PenmanPET(double latitude, double elevation, int J,
                 double Tmin, double Tmax, double RHmin, double RHmax, double R_s,
                 double u, double z = 2.0, double z0 = 0.001,
                 double alpha = 0.08, String windfun = "1956") {
  latitude = latitude*(PI/180.0); //from degrees to radians
  double Ta = (Tmax + Tmin)/2.0;
  double RHmean = (RHmax + RHmin)/2.0;
  double P = 101.3 * pow((293.0 - 0.0065 * elevation)/293.0,5.26);
  double delta = 4098.0 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/pow(Ta +
    237.3,2.0);
  double gamma = 0.00163*(P/LAMBDA);
  double d_r2 = 1.0 + 0.033 * cos(2.0 * PI/365.0 * ((double)J));
  double delta2 = 0.409 * sin(2.0 * PI/365.0 * ((double)J) - 1.39);
  double w_s = acos(-tan(latitude) * tan(delta2));
  // double N = 24.0/PI*w_s; (N not used)
  double R_a = (1440.0/PI) * d_r2 * GSC * (w_s * sin(latitude) * sin(delta2) + cos(latitude) * cos(delta2) * sin(w_s));
  double R_so = (0.75 + (2.0 * 0.00001) * elevation) * R_a;
  double PET = 0.0;
  if(!NumericVector::is_na(u)) {
    double u2 = u * log(2.0/z0)/log(z/z0);
    double vs_Tmax = 0.6108 * exp(17.27 * Tmax/(Tmax + 237.3));
    double vs_Tmin = 0.6108 * exp(17.27 * Tmax/(Tmax + 237.3));
    double vas = (vs_Tmax + vs_Tmin)/2.0;
    double vabar = (vs_Tmin * (RHmax/100.0) + vs_Tmax * (RHmin/100.0))/2.0;
    double R_nl = SIGMA * (0.34 - 0.14 * sqrt(vabar)) * (pow(Tmax + 273.2,4.0) + pow(Tmin + 273.2,4.0))/2.0 * (1.35 * R_s/R_so - 0.35);
    double R_ns = (1.0 - alpha) * R_s; //Short radiation (after acounting for surface albedo)
    double R_n = R_ns - R_nl; //Net radiation
    double Ea = (vas - vabar);
    if(windfun == "1956") {
      Ea = Ea*(1.313 + 1.381 * u2);
    } else if(windfun == "1948") {
      Ea = Ea*(2.626 + 1.381*u2);
    }
    PET = delta/(delta + gamma) * (R_n/LAMBDA) + gamma/(delta + gamma) * Ea;
  } else {
    //Equation by Valiantzas (2006, eq 33) for situations where wind is not available
    //Valiantzas JD (2006) Simplified versions for the Penman evaporation equation using routine weather data. Journal of Hydrology 331, 690–702. doi:10.1016/j.jhydrol.2006.06.012.
    PET = 0.047 * R_s * sqrt(Ta + 9.5) - 2.4 * pow(R_s/R_a,2.0) + 0.09 * (Ta + 20.0) * (1.0 - RHmean/100.0);
  }
  if(PET<0.0) PET = 0.0;
  return(PET);
}

// [[Rcpp::export("penmanpoint")]]
NumericVector PenmanPETPointSeries(double latitude, double elevation, IntegerVector J,
                 NumericVector Tmin, NumericVector Tmax, NumericVector RHmin, NumericVector RHmax, NumericVector R_s,
                 NumericVector u, double z = 2.0,
                 double z0 = 0.001, double alpha = 0.08, String windfun ="1956") {
  int ndays = J.size();
  NumericVector PET(ndays);
  for(int d=0; d<ndays;d++) {
    if(NumericVector::is_na(z)) PET[d] = PenmanPET(latitude, elevation, J[d],Tmin[d], Tmax[d], RHmin[d],RHmax[d], R_s[d], R_NaReal, R_NaReal, z0, alpha);
    else PET[d] = PenmanPET(latitude, elevation, J[d],
                            Tmin[d], Tmax[d], RHmin[d],RHmax[d], R_s[d],
                            u[d], z, z0, alpha, windfun);
  }
  return(PET);
}

// [[Rcpp::export(".PenmanPETPointsDay")]]
NumericVector PenmanPETPointsDay(NumericVector latitude, NumericVector elevation, int J,
                                 NumericVector Tmin, NumericVector Tmax, NumericVector RHmin, NumericVector RHmax, NumericVector R_s,
                                 NumericVector u, double z = 2.0,
                                 double z0 = 0.001, double alpha = 0.08,
                                 String windfun ="1956") {
  int points = latitude.size();
  NumericVector PET(points);
  for(int d=0; d<points;d++) {
    PET[d] = PenmanPET(latitude[d], elevation[d], J,
                       Tmin[d], Tmax[d], RHmin[d],RHmax[d], R_s[d],
                       u[d], z, z0, alpha, windfun);
  }
  return(PET);
}
