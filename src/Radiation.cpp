#include <Rcpp.h>
using namespace Rcpp;


const double SOLAR = 1.361; //in kW/m2


/**
* Returns the sunrise and sunset hours in hour angle (radians)
*
* L0 - Latitude of actual slope, in radians
* A - Azimuth of slope, in radians from north
* I - Inclination of slope, in radians above horizontal
* delta - Solar declination, in radians
*/
NumericVector sunRiseSet(double L0, double A, double I, double delta){
  double L1 = asin(cos(I)*sin(L0)+sin(I)*cos(L0)*cos(A)); //latitude on equivalent slope
  double den = cos(I)*cos(L0)-sin(I)*sin(L0)*cos(A);
  double L2;
  if(den<0) {
    L2 = atan((sin(I)*sin(A))/den)+PI;
  } else {
    L2 = atan((sin(I)*sin(A))/den);
  }
  double T = acos(std::max(std::min(-tan(L1)*tan(delta),1.0),-1.0));
  double T7 = T-L2; //hour angle of sunset on equivalent slope
  double T6 = -T-L2; //hour angle of sunrise on equivalent slope
  double T1 = acos(std::max(std::min(-tan(L0)*tan(delta),1.0),-1.0));  //hour angle of sunset on horizontal surface
  double T0 = - T1; //hour angle of sunrise on horizontal surface
  double T3 = std::min(T1,T7); //hour angle of sunset on slope
  double T2 = std::max(T0,T6); //hour angle of sunrise on slope
  return(NumericVector::create(T2,T3));
}

/**
* Returns instant radiation (in kW/m2). From Granier & Ohmura (1968)
* Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800
*
*  phi - latitude of the slope, in radians
*  A - Azimuth of slope, in radians from north
*  Zx - Zenith angle of the vector normal to the slope (= slope?), in radians
*  delta - Solar declination, in radians
*  H - hour angle measured from solar noon, in radians
*/
double RpotInstant(double phi, double A, double Zx, double delta, double H) {
  double t1 = (sin(phi)*cos(H))*(-1.0*cos(A)*sin(Zx)) -1.0*sin(H)*(sin(A)*sin(Zx)) + (cos(phi)*cos(H))*cos(Zx);
  double t2 = cos(phi)*(cos(A)*sin(Zx)) + sin(phi)*cos(Zx);
  return(SOLAR*(t1*cos(delta)+ t2*sin(delta)));
}

/**
* One-day potential solar radiation (in MJ/m2). From Granier & Ohmura (1968)
* Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800
*
*  phi - latitude of the slope, in degrees
*  A - Azimuth of slope, in degrees from north
*  Zx - Zenith angle of the vector normal to the slope (= slope) in degrees
*  J - Julian day (1 to 365)
*  step - Number of seconds step (for integration of instant radiation)
*/
double RpotDay(double phi, double A, double Zx, double J, double step = 1200.0) {
  //Translate to radians
  phi = phi*(PI/180.0);
  A = A*(PI/180.0);
  Zx = Zx*(PI/180.0);
  //Solar declination (in radians) from julian day
  double delta = 0.4093*sin((2.0*PI*((double)J)/365.0) - 1.405);
  double hradstep = step*(5.0/1200.0)*(PI/180.0);
  double Rpot = 0.0;
  NumericVector srs = sunRiseSet(phi, A, Zx, delta);
  for(double hrad=srs[0];hrad<srs[1];hrad+=hradstep) { //72 steps (3 per hour)
    Rpot += step*std::max(0.0, RpotInstant(phi, A, Zx, delta, hrad));
  }
  return(Rpot/1000.0);
}



/**
* Returns a vector with potential daily radiation (in MJ/m2).
*
*  phi - latitude of the slope, in degrees
*  A - Azimuth of slope, in degrees from north
*  Zx - Zenith angle of the vector normal to the slope (= slope?) in degrees
*  doy - A vector with julian days
*  step - Number of seconds step (for integration of instant radiation)
*/
// [[Rcpp::export(".potentialRadiationSeries")]]
NumericVector potentialRadiationSeries(double phi, double A, double Zx, NumericVector doy, double step = 1200.0) {
  NumericVector Rpot(doy.size());
  for(int i=0;i<doy.size(); i++) {
    Rpot[i] = RpotDay(phi,A,Zx,doy[i], step);
  }
  return(Rpot);
}
// [[Rcpp::export(".potentialRadiationPoints")]]
NumericVector potentialRadiationPoints(double phi, NumericVector A, NumericVector Zx, int doy, double step = 1200.0) {
  NumericVector Rpot(A.size());
  for(int i=0;i<A.size(); i++) {
    Rpot[i] = RpotDay(phi,A[i],Zx[i],doy, step);
  }
  return(Rpot);
}

/**
* One-day solar radiation (in MJ/m2). From Thornton & Running (1999)
*
*  phi - latitude of the slope, in degrees
*  elevation - Elevation from sea level (in meters)
*  A - Azimuth of slope, in degrees from north
*  Zx - Zenith angle of the vector normal to the slope (= slope?) in degrees
*  doy - Day of the year, Julian day (1 to 365)
*  diffTemp - Difference between maximum and minimum temperature (in degrees Celsius)
*  VP - Vapor pressure (in Pa)
*  P - Precipitation (in mm of water)
*  step - Number of seconds step (for integration of instant potential radiation and maximum transmittance)
*
*  Bibliography:
*  Thornton, P.E., Running, S.W., 1999. An improved algorithm for estimating incident daily solar radiation from measurements of temperature, humidity, and precipitation. Agric. For. Meteorol. 93, 211–228. doi:10.1016/S0168-1923(98)00126-9
*  Garnier, B.J., Ohmura, A., 1968. A method of calculating the direct shortwave radiation income of slopes. J. Appl. Meteorol. 7: 796-800
*  Pearcy, R.W., Ehleringer, J.R., Mooney, H.A., Rundel, P.W., 1991. Plant Physiological Ecology: Field Methods and Instrumentation. Chapman & Hall, New York 455 pp.
*/
double RDay(double phi, double elevation, double A, double Zx, double doy,
            double diffTemp, double diffTempMonth, double VP, double P, double step = 1200.0) {
  //Translate to radians
  phi = phi*(PI/180.0);
  A = A*(PI/180.0);
  Zx = Zx*(PI/180.0);
  //Solar declination from julian day
  double delta = 0.4093*sin((2.0*PI*((double)doy)/365.0) - 1.405);
  double Rpot = 0.0;
  double B = 0.031+0.201*exp(-0.185*diffTempMonth);
  double Tfmax = 1.0-0.9*exp(-B*pow(diffTemp,1.5));
  if(P>0.0) Tfmax *=0.75; //Correction for wet days
  double Ttmax = 0.0, RpotInst = 0.0, theta=0.0;
  double pratio = pow(1.0 -2.2569e-5*elevation,5.2553); // from Pearcy et al. 1991
  double hradstep = step*(5.0/1200.0)*(PI/180.0);
  NumericVector srs = sunRiseSet(phi, A, Zx, delta);
  for(double hrad=srs[0];hrad<srs[1];hrad+=hradstep) { //72 steps (3 per hour)
    RpotInst = RpotInstant(phi, A, Zx, delta, hrad);
    if(RpotInst>0.0) {
      //Solar zenith angle
      theta = sin(phi)*sin(delta)+cos(phi)*cos(delta)*cos(hrad);
      Ttmax +=step*RpotInst*pow(0.87,pratio*(1.0/cos(theta)));
      Rpot += step*RpotInst;
    }
  }
  Ttmax = (Ttmax/Rpot) -6.1e-5*VP;
  //  Rcout<<Rpot<<" "<<Ttmax<<" "<<Tfmax<<"\n";
  if(Rpot==0.0) return(0.0);
  return(Rpot*Ttmax*Tfmax/1000.0); //Radiation in MJ/m2
}


/**
* Returns a vector with daily radiation (in MJ/m2).
*
*  phi - latitude of the slope, in degrees
*  A - Azimuth of slope, in degrees from north
*  Zx - Zenith angle of the vector normal to the slope (= slope?) in degrees
*  doy - A vector with julian days
*  step - Number of seconds step (for integration of instant radiation)
*/
// [[Rcpp::export(".radiationSeries")]]
NumericVector radiationSeries(double phi, double elevation, double A, double Zx, NumericVector doy,
                              NumericVector diffTemp, NumericVector diffTempMonth, NumericVector VP, NumericVector P, double step = 1200.0) {
  NumericVector Rpot(doy.size());
  for(int i=0;i<doy.size(); i++) {
    Rpot[i] = RDay(phi,elevation, A,Zx,doy[i], diffTemp[i], diffTempMonth[i], VP[i], P[i],step);
  }
  return(Rpot);
}

// [[Rcpp::export(".radiationPoints")]]
NumericVector radiationPoints(NumericVector phi, NumericVector elevation, NumericVector A, NumericVector Zx, int doy,
                              NumericVector diffTemp, NumericVector diffTempMonth, NumericVector VP, NumericVector P,double step = 1200.0) {
  int npoints = A.size();
  NumericVector Rpot(npoints);
  for(int i=0;i<npoints; i++) {
    Rpot[i] = RDay(phi[i],elevation[i], A[i],Zx[i],doy, diffTemp[i], diffTempMonth[i],VP[i], P[i], step);
  }
  return(Rpot);
}


