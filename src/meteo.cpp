#include <Rcpp.h>
#include "gaussianFilter.h"

using namespace Rcpp;

// [[Rcpp::export(".pseudoRainfall")]]
NumericVector pseudoRainfall(NumericVector RainM, NumericVector daysMonthAll, double shape = 2.0, double scale = 4.0, int firstMonth = 1) {
  RNGScope scope;

  int nMonths = RainM.size();
  //Determines the number of days
  int numDays = 0;
  int month = firstMonth;
  for(int i=0;i<nMonths;i++) {
    numDays +=daysMonthAll[i];
    if(month==12) month=1;
    else month +=1;
  }

  NumericVector Rainfall=rep(0.0,numDays);
  NumericVector g = rgamma(numDays,shape, scale);
  NumericVector pd = runif(numDays);
  int cg = 0;
  int cumDays = 0;
  double NMD = 0, pMonth, rd;

  for(int m=0; m<nMonths; m++) {
    NMD = daysMonthAll[m];
    pMonth = RainM[m];
    while(pMonth>0){
      rd = g[cg];
      if(cg==numDays) cg=0;
      if(rd>pMonth) rd = pMonth;
      Rainfall[cumDays+((int)(pd[cg]*NMD))] += rd;
      pMonth = pMonth - rd;
      cg++;
    }
    if(RainM[m]>0) for(int d = cumDays; d<(cumDays+NMD);d++) Rainfall[d] /=RainM[m];
    cumDays += (int) NMD;
  }
  return(Rainfall);
}
// [[Rcpp::export(".meteo")]]
DataFrame meteo(DataFrame MeteoMonth, NumericVector landscapeRainfall = NumericVector::create(), double ERconv=0.05, double ERsyn = 0.2, double shape = 2.0, double scale = 4.0, double albedo = 0.17, int firstMonth = 1, bool cyclic = false) {
  RNGScope scope;

  NumericVector RainM = MeteoMonth["Rainfall"];
  NumericVector TempM = MeteoMonth["Temp"];
  NumericVector RadM = MeteoMonth["Rad"];
  NumericVector daysMonthAll = MeteoMonth["NumDays"];


  int nMonths = RainM.size();
  NumericVector ERMonthAll(nMonths);
  //Determines the number of days
  int numDays = 0;
  int month = firstMonth;
  for(int i=0;i<nMonths;i++) {
    numDays +=daysMonthAll[i];
    if(month<=5 || month==12) ERMonthAll[i] = ERsyn;
    else ERMonthAll[i] = ERconv;
    if(month==12) month=1;
    else month +=1;
  }

  NumericVector Rainfall=rep(0.0,numDays);
  NumericVector Temp=rep(0.0,numDays);
  NumericVector Rad=rep(0.0,numDays);
  NumericVector ER=rep(0.0,numDays);
  NumericVector g = rgamma(numDays,shape, scale);
  NumericVector pd = runif(numDays);
  int day = 0, cg = 0;
  int cumDays = 0;
  int prevM = 0;
  int nextM = 0;
  double NMD = 0, pMonth, rd, dd, nN, Rs;

  for(int m=0; m<nMonths; m++) {
    NMD = daysMonthAll[m];
    if(landscapeRainfall.size()>0) {
      for(int d=cumDays;d<cumDays+NMD;d++){
        Rainfall[d] = landscapeRainfall[d]*RainM[m];
      }
    } else {
      pMonth = RainM[m];
      while(pMonth>0){
        rd = g[cg];
        if(cg==numDays) cg=0;
        if(rd>pMonth) rd = pMonth;
        Rainfall[cumDays+((int)(pd[cg]*NMD))] += rd;
        pMonth = pMonth - rd;
        cg++;
      }
    }
    prevM = m-1;
    nextM = m+1;
    if(cyclic && prevM<0) prevM = 11;
    else if(prevM<0) prevM = 0;
    if(cyclic && m == nMonths-1) nextM = 0;
    else if(!cyclic && m == nMonths-1) nextM = m;
    for(int d=0;d<NMD;d++){
      dd = (double) d;
      if(dd<=(NMD/2.0)) {
        Temp[day] = TempM[m]*(((NMD/2.0)+dd)/NMD)+TempM[prevM]*(((NMD/2.0)-dd)/NMD);
        Rs = RadM[m]*(((NMD/2.0)+dd)/NMD)+RadM[prevM]*(((NMD/2.0)-dd)/NMD);
      } else {
        Temp[day] = TempM[m]*(((NMD*3.0/2.0)-dd)/NMD)+TempM[nextM]*((dd-(NMD/2.0))/NMD);
        Rs = RadM[m]*(((NMD*3.0/2.0)-dd)/NMD)+RadM[nextM]*((dd-(NMD/2.0))/NMD);
      }
      if(Rainfall[day]>0) nN = 0.25;
      else nN = 0.75;
      Rad[day] = (1.0-albedo)*Rs-0.1927987*(1.0+4.0*nN)*(100.0-Temp[day]);
      if(Rad[day]< 0.0) Rad[day] = 0.0;
      ER[day] = ERMonthAll[m];
      day++;
    }
    cumDays += (int) NMD;
  }
  Rcpp::DataFrame df = DataFrame::create(_["Rainfall"] = Rainfall,_["Temp"] = Temp,
                                         _["Rn"] = Rad,_["ER"] = ER);
  return(df);
}

// [[Rcpp::export(".temporalSmoothing")]]
NumericMatrix temporalSmoothing(NumericMatrix input, int numDays, bool prec) {
  int nrows = input.nrow();
  int ncols = input.ncol();
  NumericVector filter(2*numDays+1), weights(2*numDays+1);
  NumericMatrix output(nrows,ncols);
  for(int r = 0;r<nrows;r++) {  //Station loop
    for(int c = 0;c<ncols;c++) { //Day loop
      for(int fpos = -numDays;fpos<=numDays;fpos++) {
        if((c+fpos>-1) & (c+fpos<ncols)) {
          if(!NumericVector::is_na(input(r,c+fpos))) {
            filter[fpos+numDays] = input(r,c+fpos);
            weights[fpos+numDays] = 1.0;
            if(prec & (filter[fpos+numDays]==0.0)) weights[fpos+numDays] = 0.0;
          } else {
            filter[fpos+numDays] = 0.0;
            weights[fpos+numDays] = 0.0;
          }
        } else {
          filter[fpos+numDays] = 0.0;
          weights[fpos+numDays] = 0.0;
        }
      }
      double den = std::accumulate(weights.begin(), weights.end(), 0.0);
      if(den>0.0) output(r,c) = std::accumulate(filter.begin(), filter.end(), 0.0)/den;
      else output(r,c) = R_NaReal;
    }
  }
  return(output);
}

NumericVector weightedRegression(NumericVector Y, NumericVector X, NumericVector W) {
  NumericVector XW = X*W;
  NumericVector YW = Y*W;
  int n = X.size();
  double Wsum = std::accumulate(W.begin(), W.end(), 0.0);
  W = (((double) n)/Wsum)*W; //Normalize weights to sum n
  double wMeanX = std::accumulate(XW.begin(), XW.end(), 0.0)/((double)n);
  double wMeanY = std::accumulate(YW.begin(), YW.end(), 0.0)/((double)n);
  double cov = 0.0, var = 0.0;
  for (int k = 0; k < n; k++) {
      var = var + W[k] * X[k] * X[k];
      cov = cov + W[k] * X[k] * Y[k];
  }
  cov = cov -((double)n)*wMeanX*wMeanY;
  var = var - ((double) n)*wMeanX*wMeanX;
  double b = cov/var;
//  Rcout<< wMeanX<<" "<<wMeanY<<" "<< cov<<" "<< var<<"\n";
  double a = wMeanY-b*wMeanX;
  return(NumericVector::create(a,b));
}

double interpolateTemperaturePoint(double xp, double yp, double zp, NumericVector X, NumericVector Y, NumericVector Z, NumericVector T, NumericVector zDif, NumericVector tDif,double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3){
  int nstations = X.size();
  int nDif = tDif.size();
  NumericVector r(nstations);
  for(int i=0;i<nstations;i++) {
    r[i] = sqrt(pow(xp-X[i],2.0)+pow(yp-Y[i],2.0));
  }
  double Rp = estimateRp(r, iniRp, alpha, N, iterations);
  NumericVector W = gaussianFilter(r, Rp, alpha);
  //Weights for weighted regression
  NumericVector WDif(nDif);
  int c = 0;
  for(int i=0;i<nstations;i++) {
    for(int j=0;j<i;j++) {
      WDif[c] = W[i]*W[j];
      c++;
    }
  }
  //Weighted regression
  NumericVector wr = weightedRegression(tDif, zDif, WDif);
  double Wnum = 0.0;
  //Apply weighting
  for(int i=0;i<nstations;i++) {
    Wnum +=W[i]*(T[i]+wr[0]+wr[1]*(zp-Z[i]));
  }
  return(Wnum/std::accumulate(W.begin(), W.end(), 0.0));
}


NumericVector interpolateTemperaturePoints(NumericVector Xp, NumericVector Yp, NumericVector Zp, NumericVector X, NumericVector Y, NumericVector Z, NumericVector T,  double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3){
  int npoints = Xp.size();
  int nstations = X.size();
  NumericVector Tp(npoints);
  NumericVector zDif(nstations*(nstations-1));
  NumericVector tDif(nstations*(nstations-1));
  int c = 0;
  for(int i=0;i<nstations;i++) {
    for(int j=0;j<i;j++) {
      zDif[c] = Z[i]-Z[j];
      tDif[c] = T[i]-T[j];
      c++;
    }
  }
  for(int i=0;i<npoints;i++) {
    Tp[i] = interpolateTemperaturePoint(Xp[i], Yp[i], Zp[i], X,Y,Z,T, zDif, tDif, iniRp, alpha, N, iterations);
  }
  return(Tp);
}

// [[Rcpp::export(".interpolateTemperatureSeriesPoints")]]
NumericMatrix interpolateTemperatureSeriesPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp, NumericVector X, NumericVector Y, NumericVector Z, NumericMatrix T,  double iniRp = 140000, double alpha = 3.0, int N = 30, int iterations = 3){
  int npoints = Xp.size();
  int nstations = X.size();
  int nDays = T.ncol();
  NumericMatrix Tp(npoints,nDays);
  LogicalVector missing(nstations);
  for(int d = 0;d<nDays;d++) {
//    Rcout<<"Day: "<<d<<"\n";
    int nmis = 0;
    for(int i=0;i<nstations;i++) {
      missing[i] = NumericVector::is_na(T(i,d));
      if(missing[i]) nmis++;
    }
    NumericVector Tday(nstations-nmis);
    NumericVector Xday(nstations-nmis);
    NumericVector Yday(nstations-nmis);
    NumericVector Zday(nstations-nmis);
    int c = 0;
    for(int i=0;i<nstations;i++) {
      if(!missing[i]) {
        Tday[c] = T(i,d);
        Xday[c] = X[i];
        Yday[c] = Y[i];
        Zday[c] = Z[i];
        c++;
      }
    }
    NumericVector Tpday = interpolateTemperaturePoints(Xp, Yp,Zp, Xday, Yday, Zday, Tday,iniRp, alpha, N, iterations);
    for(int p=0;p<npoints;p++) {
      Tp(p,d) = Tpday[p];
    }
  }
  return(Tp);
}

double interpolatePrecipitationPoint(double xp, double yp, double zp, NumericVector X, NumericVector Y, NumericVector Z, NumericVector P, NumericVector zDif, NumericVector pRat,
                                     double iniRp = 140000, double alpha_event = 6.25, double alpha_amount = 6.25, int N_event = 20,int N_amount = 20, int iterations = 3, double popcrit = 0.5, double fmax = 0.95){
  int nstations = X.size();
  int nDif = pRat.size();
  NumericVector r(nstations);
  for(int i=0;i<nstations;i++) {
    r[i] = sqrt(pow(xp-X[i],2.0)+pow(yp-Y[i],2.0));
  }
  double Rp = estimateRp(r, iniRp, alpha_event, N_event, iterations);
  NumericVector Wevent = gaussianFilter(r, Rp, alpha_event);
  double Weventsum = std::accumulate(Wevent.begin(), Wevent.end(), 0.0);
  //Probability of precipitation
  double pop = 0.0;
  for(int i=0;i<nstations;i++) {
    if(P[i]>0) {
      pop += Wevent[i];
    }
  }
  pop = pop/Weventsum;
  //If precipitation occurs then calculate amount
  if(pop >=popcrit) {
    Rp = estimateRp(r, iniRp, alpha_amount, N_amount, iterations);
    NumericVector Wamount = gaussianFilter(r, Rp, alpha_amount);
    //Weights for weighted regression
    NumericVector WDif(nDif);
    int c = 0;
    for(int i=0;i<nstations;i++) {
      for(int j=0;j<i;j++) {
        WDif[c] = Wamount[i]*Wamount[j];
        c++;
      }
    }
    //Weighted regression
    NumericVector wr = weightedRegression(pRat, zDif, WDif);
//    Rcout<<Rp<<" " <<wr[0]<<" " << wr[1]<<"\n";
    double Wnum = 0.0, Wden = 0.0, f = 0.0;
    //Apply weighting
    for(int i=0;i<nstations;i++) {
      if(P[i]>0) {
        f = wr[0]+wr[1]*(zp-Z[i]);
//        Rcout<<f<<"\n";
        if(f>fmax) f = fmax;
        else if(f< ((-1.0)*fmax)) f = -1.0*fmax;
//        Rcout<<f<<" "<<((1.0+f)/(1.0-f))<<  "\n";
        Wnum +=Wevent[i]*P[i]*((1.0+f)/(1.0-f));
        Wden +=Wevent[i];
      }
    }
    return(Wnum/Wden);
  }
  return(0.0);
}

double interpolatePrecipitationEventPoint(double xp, double yp, double zp, NumericVector X, NumericVector Y, NumericVector Z, NumericVector Pevent,
                                          double iniRp = 140000, double alpha = 6.25, int N = 20, int iterations = 3, double popcrit = 0.5){
  int nstations = X.size();
  NumericVector r(nstations);
  for(int i=0;i<nstations;i++) {
    r[i] = sqrt(pow(xp-X[i],2.0)+pow(yp-Y[i],2.0));
  }
  double Rp = estimateRp(r, iniRp, alpha, N, iterations);
  NumericVector W = gaussianFilter(r, Rp, alpha);
  double Wsum = std::accumulate(W.begin(), W.end(), 0.0);
  //Probability of precipitation
  double pop = 0.0;
  for(int i=0;i<nstations;i++) {
    if(Pevent[i]>0) {
      pop += W[i];
    }
  }
  pop = pop/Wsum;
  //If precipitation occurs then calculate amount
  if(pop >=popcrit) return(1.0);
  return(0.0);
}

NumericVector interpolatePrecipitationPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp, NumericVector X, NumericVector Y, NumericVector Z, NumericVector P, NumericVector Psmooth,
                                             double iniRp = 140000, double alpha_event = 6.25, double alpha_amount = 6.25,
                                             int N_event = 20, int N_amount = 20,int iterations = 3, double popcrit = 0.5, double fmax = 0.95){
  int npoints = Xp.size();
  int nstations = X.size();
  NumericVector Pp(npoints);
  NumericVector zDif(nstations*(nstations-1));
  NumericVector pRat(nstations*(nstations-1));
  int c = 0;
  for(int i=0;i<nstations;i++) {
    for(int j=0;j<i;j++) {
      zDif[c] = Z[i]-Z[j];
      if((Psmooth[i]+Psmooth[j])>0.0) pRat[c] = (Psmooth[i]-Psmooth[j])/(Psmooth[i]+Psmooth[j]);
      c++;
    }
  }
  for(int i=0;i<npoints;i++) {
    Pp[i] = interpolatePrecipitationPoint(Xp[i], Yp[i], Zp[i], X,Y,Z,P, zDif, pRat,
                                          iniRp, alpha_event, alpha_amount, N_event, N_amount, iterations, popcrit, fmax);
  }
  return(Pp);
}

NumericVector interpolatePrecipitationEventPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp,
                                                  NumericVector X, NumericVector Y, NumericVector Z, NumericVector Pevent,
                                                  double iniRp = 140000, double alpha = 6.25, int N = 20, int iterations = 3, double popcrit = 0.5){
  int npoints = Xp.size();
  NumericVector Pp(npoints);
  for(int i=0;i<npoints;i++) {
    Pp[i] = interpolatePrecipitationEventPoint(Xp[i], Yp[i], Zp[i], X,Y,Z,Pevent,
                                               iniRp, alpha, N, iterations, popcrit);
  }
  return(Pp);
}

// [[Rcpp::export(".interpolatePrecipitationSeriesPoints")]]
NumericMatrix interpolatePrecipitationSeriesPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp, NumericVector X, NumericVector Y, NumericVector Z, NumericMatrix P, NumericMatrix Psmooth,
                                                   double iniRp = 140000, double alpha_event = 6.25, double alpha_amount = 6.25, int N_event = 20, int N_amount = 20,
                                                   int iterations = 3, double popcrit = 0.5, double fmax = 0.95){
  int npoints = Xp.size();
  int nstations = X.size();
  int nDays = P.ncol();
  NumericMatrix Pp(npoints,nDays);
  LogicalVector missing(nstations);
  for(int d = 0;d<nDays;d++) {
//    Rcout<<"Day: "<<d<<"\n";
    int nmis = 0;
    for(int i=0;i<nstations;i++) {
      missing[i] = NumericVector::is_na(P(i,d));
      if(missing[i]) nmis++;
    }
    NumericVector Pday(nstations-nmis);
    NumericVector Psmoothday(nstations-nmis);
    NumericVector Xday(nstations-nmis);
    NumericVector Yday(nstations-nmis);
    NumericVector Zday(nstations-nmis);
    int c = 0;
    for(int i=0;i<nstations;i++) {
      if(!missing[i]) {
        Pday[c] = P(i,d);
        Psmoothday[c] = Psmooth(i,d);
        Xday[c] = X[i];
        Yday[c] = Y[i];
        Zday[c] = Z[i];
        c++;
      }
    }
    NumericVector Ppday = interpolatePrecipitationPoints(Xp, Yp,Zp, Xday, Yday, Zday, Pday,Psmoothday, iniRp, alpha_event, alpha_amount,
                                                         N_event, N_amount, iterations, popcrit, fmax);
    for(int p=0;p<npoints;p++) {
      Pp(p,d) = Ppday[p];
    }
  }
  return(Pp);
}

// [[Rcpp::export(".interpolatePrecipitationEventSeriesPoints")]]
NumericMatrix interpolatePrecipitationEventSeriesPoints(NumericVector Xp, NumericVector Yp, NumericVector Zp,
                                                        NumericVector X, NumericVector Y, NumericVector Z, NumericMatrix Pevent,
                                                        double iniRp = 140000, double alpha = 6.25, int N = 20, int iterations = 3, double popcrit = 0.5){
  int npoints = Xp.size();
  int nstations = X.size();
  int nDays = Pevent.ncol();
  NumericMatrix Pp(npoints,nDays);
  LogicalVector missing(nstations);
  for(int d = 0;d<nDays;d++) {
    //    Rcout<<"Day: "<<d<<"\n";
    int nmis = 0;
    for(int i=0;i<nstations;i++) {
      missing[i] = NumericVector::is_na(Pevent(i,d));
      if(missing[i]) nmis++;
    }
    NumericVector Pevday(nstations-nmis);
    NumericVector Xday(nstations-nmis);
    NumericVector Yday(nstations-nmis);
    NumericVector Zday(nstations-nmis);
    int c = 0;
    for(int i=0;i<nstations;i++) {
      if(!missing[i]) {
        Pevday[c] = Pevent(i,d);
        Xday[c] = X[i];
        Yday[c] = Y[i];
        Zday[c] = Z[i];
        c++;
      }
    }
    NumericVector Ppday = interpolatePrecipitationEventPoints(Xp, Yp,Zp,
                                                         Xday, Yday, Zday, Pevday,
                                                         iniRp, alpha, N, iterations, popcrit);
    for(int p=0;p<npoints;p++) {
      Pp(p,d) = Ppday[p];
    }
  }
  return(Pp);
}

/**
 * Temperature (ºC) to vapor pressure (in Pa)
 * Murray (1967) formulation
 */
double temp2VP(double TD) {
  return(610.78*exp((17.269*TD)/(237.3+TD)));
}
/**
 * Calculates relative humidity from temperature
 * T - Average temperature (in Celsius)
 * TD - Dewpoint temperature (in Celsius)
 */
double relativeHumidity(double T,double TD) {
  return(std::min(100.0*(temp2VP(TD)/temp2VP(T)),100.0));
}

double vapourPressureFromRH(double T, double RH) {
  return(temp2VP(T)*(RH/100.0));
}

double dewpointTemperatureFromRH(double T, double RH) {
  double vp = vapourPressureFromRH(T,RH);
  return(std::min((237.3*log(vp/610.78))/(17.269-log(vp/610.78)),40.0));
}

// [[Rcpp::export(".vapourPressureFromRH")]]
NumericMatrix vapourPressureFromRH(NumericMatrix T, NumericMatrix RH) {
  int nr = T.nrow();
  int nc = T.ncol();
  NumericMatrix VP(nr, nc);
  for(int i=0;i<nr;i++){
    for(int j=0;j<nc;j++) {
      if(NumericVector::is_na(T(i,j)) | NumericVector::is_na(RH(i,j)) ) {
        VP(i,j) = NA_REAL;
      } else {
        VP(i,j) = vapourPressureFromRH(T(i,j), RH(i,j));
      }
    }
  }
  return(VP);
}

// [[Rcpp::export(".dewpointTemperatureFromRH")]]
NumericMatrix dewpointTemperatureFromRH(NumericMatrix T, NumericMatrix RH) {
  int nr = T.nrow();
  int nc = T.ncol();
  NumericMatrix DT(nr, nc);
  for(int i=0;i<nr;i++){
    for(int j=0;j<nc;j++) {
      if(NumericVector::is_na(T(i,j)) | NumericVector::is_na(RH(i,j)) ) {
        DT(i,j) = NA_REAL;
      } else {
        DT(i,j) = dewpointTemperatureFromRH(T(i,j), RH(i,j));
      }
    }
  }
  return(DT);
}

/**
 * Assumes that minimum daily temperature is a reasonable surrogate of dew-point temperature!
 */

// [[Rcpp::export(".temp2VP")]]
NumericVector temp2VP(NumericVector TD) {
  NumericVector vp(TD.size());
  for(int i=0;i<TD.size();i++) vp[i] = temp2VP(TD[i]);
  return(vp);
}


// [[Rcpp::export(".relativeHumidityFromMinMaxTemp")]]
NumericVector relativeHumidityFromMinMaxTemp(NumericVector Tmin,NumericVector Tmax) {
  NumericVector rh(Tmin.size());
  for(int i=0;i<Tmin.size();i++) rh[i] = relativeHumidity(Tmax[i]*0.606+Tmin[i]*0.394, Tmin[i]);
  return(rh);
}

// [[Rcpp::export(".relativeHumidityFromDewpointTemp")]]
NumericVector relativeHumidityFromDewpointTemp(NumericVector T, NumericVector TD) {
  NumericVector rh(T.size());
  for(int i=0;i<T.size();i++) rh[i] = relativeHumidity(T[i], TD[i]);
  return(rh);
}
