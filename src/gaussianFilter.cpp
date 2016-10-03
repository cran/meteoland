#include <Rcpp.h>
using namespace Rcpp;

double gaussianFilter(double r, double Rp, double alpha) {
  if(r>Rp) return(0.0);
  return(exp(-alpha*pow(r/Rp,2.0))-exp(-alpha));
}

NumericVector gaussianFilter(NumericVector r, double Rp, double alpha) {
  int n = r.size();
  NumericVector w(n);
  for(int i=0;i<n;i++) {
    w[i] = gaussianFilter(r[i], Rp, alpha);
  }
  return(w);
}

double estimateRp(NumericVector r, double iniRp, double alpha, int N, int iterations = 3) {
  //Initialize with initial value
  double Rpest = iniRp;
  NumericVector wIni;
  double Dp;
  double Wmean = ((1.0-exp(-alpha))/alpha)-exp(-alpha);
  double Nstar = (double) 2.0*N;
  double Wsum;
  for(int it = 0;it<iterations;it++) {
    //Estimate weights and station density
    wIni = gaussianFilter(r,Rpest,alpha);
    Wsum = std::accumulate(wIni.begin(), wIni.end(), 0.0);
    Dp = (Wsum/Wmean)/(3.141592*pow(Rpest,2.0));
    if(it==(iterations-1)) Nstar = (double) N;
    Rpest = sqrt(Nstar/(Dp*3.141592));
    // Rcout<<Dp<< " "<<Wsum<<" "<< Wmean<<" "<<Rpest<<"\n";
  }
  return(Rpest);
}
