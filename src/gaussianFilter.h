#include <Rcpp.h>

#ifndef GAUSSIAN_FILTER_H
#define GAUSSIAN_FILTER_H
#endif
using namespace Rcpp;

double gaussianFilter(double r, double Rp, double alpha);
NumericVector gaussianFilter(NumericVector r, double Rp, double alpha);
double estimateRp(NumericVector r, double iniRp, double alpha, int N, int iterations = 3);
