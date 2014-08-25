#include <RcppArmadillo.h>
using namespace Rcpp;
#include "Utils.h"

// [[Rcpp::export]]
SEXP runifsphereCPP(double n, double p) 
{  
  arma::mat X = Utils::runifsphere(n, p);
  return wrap(X);
}

// [[Rcpp::export]]
SEXP covCPP(SEXP rX, int threads) 
{
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  arma::mat cov = Utils::cov(X, threads);
  return wrap(cov);
}

// [[Rcpp::export]]
SEXP meanCPP(SEXP rX, int threads) 
{
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  arma::rowvec meanr = Utils::mean(X, threads);
  return wrap(meanr);
}
