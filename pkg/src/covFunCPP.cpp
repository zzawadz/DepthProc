#include <RcppArmadillo.h>
using namespace Rcpp;
#include "RobCovLib/RobCovLib.h"

// [[Rcpp::export]]
SEXP CovLPCPP(SEXP rX, double p, double a, double b) 
{
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  arma::mat cov = RobCovLib::LPDepthCovarianceEstimator(X, p, a, b);
  return wrap(cov);
}
