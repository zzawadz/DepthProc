#include <RcppArmadillo.h>
using namespace Rcpp;
#include "RobCovLib.h"

// [[Rcpp::export]]
SEXP CovLPCPP(SEXP X, double p, double a, double b) 
{
  Rcpp::NumericMatrix cX(X);
  arma::mat aX(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  arma::mat cov = RobCovLib::LPDepthCovarianceEstimator(aX, p, a, b);
  return wrap(cov);
}
