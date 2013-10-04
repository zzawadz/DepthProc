#include <RcppArmadillo.h>
using namespace Rcpp;
#include "RobCovLib/Depth.h"

// [[Rcpp::export]]
SEXP depthMahCPP(SEXP ru, SEXP rX) 
{
  Rcpp::NumericMatrix cu(ru);
  arma::mat u(cu.begin(), cu.nrow(), cu.ncol(), false);
  
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  
  arma::vec depth = Depth::MahalanobisDepth(u, X);
  return wrap(depth);
}
