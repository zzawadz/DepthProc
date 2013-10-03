#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP timesTwo(SEXP ru, SEXP rX) 
{
  Rcpp::NumericMatrix cu(ru);
  arma::mat u(cu.begin(), cu.nrow(), cu.ncol(), false);
  
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  
  arma::vec depth = Location::MahalanobisDepth(X, w);
  return wrap(depth);
}
