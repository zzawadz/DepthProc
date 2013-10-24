#include <RcppArmadillo.h>
using namespace Rcpp;
#include "RobCovLib/Utils.h"

// [[Rcpp::export]]
SEXP runifsphereCPP(double n, double p, int seed) 
{  
  arma::mat X = Utils::runifsphere(n, p, seed);
  return wrap(X);
}
