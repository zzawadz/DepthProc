#include <RcppArmadillo.h>
using namespace Rcpp;
#include "RobCovLib/Utils.h"

// [[Rcpp::export]]
SEXP runifsphereCPP(double n, double p) 
{  
  arma::mat X = Utils::runifsphere(n, p);
  return wrap(X);
}
