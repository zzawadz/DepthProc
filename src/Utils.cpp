#include "Utils.h"
#include <limits>
using namespace Rcpp ;

namespace Utils
{
	arma::mat runifsphere(size_t n, size_t p)
	{
		//arma::mat X(n, p);
		NumericVector rx = rnorm(n*p);
    arma::mat X(rx.begin(), n, p, false);
    
    //X.randn();
		arma::vec norm = arma::sum(X % X, 1);
		norm = arma::sqrt(norm);
		X.each_col() /= norm;
		return X;
	}
  
////////////// seq for Armadillo - works as seq in R
arma::vec seq(const double& x, const double& y, const size_t& length)
{
  arma::vec result(length);
  double step = (y-x)/static_cast<double>(length-1);
  result(0) = x;
  for(size_t i = 1; i < length; i++) result(i) = result(i-1) + step;
  return(result);
}


 
}
