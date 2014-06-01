#include "Utils.h"
#include <limits>

namespace Utils
{
	arma::mat runifsphere(size_t n, size_t p, int seed = -1)
	{
		if(seed >= 0) std::srand(seed);

		arma::mat X(n, p);
		X.randn();
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


///////////////////////////////////////////////////////////////////////////////
  arma::rowvec mean(const arma::mat& X, int threads)
  {
    size_t d = X.n_cols;
    size_t n = X.n_rows;
    double n1= n;
    size_t i,k;
    arma::rowvec meanr(d);
    double tmp;
    
    if(threads < 1) threads = omp_get_max_threads();
    
    for(i = 0; i < d; i++)
    {
        tmp = 0;
        #pragma omp parallel for shared(X,d,n,i) private(k) reduction(+:tmp) num_threads(threads)
        for(k = 0; k < n; k++)
        {
          tmp += X.at(k,i);
        }
        meanr.at(i) = tmp/n1;
    }
    
    return meanr;
  }

///////////////////////////////////////////////////////////////////////////////

  arma::mat cov(const arma::mat& X, int threads)
  {
    size_t d = X.n_cols;
    size_t n = X.n_rows;
    double n1= n-1;
    size_t i,j,k;
    arma::mat cov(d,d);
    
    arma::rowvec meanr = Utils::mean(X, threads);
    
    double m1,m2;
    double tmp;
    
    if(threads < 1) threads = omp_get_max_threads();
    
    #pragma omp parallel for shared(X,n,n1,meanr) private(i,j, k, m1, m2, tmp) num_threads(threads)
    for(i = 0; i < d; i++)
    {
      m1 = meanr.at(i);
      
      for(j = i; j < d; j++)
      {
        m2 = meanr.at(j);
        tmp = 0;
        
        for(k = 0; k < n; k++)
        {
          tmp += (X.at(k,i) - m1)*(X.at(k,j) - m2);
        }
        tmp = tmp/n1;
        cov.at(i,j) = tmp;
        cov.at(j,i) = tmp;
      }
    }
    
    return cov;
  }
 
}
