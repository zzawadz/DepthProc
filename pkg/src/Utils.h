#ifndef UTILS_INCLUDES
#define UTILS_INCLUDES

#include "RobCovLibConfig.h"
#include "armadillo"
#include <omp.h>

namespace Utils
{
 arma::mat runifsphere(size_t n, size_t p, int seed);
 arma::rowvec mean(const arma::mat& X, int threads);
 arma::mat cov(const arma::mat& X, int threads);
}

#endif
