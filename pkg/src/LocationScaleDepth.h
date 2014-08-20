#ifndef LSD_INCLUDES
#define LSD_INCLUDES

#include "RobCovLibConfig.h"
#include "armadillo"
#include <limits>
#include <math.h>
#include <algorithm>

namespace LSD
{
  arma::vec sampleDepthContForMu(size_t d, double mu, const arma::vec& y, size_t m);
  arma::vec sampleMaxDepthForMu(double mu,const arma::vec& y, size_t d_min, size_t max_iter, double eps);
  arma::vec sampleMaxLocScaleDepth(arma::vec y, size_t max_iter, double eps, double p_length);
}




#endif