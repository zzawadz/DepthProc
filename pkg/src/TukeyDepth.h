#ifndef TUKEY_INCLUDES
#define TUKEY_INCLUDES

#include "RobCovLibConfig.h"
#include "armadillo"
#include <math.h>
#include <algorithm>

namespace Tukey
{
  
  double getDepths1(double m,const double j);
  double getHDEP(size_t NT, size_t N, double NUMH);
  arma::vec getALPHA(const arma::vec& X, const arma::vec& Y,const double& U,const double& V, const double& P, const double& P2,const double& EPS);
  double depthTukey2dExact(double U, double V,const arma::mat& m);
  
}



#endif
