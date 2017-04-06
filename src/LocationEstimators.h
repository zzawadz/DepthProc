#ifndef LOCATION_INCLUDES
#define LOCATION_INCLUDES

#include "RobCovLibConfig.h"
#include "armadillo"
#include "Depth.h"

namespace Location
{
	//Location estimators
  arma::rowvec LPDepthLocationEstimator(const arma::mat& X, const double &p, const double& a, const double& b);
  arma::rowvec WeightedLocationEstimator(const arma::mat& X, const arma::vec& w);

}

#endif
