#ifndef ROBCOVLIB_INCLUDES
#define ROBCOVLIB_INCLUDES

#include "RobCovLibConfig.h"
#include "armadillo"


namespace RobCovLib
{
	// Covariance estimators
	__declspec(dllexport) arma::mat WeightedCovarianceEstimator(const arma::mat& X, arma::rowvec locX, const arma::vec& w);
	__declspec(dllexport) arma::mat LPDepthCovarianceEstimator(const arma::mat& X, const double &p, const double& a, const double& b);
}


#endif