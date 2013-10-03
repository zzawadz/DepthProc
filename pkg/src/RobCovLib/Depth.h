#ifndef DEPTH_INCLUDES
#define DEPTH_INCLUDES

#include "armadillo"

namespace Depth
{
	__declspec(dllexport) arma::vec LPDepth(const arma::mat& X, const double &p, const double& a, const double& b);
	__declspec(dllexport) arma::vec LPDepth(const arma::mat& X, const arma::mat& Y, const double &p, const double& a, const double& b);
}

#endif