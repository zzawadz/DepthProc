#ifndef DEPTH_INCLUDES
#define DEPTH_INCLUDES

#include "RobCovLibConfig.h"
#include "Utils.h"
#include "TukeyDepth.h"
#include "armadillo"
#include <omp.h>

namespace Depth
{

 // LPDepth
 arma::vec LPDepth(const arma::mat& X, const double &p, const double& a, const double& b, int threads);
 arma::vec LPDepth(const arma::mat& X, const arma::mat& Y, const double &p, const double& a, const double& b, int threads);


	// Mahalanobis Depth
 arma::vec MahalanobisDepth(const arma::mat& X, int threads);
 arma::vec MahalanobisDepth(const arma::mat& X, const arma::mat& Y, int threads);
 arma::vec MahalanobisDepth(const arma::mat& X, const arma::mat& Y, const arma::mat& cov, int threads);
 arma::vec MahalanobisDepth(const arma::mat& X, const arma::mat& Y, const arma::rowvec& mean, int threads);
 arma::vec MahalanobisDepth(const arma::mat& X, const arma::mat& Y, const arma::mat& cov, const arma::rowvec& mean, int threads);

	// Projection Depth
	arma::vec ProjectionDepth(const arma::mat& X, size_t nproj, double seed, int threads);
	arma::vec ProjectionDepth(const arma::mat& X, const arma::mat& Y, size_t nproj, double seed, int threads);

  arma::vec TukeyDepth(const arma::mat& X, const arma::mat& Y,bool exact, int threads);
}

#endif
