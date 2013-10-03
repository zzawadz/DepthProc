#include "LocationEstimators.h"

namespace RobCovLib
{
	arma::mat WeightedCovarianceEstimator(const arma::mat& X, arma::rowvec locX, const arma::vec& w)
	{
		size_t n = X.n_rows;
		double sumW = arma::sum(w);
		arma::mat total_sum = arma::zeros(X.n_cols);

		arma::mat tmpX = X;
		tmpX.each_row() -= locX;

		arma::mat tmpXW = tmpX;
		tmpXW.each_col() %= w;

		arma::mat covMat = tmpXW * tmpX;

		return covMat;
	}

	arma::mat LPDepthCovarianceEstimator(const arma::mat& X, const double &p, const double& a, const double& b)
	{
		arma::vec weights = Depth::LPDepth(X, p, a, b);
		arma::rowvec locX = Location::WeightedLocationEstimator(X, weights);
		return WeightedCovarianceEstimator(X, locX, weights);
	}

}
