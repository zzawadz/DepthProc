#include "LocationEstimators.h"
#include "Depth.h"

namespace Location
{
	arma::rowvec WeightedLocationEstimator(const arma::mat& X, const arma::vec& w)
	{
		size_t n = X.n_rows;
		double sumW = arma::sum(w);
		arma::rowvec total_sum = arma::zeros<arma::rowvec>(X.n_cols);
		for(size_t i = 0; i < n; i++)
		{
			total_sum += X.row(i)*w(i);
		}
		return total_sum/sumW;
	}

	arma::rowvec LPDepthLocationEstimator(const arma::mat& X, const double &p, const double& a, const double& b)
	{
		arma::vec weights = Depth::LPDepth(X,p,a,b);
		return WeightedLocationEstimator(X, weights);
	}
}
