#include "Depth.h"
#include <stdio.h>


namespace Depth{

	arma::vec LPDepth(const arma::mat& X, const double &p, const double& a, const double& b)
	{
		return LPDepth(X,X,p,a,b);
	}

	arma::vec LPDepth(const arma::mat& X, const arma::mat& Y, const double &p, const double& a, const double& b)
	{
		size_t d = Y.n_cols;
		size_t n_y = Y.n_rows;
		size_t n_x = X.n_rows;

		const double pr = 1/p;

		arma::vec depth(n_x);

		arma::rowvec tmp = arma::zeros<arma::rowvec>(d);
		double sum_res = 0;
		double tmp_sum;


		for(size_t k = 0; k< n_x; k++)
		{
			sum_res = 0;
			for(size_t i = 0; i < n_y; i++)
			{
				tmp = X.row(k) - Y.row(i);
				tmp = arma::abs(tmp);
				tmp = arma::pow(tmp, p);
				tmp_sum = arma::sum(tmp);
				tmp_sum = a*tmp_sum + b;
				sum_res = sum_res + tmp_sum;
			}
			depth(k) = 1/(1 + sum_res/static_cast<double>(n_y));
		}
		return depth;
	}
 }