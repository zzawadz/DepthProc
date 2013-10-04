#include "Depth.h"
#include <stdio.h>
#include <iostream>

namespace Depth{

	// LPDEPTH

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

	// Mahalanobis Depth
	arma::vec MahalanobisDepth(const arma::mat& X)
	{
		return(MahalanobisDepth(X,X));
	}
	arma::vec MahalanobisDepth(const arma::mat& X, const arma::mat& Y)
	{
		size_t n = X.n_rows;
		arma::mat covY = arma::cov(Y);
		covY = covY.i();
		arma::rowvec mean = arma::mean(Y);
		
		arma::vec depth(n);

		arma::rowvec tmpX;
		double dist;

		for(size_t i = 0; i < n; i++)
		{
			tmpX = X.row(i) - mean;
			tmpX = tmpX * covY *tmpX.t();
			dist = 1.0/(1.0 + tmpX(0));
			depth(i) = dist;
		}
		return(depth);
	}

 }