#include "Depth.h"
#include "Utils.h"
#include <stdio.h>
#include <iostream>

namespace Depth{

	// LPDEPTH

	arma::vec LPDepth(const arma::mat& X, const double &p, const double& a, const double& b, int threads)
  {
		return LPDepth(X, X, p, a, b, threads);
	}

	arma::vec LPDepth(const arma::mat& X, const arma::mat& Y, const double &p, const double& a, const double& b, int threads)
	{
		size_t d = Y.n_cols;
		size_t n_y = Y.n_rows;
		size_t n_x = X.n_rows;

		const double pr = 1/p;

		arma::vec depth(n_x);

		arma::rowvec tmp = arma::zeros<arma::rowvec>(d);
		double sum_res = 0;
		double tmp_sum;
    size_t k,i;
    
    if(threads < 1) threads = omp_get_max_threads();
    
    #pragma omp parallel for shared(X, n_x, n_y, p, b, a) private(k, i, sum_res, tmp, tmp_sum) num_threads(threads)
		for(k = 0; k< n_x; k++)
		{
			sum_res = 0;
			for(i = 0; i < n_y; i++)
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
	arma::vec MahalanobisDepth(const arma::mat& X, int threads)
	{
		return(MahalanobisDepth(X,X,threads));
	}
	arma::vec MahalanobisDepth(const arma::mat& X, const arma::mat& Y, int threads)
	{
		arma::mat cov;
		arma::rowvec mean;
		
    // if threads == -2 uses paraller function to compute 
    // covariance matrix and mean vector
    if(threads == -2)
    {
      mean = Utils::mean(Y, threads);
      cov = Utils::cov(Y, threads);
    } else
    {
       cov = arma::cov(Y);
       mean = arma::mean(Y);
    }
    
		return(MahalanobisDepth(X,Y,cov,mean,threads));
	}

  
 arma::vec MahalanobisDepth(const arma::mat& X, const arma::mat& Y, const arma::mat& cov, int threads)
 {
    arma::rowvec mean;
    if(threads == -2)
    {
      mean = Utils::mean(Y, threads);
    }
    else {
      mean = arma::mean(Y);
    }
    return(MahalanobisDepth(X,Y,cov,mean,threads));
 }
 
 arma::vec MahalanobisDepth(const arma::mat& X, const arma::mat& Y, const arma::rowvec& mean, int threads)
 {
    arma::mat cov;
    if(threads == -2)
    {
      cov = Utils::cov(Y, threads);
    } else
    {
       cov = arma::cov(Y);
    }
    return(MahalanobisDepth(X,Y,cov,mean,threads));
 }
 
 arma::vec MahalanobisDepth(const arma::mat& X, const arma::mat& Y, const arma::mat& cov, const arma::rowvec& mean, int threads)
 {
    size_t n = X.n_rows;
    arma::vec depth(n);
    arma::mat covY = cov.i();
    
  	arma::rowvec tmpX;
		double dist;
    size_t i;
    
    if(threads < 1) threads = omp_get_max_threads();
    
    #pragma omp parallel for shared(X,n,mean, covY) private(i, tmpX, dist) num_threads(threads)
		for(i = 0; i < n; i++)
		{
			tmpX = X.row(i) - mean;
			tmpX = tmpX * covY *tmpX.t();
			dist = 1.0/(1.0 + tmpX(0));
			depth(i) = dist;
		}
		return(depth);
 }



	// Projection Depth
	arma::vec ProjectionDepth(const arma::mat& X, size_t nproj, double seed)
	{
		return ProjectionDepth(X, X, nproj, seed);
	}

	arma::vec ProjectionDepth(const arma::mat& X, const arma::mat& Y, size_t nproj, double seed)
	{
		size_t nx = X.n_rows;
		size_t ny = Y.n_rows;
		size_t d  = Y.n_cols;

		arma::mat directions = Utils::runifsphere(nproj, d, seed);
		directions = directions.t();

		arma::vec depth(nx);

		arma::vec tmpProj(ny);
		arma::rowvec medians(nproj);
		arma::rowvec mads(nproj);

		for(size_t i = 0; i < nproj; i++)
		{
			tmpProj = Y * directions.col(i);
			medians(i) = arma::median(tmpProj);
			mads(i) = arma::median(arma::abs(tmpProj - medians(i)));
		}

		arma::rowvec tmpX(nproj);

		for(size_t i = 0; i < nx; i++)
		{
			tmpX = X.row(i) * directions;
			tmpX -= medians;
			tmpX /= mads;
			tmpX = arma::abs(tmpX);
			depth(i) = arma::max(tmpX);
		}

		depth = 1/(1+depth);
		
		return depth;
	}
  
  
  /// Under development!!!

  
  
  
  
  
 }
 