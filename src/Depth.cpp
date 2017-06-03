#include "Depth.h"

namespace Depth
{

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

		//const double pr = 1/p;

		arma::vec depth(n_x);
    size_t k,i;

    if(threads < 1) threads = omp_get_max_threads();

    #pragma omp parallel for shared(depth, X, Y, n_x, n_y, d, p, b, a) private(k) num_threads(threads)
		for(k = 0; k< n_x; k++)
		{
      arma::rowvec tmp = arma::zeros<arma::rowvec>(d);
  	  double sum_res = 0;
		  double tmp_sum;

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
	arma::vec ProjectionDepth(const arma::mat& X, size_t nproj, int threads)
	{
		return ProjectionDepth(X, X, nproj, threads);
	}

	arma::vec ProjectionDepth(const arma::mat& X, const arma::mat& Y, size_t nproj, int threads)
	{
    if(threads < 1) threads = omp_get_max_threads();

		size_t nx = X.n_rows;
		size_t ny = Y.n_rows;
		size_t d  = Y.n_cols;

		arma::mat directions = Utils::runifsphere(nproj, d);
		directions = directions.t();

		arma::vec depth(nx);

		arma::vec tmpProj(ny);
		arma::rowvec medians(nproj);
		arma::rowvec mads(nproj);

    size_t i;

    #pragma omp parallel for shared(nproj,Y,medians,mads,directions) private(tmpProj,i) num_threads(threads)
		for(i = 0; i < nproj; i++)
		{
			tmpProj = Y * directions.col(i);
			medians(i) = arma::median(tmpProj);
			mads(i) = arma::median(arma::abs(tmpProj - medians(i)));
		}

		arma::rowvec tmpX(nproj);

    #pragma omp parallel for shared(X,directions,medians,mads,nx,depth) private(i, tmpX) num_threads(threads)
		for(i = 0; i < nx; i++)
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


  // Tukey Depth - Exact algorithm
  arma::vec TukeyDepth(const arma::mat& X, const arma::mat& Y,bool exact, int threads)
  {
    if(threads < 1) threads = omp_get_max_threads();

    size_t n = X.n_rows;
    arma::vec depth(n);
    size_t i;
    #pragma omp parallel for shared(X,Y,depth,n) private(i) num_threads(threads)
  	for(i = 0; i < n; i++)
		{
      depth[i] = Tukey::depthTukey2dExact(X.at(i,0),X.at(i,1),Y);
		}
    return depth;
  }

  // MBDepth
  arma::vec MBDepth(const arma::mat& X)
  {
  //Rcpp::NumericMatrix cX(rX);
  //arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);

    arma::vec depth(X.n_rows); depth.zeros();

    size_t d = X.n_cols;
    for(size_t i = 0; i < d; i++)
    {
      depth += MBD::depthForCol(X.col(i));
    }

    depth = depth / (d * Rf_choose(X.n_rows, 2));

    return depth;
  }


  arma::vec MBDepth(const arma::mat& X, const arma::mat& Y)
  {
    //Rcpp::NumericMatrix cY(rY);
    //arma::mat Y(cY.begin(), cY.nrow(), cY.ncol(), false);
    //Rcpp::NumericMatrix cX(rX);
    //arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);

    arma::vec depth(X.n_rows); depth.zeros();

    size_t d = X.n_cols;
    for(size_t i = 0; i < d; i++)
    {
      depth += MBD::depthFuncForRef(X.col(i), Y.col(i));
    }

    depth = depth / (d * Rf_choose(Y.n_rows, 2));

    return depth;
  }


}


 /// Utils functions for MBDepth
namespace MBD
{
  arma::vec depthForCol(arma::vec x)
  {
    arma::uvec idx = sort_index( x );
    arma::vec sorted = x(idx);
    size_t n = idx.n_elem;
    arma::uvec equal(n); equal.ones();
    arma::uvec lowidx(n);



    size_t pos;
    size_t ti;
    size_t eq;


    for(size_t i = 0; i < n; i++)
    {

      pos = idx(i);
      lowidx(pos) = i;
        eq = 0;

        if(i > 0)
        {
          ti = i - 1;

          while(sorted(i) == sorted(ti))
          {
            lowidx(pos) -= 1;
            eq++;
            if(ti == 0) break;
            ti--;
          }
        }

        ti = i + 1;
        while((ti < n) && (sorted(i) == sorted(ti)))
        {
            equal(pos) +=  1;
            ti++;
        }
        equal(pos) = equal(pos) + eq;

    }


    equal = equal + lowidx;
    arma::vec depth(n);

    for(size_t i = 0; i < n; i++)
    {
      double multiplicity = equal(i) - lowidx(i);
      depth(i) = lowidx(i) * (n - (equal(i))) + multiplicity * (n - equal(i) + lowidx(i));
      depth(i) += Rf_choose(multiplicity, 2);
    }

    return depth;
  }

  ///////////////////////////

  arma::vec depthFuncForRef(arma::vec X,arma::vec Y)
  {
    size_t n = X.n_elem;
    size_t n0 = Y.n_elem;
    arma::vec depth(n);

    Y = arma::sort(Y);

    double index1;
    double index2;
    double tval;
    size_t j;

    for(size_t i = 0; i < n; i++)
    {
            index1 = 0;
            index2 = 0;
            tval = X[i];
            j = 0;
            while(j < n0 && tval >= Y[j])
            {
              index2++;
              if(tval > Y[j]) index1++;
              j++;
            }

            double multiplicity = index2 - index1;
            depth[i] =  (index1 + multiplicity) * (n0 - index1 - multiplicity) +
            multiplicity * (index1 + (multiplicity - 1)/2);
    }

      return depth;
  }

}

