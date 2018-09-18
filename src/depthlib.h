#ifndef DEPTHLIB_INCLUDES
#define DEPTHLIB_INCLUDES

#include "RobCovLibConfig.h"
#include "armadillo"
#include <limits>
#include <math.h>
#include <algorithm>
#include <vector>
#include <RcppArmadillo.h>

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
  arma::vec ProjectionDepth(const arma::mat& X, size_t nproj, int threads);
  arma::vec ProjectionDepth(const arma::mat& X, const arma::mat& Y, size_t nproj, int threads);


  // Tukey depth
  arma::vec TukeyDepth(const arma::mat& X, const arma::mat& Y,bool exact, int threads);


  //Modified band depth
  arma::vec MBDepth(const arma::mat& X);
  arma::vec MBDepth(const arma::mat& X, const arma::mat& Y);

}

namespace MBD
{
  arma::vec depthForCol(arma::vec x);
  arma::vec depthFuncForRef(arma::vec X,arma::vec Y);
}

namespace Tukey
{
  double getDepths1(double m,const double j);
  double getHDEP(size_t NT, size_t N, double NUMH);
  arma::vec getALPHA(const arma::vec& X, const arma::vec& Y,const double& U,const double& V, const double& P, const double& P2,const double& EPS);
  double depthTukey2dExact(double U, double V,const arma::mat& m);
}

namespace RobCovLib
{
  // Covariance estimators
  arma::mat WeightedCovarianceEstimator(const arma::mat& X, arma::rowvec locX, const arma::vec& w);
  arma::mat LPDepthCovarianceEstimator(const arma::mat& X, const double &p, const double& a, const double& b);
}

namespace Utils
{
  arma::mat runifsphere(size_t n, size_t p);
  arma::vec seq(const double& x, const double& y, const size_t& length);
  arma::rowvec mean(const arma::mat& X, int threads);
  arma::mat cov(const arma::mat& X, int threads);
}

namespace LSD
{
  arma::vec sampleDepthContForMu(size_t d, double mu, const arma::vec& y, size_t m);
  arma::vec sampleMaxDepthForMu(double mu,const arma::vec& y, size_t d_min, size_t max_iter, double eps);
  arma::vec sampleMaxLocScaleDepth(arma::vec y, size_t max_iter, double eps, double p_length);
}

namespace Location
{
  //Location estimators
  arma::rowvec LPDepthLocationEstimator(const arma::mat& X, const double &p, const double& a, const double& b);
  arma::rowvec WeightedLocationEstimator(const arma::mat& X, const arma::vec& w);
}

// Based on Fortran code from depth package

namespace Tukey
{

double getDepths1(double m,const double j)
{
  if(m < j) return 0.0;
  if(j==1) return m;
  if(j==2) return (m*(m-1.0))/2.0;
  if(j==3) return (m*(m-1.0)*(m-2.0))/6.0;
  return 0.0;
}

double getHDEP(size_t NT, size_t N, double NUMH)
{
  //NUMS = NUMS+depths1(NT,1)*depths1(NN,2)+depths1(NT,2)*depths1(NN,1)+
  //  depths1(NT,3);
  //if(N >= 3) SDEP<-(NUMS+0.0)/(depths1(N,3))
  NUMH = NUMH+NT;
  double HDEP = NUMH/N;
  return HDEP;
}


arma::vec getALPHA(const arma::vec& X, const arma::vec& Y,const double& U,const double& V, const double& P, const double& P2,const double& EPS)
{

  //Rcpp::NumericVector cX(rX);
  //arma::vec X(cX.begin(), cX.length(), false);
  //Rcpp::NumericVector cY(rY);
  //arma::vec Y(cY.begin(), cY.length(), false);

  size_t N = X.n_elem;
  size_t NT = 0;

  arma::vec ALPHA(N);

  // temporary variables
  double DV;
  double XU;
  double YU;


  for(size_t i = 0; i < N; i++)
  {
    DV = sqrt(((X[i]-U)*(X[i]-U)+(Y[i]-V)*(Y[i]-V)));

    if (DV <= EPS) { NT++; }
    else
    {
      XU = (X[i]-U)/DV;
      YU = (Y[i]-V)/DV;

      if (fabs(XU) > fabs(YU))
      {
        if (X[i] >= U)
        {
          ALPHA[i-NT] = asin(YU);
          if(ALPHA[i-NT] < 0.0)
          {
            ALPHA[i-NT] = P2+ALPHA[i-NT];
          }
        }
        else
        {
          ALPHA[i-NT] = P-asin(YU);
        }
      }
      else
      {
        if (Y[i] >= V)
          ALPHA[i-NT] = acos(XU);
        else
          ALPHA[i-NT] = P2-acos(XU);
      }
      if (ALPHA[i-NT] >= P2-EPS) ALPHA[i-NT] = 0.0;
    }

  }


  ALPHA.resize(N-NT);
  ALPHA = arma::sort(ALPHA);
  return ALPHA;
}


double depthTukey2dExact(double U, double V,const arma::mat& m)
{
  //  Compute the halfspace depth of the point (u,v) for the pairs of points
  //  in the n by 2 matrix m.
  //Rcpp::NumericMatrix cm(rm);
  //arma::mat m(cm.begin(), cm.nrow(), cm.ncol(), false);
  // CONST
  const size_t N   = m.n_rows;
  const double P   = M_PI;
  const double P2  = P*2.0;
  const double EPS = 0.000001;

  // Var
  arma::colvec X = m.col(0);
  arma::colvec Y = m.col(1);
  double NUMH = 0.0;

  arma::vec ALPHA = getALPHA(X,Y,U,V,P,P2,EPS);
  size_t NN = ALPHA.n_rows;
  size_t NT = N - NN;


  if(NN<=1) return getHDEP(NT, N, NUMH);

  double ANGLE = ALPHA[0]-ALPHA[NN-1]+P2;
  for(size_t i =1; i < NN; i++)
  {
    ANGLE = std::max(ANGLE,ALPHA[i]-ALPHA[i-1]);
  }
  /*If NN end */
  if(ANGLE > (P+EPS)) return getHDEP(NT, N, NUMH);


  ANGLE = ALPHA[0];
  size_t NU = 0;
  for(size_t i = 0; i < NN; i++)
  {
    ALPHA[i] = ALPHA[i]-ANGLE;
    if(ALPHA[i]<(P-EPS)) NU++;
  }
  /*If NN end */
  if(NU >= NN) return getHDEP(NT, N, NUMH);


  size_t JA = 1;
  size_t JB = 1;
  double ALPHK = ALPHA[0];
  double BETAK = ALPHA[NU]-P;
  size_t NN2   = NN*2;
  //size_t NBAD  = 0;
  size_t I     = NU;
  size_t NF    = NN;

  double ADD;
  arma::vec FV(NN);

  for(size_t J = 0; J < NN2; J++)
  {
    ADD = ALPHK+EPS;


    if (ADD < BETAK)
    {
      NF++;


      if(JA < NN)
      {
        JA++;
        ALPHK = ALPHA[JA-1];
      }
      else ALPHK = P2+1.0;


    }
    else
    {
      I++;
      if(I > NN)
      {
        I = 1;
        NF = NF-NN;
      }
      FV[I-1] = NF;

      if(JB < NN)
      {
        JB++;
        if(JB+NU <= NN)
          BETAK = ALPHA[JB+NU-1]-P;
        else
          BETAK = ALPHA[JB+NU-NN-1]+P;
      }
      else
        BETAK = P2+1.0;
    }
  }

  size_t GI = 0;
  size_t KI = 0;
  double AEPS;
  JA = 1;

  ANGLE = ALPHA[0];
  size_t dif = NN-FV[0];
  NUMH = (FV[0] < dif)?FV[0] : dif;


  for(size_t I = 1; I < NN; I++)
  {
    AEPS = ANGLE+EPS;
    if(ALPHA[I] <= AEPS)
    {
      JA++;
    }
    else
    {
      GI = GI+JA;
      JA = 1;
      ANGLE = ALPHA[I];
    }
    KI   = FV[I]-GI;

    NUMH = (NUMH < FV[I] - GI)? NUMH : FV[I] - GI;
    NUMH = (NUMH < NN-KI)? NUMH : NN-KI;

  }

  return getHDEP(NT, N, NUMH);
}

}


namespace Utils
{
arma::mat runifsphere(size_t n, size_t p)
{
  //arma::mat X(n, p);
  Rcpp::NumericVector rx = Rcpp::rnorm(n*p);
  arma::mat X(rx.begin(), n, p, false);

  //X.randn();
  arma::vec norm = arma::sum(X % X, 1);
  norm = arma::sqrt(norm);
  X.each_col() /= norm;
  return X;
}

////////////// seq for Armadillo - works as seq in R
arma::vec seq(const double& x, const double& y, const size_t& length)
{
  arma::vec result(length);
  double step = (y-x)/static_cast<double>(length-1);
  result(0) = x;
  for(size_t i = 1; i < length; i++) result(i) = result(i-1) + step;
  return(result);
}


///////////////////////////////////////////////////////////////////////////////
arma::rowvec mean(const arma::mat& X, int threads)
{
  size_t d = X.n_cols;
  size_t n = X.n_rows;
  double n1= n;
  size_t i,k;
  arma::rowvec meanr(d);
  double tmp;

  if(threads < 1) threads = omp_get_max_threads();

  for(i = 0; i < d; i++)
  {
    tmp = 0;
#pragma omp parallel for shared(X,d,n,i) private(k) reduction(+:tmp) num_threads(threads)
    for(k = 0; k < n; k++)
    {
      tmp += X.at(k,i);
    }
    meanr.at(i) = tmp/n1;
  }

  return meanr;
}

///////////////////////////////////////////////////////////////////////////////

arma::mat cov(const arma::mat& X, int threads)
{
  size_t d = X.n_cols;
  size_t n = X.n_rows;
  double n1= n-1;
  size_t i,j,k;
  arma::mat cov(d,d);

  arma::rowvec meanr = Utils::mean(X, threads);

  double m1,m2;
  double tmp;

  if(threads < 1) threads = omp_get_max_threads();

#pragma omp parallel for shared(X,n,n1,meanr) private(i,j, k, m1, m2, tmp) num_threads(threads)
  for(i = 0; i < d; i++)
  {
    m1 = meanr.at(i);

    for(j = i; j < d; j++)
    {
      m2 = meanr.at(j);
      tmp = 0;

      for(k = 0; k < n; k++)
      {
        tmp += (X.at(k,i) - m1)*(X.at(k,j) - m2);
      }
      tmp = tmp/n1;
      cov.at(i,j) = tmp;
      cov.at(j,i) = tmp;
    }
  }

  return cov;
}

}


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
  arma::vec weights = Depth::LPDepth(X,p,a,b,-1);
  return WeightedLocationEstimator(X, weights);
}
}

namespace RobCovLib
{
arma::mat WeightedCovarianceEstimator(const arma::mat& X, arma::rowvec locX, const arma::vec& w)
{
  //size_t n = X.n_rows;
  double sumW = arma::sum(w);
  arma::mat total_sum = arma::zeros(X.n_cols);

  arma::mat tmpX = X;
  tmpX.each_row() -= locX;

  arma::mat tmpXW = tmpX;
  tmpXW.each_col() %= w;

  arma::mat covMat = tmpXW.t() * tmpX;

  return covMat/sumW;
}

arma::mat LPDepthCovarianceEstimator(const arma::mat& X, const double &p, const double& a, const double& b)
{
  arma::vec weights = Depth::LPDepth(X, p, a, b,-1);
  arma::rowvec locX = Location::WeightedLocationEstimator(X, weights);
  return WeightedCovarianceEstimator(X, locX, weights);
}

}


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
      tmp_sum = std::pow(tmp_sum, 1.0 / p);
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


namespace LSD
{

arma::vec sampleDepthContForMu(size_t d, double mu, const arma::vec& y, size_t m)
{
  /* Returns vector with lbound, ubound, tbound, case, M */
  size_t n = y.n_elem;


  bool case_ = false;
  bool tbound = false;

  double lbound = std::numeric_limits<int>::min();
  double ubound = std::numeric_limits<int>::max();
  double tmp;

  if(y[m]>mu) case_  = false;
  if(y[m]==mu) case_ = true;


  if( d > 0 && (d<=m && d<=n-m-case_))
  {
    for(size_t i = m-d; i<m; i++)
    {
      tmp = (mu-y[i])*(y[d+i]-mu);
      tmp = sqrt(tmp);
      if(tmp > lbound) lbound = tmp;
    }

    for(size_t i = 0; i < d; i++)
    {
      tmp = (mu-y[i])*(y[n-d+i]-mu);
      tmp = sqrt(tmp);
      if(tmp < ubound) ubound = tmp;
    }

    if(lbound <= ubound)  tbound = true;
    else{tbound = false;}
  }
  else{
    lbound = 1;
    ubound = 0;
    tbound = false;
  }

  arma::vec result(5);
  result[0] = lbound;
  result[1] = ubound;
  result[2] = tbound;
  result[3] = case_;
  result[4] = m;
  return result;
}



arma::vec sampleMaxDepthForMu(double mu,const arma::vec& y, size_t d_min, size_t max_iter, double eps)
{
  size_t n = y.n_elem;
  size_t m = 0;
  for(size_t i = 0; i < n; i++) if(y[i]<mu) m++;

  size_t d = y[m]>mu ? std::min(m,n-m) : std::min(m,n-m-1);

  arma::vec cont = sampleDepthContForMu(d, mu, y, m);
  double difbound = cont[1]-cont[0]; //difbound<-cont["ubound"]-cont["lbound"];


  size_t iter = 0;

  if(fabs(difbound)>eps)
  {

    iter++;
    size_t d_up = d;
    size_t d_low = d_min;

    while(iter < max_iter && fabs(difbound) > eps && d_up-1>d_low)
    {

      iter++;
      (difbound < -eps ? d_up : d_low) = d;
      d = rint(static_cast<double>(d_up+d_low)/2);
      cont = sampleDepthContForMu(d, mu, y, m);
      difbound = cont[1]-cont[0];
    }
  }

  if(difbound< -eps)
  {
    d = d-1;
    cont = sampleDepthContForMu(d, mu, y, m);
    difbound = cont[1]-cont[0];
  }

  arma::vec result(4);
  result[0] = d;
  result[1] = (cont[1]+cont[0])/2; //(cont["ubound"]+cont["lbound"])/2
  result[2] = iter; //
  result[3] = difbound;

  return result;
}

/*
 * Calculate maximum location scale depth from given sample
 */
arma::vec sampleMaxLocScaleDepth(arma::vec y, size_t max_iter, double eps, double p_length)
{
  // Calculates the maximum sample location-scale depth
  // for the data set y
  // Uses function sample.depth.cont.for.mu and sample.max.depth.for.mu
  // p.length is the maximum length of the precision step at the end
  y = arma::sort(y);

  size_t N = y.n_elem;

  size_t d_min = floor(static_cast<double>(N)/3);
  size_t n_mid = round(static_cast<double>(N)/2);

  arma::vec res = LSD::sampleMaxDepthForMu(y[n_mid-1], y, d_min, max_iter, eps);

  size_t d = res[0]; // "d"
  double s = res[1]; //"sigma"
  double difb = res[3]; //difbound;


  size_t all_iterations = res[2];

  /// temp variables
  size_t n_mid_low;
  size_t n_mid_up;
  arma::vec res_low;
  arma::vec res_up;
  size_t d_low;
  size_t d_up;
  double dec;
  size_t n_up = 0;
  size_t n_low = 0;

  if(d<N/2)
  {
    size_t i = 1;
    n_up = ceil(static_cast<double>(N)*2/3);
    n_low = floor(static_cast<double>(N)/3);

    dec = 1;
    while(i < max_iter && (n_up-1>n_low && dec>0))
    {
      i++;
      n_mid_low = ceil(static_cast<double>(n_mid+n_low)/2);
      n_mid_up = floor(static_cast<double>(n_mid+n_up)/2);
      res_low  = LSD::sampleMaxDepthForMu(y[n_mid_low-1], y, d_min, max_iter, eps);
      res_up   = LSD::sampleMaxDepthForMu(y[n_mid_up-1], y, d_min, max_iter, eps);

      all_iterations += res_low[2];
      all_iterations += res_up[2];

      d_low = res_low[0];
      d_up = res_up[0];

      if(d_low > d )
      {

        d = d_low;
        s = res_low[0];
        difb = res_low[3];
        n_up  = n_mid;
        n_mid = n_mid_low;

      }
      else
      {
        if(d_up > d){

          d = d_up;
          s = res_up[1];
          difb = res_up[3];
          n_low = n_mid;
          n_mid = n_mid_up;
        }
        else{
          if(d_low < d || d_up < d)
          {
            if(d_low < d)
            {

              n_low = n_mid_low;
            }
            if(d_up < d)
            {

              n_up = n_mid_up;
            }
          }
          else
          {
            dec = 0;
          }
        }
      }

    }
  }
  // Precision step
  size_t length = std::max(p_length,static_cast<double>(2*(n_up-n_low+1)));

  arma::vec mu = Utils::seq(y[n_low-1],y[n_up-1],length);
  arma::mat res_matrix(length,4);


  for(size_t i = 0; i < length; i++)
  {
    res_matrix.row(i) = LSD::sampleMaxDepthForMu(mu[i], y, d_min, max_iter, eps).t();
  }
  d = max(res_matrix.col(0));

  arma::uvec tmp_res = arma::find(res_matrix.col(0)==d);
  res_matrix = res_matrix.rows(tmp_res);

  size_t tmp_n = 0;
  mu = mu(tmp_res);
  if(mu.n_elem > 1)
  {
    tmp_n = rint(static_cast<double>(mu.n_elem)/2)-1;
  }

  s = res_matrix.at(tmp_n,1);

  arma::vec result(3);
  result[0] = d;
  result[1] = mu[tmp_n];
  result[2] = s;

  return result;
}

}


#endif

