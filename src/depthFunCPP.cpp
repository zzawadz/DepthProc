#include <RcppArmadillo.h>
using namespace Rcpp;
#include "Depth.h"

namespace
{
  // Defaulting an absent estimate is all the four branches this replaces ever
  // expressed, and Depth::MahalanobisDepth's own overloads already default them
  // exactly this way. threads == -2 is the package's own parallel estimator.
  arma::mat defaultCov(const arma::mat& X, int threads)
  {
    return threads == -2 ? Utils::cov(X, threads) : arma::cov(X);
  }

  arma::rowvec defaultMean(const arma::mat& X, int threads)
  {
    return threads == -2 ? Utils::mean(X, threads) : arma::mean(X);
  }
}

// [[Rcpp::export]]
SEXP depthMahCPP(SEXP ru, SEXP rX, SEXP rcov, SEXP rmean, int threads) 
{
  Rcpp::NumericMatrix cu(ru);
  arma::mat u(cu.begin(), cu.nrow(), cu.ncol(), false);
  
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  arma::mat cov;
  if(Rf_isNull(rcov))
  {
    cov = defaultCov(X, threads);
  }
  else
  {
    Rcpp::NumericMatrix ccov(rcov);
    cov = arma::mat(ccov.begin(), ccov.nrow(), ccov.ncol(), true);
  }
  
  arma::rowvec mean;
  if(Rf_isNull(rmean))
  {
    mean = defaultMean(X, threads);
  }
  else
  {
    Rcpp::NumericMatrix cmean(rmean);
    mean = arma::rowvec(cmean.begin(), cmean.ncol(), true);
  }
  
  arma::vec depth = Depth::MahalanobisDepth(u, X, cov, mean, threads);
  
  return wrap(depth);
}

// [[Rcpp::export]]
SEXP depthProjCPP(SEXP ru, SEXP rX, double nproj, int threads) 
{
  Rcpp::NumericMatrix cu(ru);
  arma::mat u(cu.begin(), cu.nrow(), cu.ncol(), false);
  
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  
  arma::vec depth = Depth::ProjectionDepth(u, X, nproj, threads);
  return wrap(depth);
}

// [[Rcpp::export]]
SEXP depthLPCPP(SEXP ru, SEXP rX, double p, double a, double b, int threads) 
{
  Rcpp::NumericMatrix cu(ru);
  arma::mat u(cu.begin(), cu.nrow(), cu.ncol(), false);
  
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  
  arma::vec depth = Depth::LPDepth(u, X, p, a, b, threads);
  return wrap(depth);
}

// [[Rcpp::export]]
SEXP depthTukeyCPP(SEXP ru, SEXP rX, bool exact, int threads) 
{
  Rcpp::NumericMatrix cu(ru);
  arma::mat u(cu.begin(), cu.nrow(), cu.ncol(), false);
  
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  
  arma::vec depth = Depth::TukeyDepth(u, X, exact, threads);
  return wrap(depth);
}

// [[Rcpp::export]]
SEXP modBandDepthRef(SEXP rX, SEXP rxRef) 
{
  Rcpp::NumericMatrix cxRef(rxRef);
  arma::mat xRef(cxRef.begin(), cxRef.nrow(), cxRef.ncol(), false);
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  arma::vec depth(X.n_rows); 
  
  depth = Depth::MBDepth(X,xRef);
  
  return wrap(depth);
}

// [[Rcpp::export]]
SEXP modBandDepth(SEXP rX) 
{
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  arma::vec depth(X.n_rows);
  
  depth = Depth::MBDepth(X);
  
  return wrap(depth);
}
