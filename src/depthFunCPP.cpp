#include <RcppArmadillo.h>
using namespace Rcpp;
#include "Depth.h"

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
    cov = arma::cov(X);
  }
  else
  {
    Rcpp::NumericMatrix ccov(rcov);
    cov = arma::mat(ccov.begin(), ccov.nrow(), ccov.ncol(), true);
  }
  
  arma::rowvec mean;
  if(Rf_isNull(rmean))
  {
    mean = arma::mean(X);
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
SEXP depthLocalProjCPP(SEXP rX, SEXP ru, double nproj, int threads)
{
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);

  // u is the single query point the sample is symmetrised about, passed as a
  // 1 x d matrix by the caller
  Rcpp::NumericMatrix cu(ru);
  arma::mat um(cu.begin(), cu.nrow(), cu.ncol(), false);
  arma::rowvec u = um.row(0);

  arma::vec depth = Depth::LocalProjectionDepth(X, u, nproj, threads);
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
