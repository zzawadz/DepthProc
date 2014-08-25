#include <RcppArmadillo.h>
using namespace Rcpp;
#include "Depth.h"

// [[Rcpp::export]]
SEXP depthMahCPP(SEXP ru, SEXP rX, SEXP rcov, SEXP rmean, int threads) 
{
  Rboolean (Rf_isNull)(SEXP s);
  
  Rcpp::NumericMatrix cu(ru);
  arma::mat u(cu.begin(), cu.nrow(), cu.ncol(), false);
  
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  arma::vec depth;
  
  
  // Cov and mean need to be computed
  if(Rf_isNull(rcov) && Rf_isNull(rmean))
  {
    depth = Depth::MahalanobisDepth(u, X, threads);
  }
  
  // Cov passed, mean need to be computed
  if(!Rf_isNull(rcov) && Rf_isNull(rmean))
  {
    Rcpp::NumericMatrix ccov(rcov);
    arma::mat cov(ccov.begin(), ccov.nrow(), ccov.ncol(), false);
    
    depth = Depth::MahalanobisDepth(u, X, cov, threads);
  }
  
  // Cov need to be computed, mean passed
  if(Rf_isNull(rcov) && !Rf_isNull(rmean))
  {
    Rcpp::NumericMatrix cmean(rmean);
    arma::rowvec mean(cmean.begin(), cmean.ncol(), false);
    
    depth = Depth::MahalanobisDepth(u, X, mean, threads);
  }
  
  // Cov need to be computed, mean passed
  if(!Rf_isNull(rcov) && !Rf_isNull(rmean))
  {
    Rcpp::NumericMatrix ccov(rcov);
    arma::mat cov(ccov.begin(), ccov.nrow(), ccov.ncol(), false);
    Rcpp::NumericMatrix cmean(rmean);
    arma::rowvec mean(cmean.begin(), cmean.ncol(), false);
    
    depth = Depth::MahalanobisDepth(u, X, cov, mean, threads);
  }
  
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


