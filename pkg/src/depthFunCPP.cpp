#include <RcppArmadillo.h>
using namespace Rcpp;
#include "Depth.h"

// [[Rcpp::export]]
SEXP depthMahCPP(SEXP ru, SEXP rX) 
{
  Rcpp::NumericMatrix cu(ru);
  arma::mat u(cu.begin(), cu.nrow(), cu.ncol(), false);
  
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  
  arma::vec depth = Depth::MahalanobisDepth(u, X);
  return wrap(depth);
}

// [[Rcpp::export]]
SEXP depthProjCPP(SEXP ru, SEXP rX, double nproj, double seed) 
{
  Rcpp::NumericMatrix cu(ru);
  arma::mat u(cu.begin(), cu.nrow(), cu.ncol(), false);
  
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);
  
  
  arma::vec depth = Depth::ProjectionDepth(u, X, nproj, seed);
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




