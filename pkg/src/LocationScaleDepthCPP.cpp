#include <RcppArmadillo.h>
#include "LocationScaleDepth.h"
#include "Utils.h"
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
SEXP sampleDepthContForMuCPP(double d, double mu, SEXP rY)
{
    Rcpp::NumericVector cY(rY);
    arma::vec y(cY); 
    y = arma::sort(y);
    
    size_t m = 0;
    size_t n = y.n_elem;
    for(size_t i = 0; i < n; i++) if(y[i]<mu) m++;
  
    arma::vec result = LSD::sampleDepthContForMu(d, mu, y, m);
    return wrap(result);
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
SEXP sampleMaxDepthForMuCPP(double mu,const SEXP rY, int d_min, int max_iter, double eps) 
{
    Rcpp::NumericVector cY(rY);
    arma::vec y(cY); 
    y = arma::sort(y);
    
    arma::vec result = LSD::sampleMaxDepthForMu(mu, y, d_min, max_iter, eps); 
    return wrap(result);
}



// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
SEXP  sampleMaxLocScaleDepthCPP(SEXP ry, double iter, double eps, double p_length)
{
    Rcpp::NumericVector cY(ry);
    arma::vec y(cY); 
    arma::vec result = LSD::sampleMaxLocScaleDepth(y, iter, eps, p_length);
    return wrap(result);
}


