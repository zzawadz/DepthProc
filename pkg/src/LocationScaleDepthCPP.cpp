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
    // Calculates the maximum sample location-scale depth  
    // for the data set y 
    // Uses function sample.depth.cont.for.mu and sample.max.depth.for.mu
    // p.length is the maximum length of the precision step at the end
    Rcpp::NumericVector cY(ry);
    arma::vec y(cY); 
    y = arma::sort(y);
    
    size_t N = y.n_elem;
    
    size_t d_min = floor(static_cast<double>(N)/3);
    size_t n_mid = round(static_cast<double>(N)/2);
    
    arma::vec res = LSD::sampleMaxDepthForMu(y[n_mid-1], y, d_min, iter, eps); 
     
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
      while(i < iter && (n_up-1>n_low && dec>0))
      {
        i++;
        n_mid_low = ceil(static_cast<double>(n_mid+n_low)/2);
        n_mid_up = floor(static_cast<double>(n_mid+n_up)/2);
        res_low  = LSD::sampleMaxDepthForMu(y[n_mid_low-1], y, d_min, iter, eps);
        res_up   = LSD::sampleMaxDepthForMu(y[n_mid_up-1], y, d_min, iter, eps);
                                                                      
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
      res_matrix.row(i) = LSD::sampleMaxDepthForMu(mu[i], y, d_min, iter, eps).t();
  /*    res<-rbind(res,sample.max.depth.for.mu(mu=mu[i],y=y,d.min=d.min,iter=iter,eps=eps))
      #   cat("i: ", i, "res: ", res[i,"i"], "\n")
      i.mu<-cbind(i.mu, res[i,"i"])*/
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
    
    return wrap(result);
}


