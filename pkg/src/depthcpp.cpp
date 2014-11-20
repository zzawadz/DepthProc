
#include <Rcpp.h>
#include <vector>
using namespace Rcpp;


///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
int computeRegDepth(double *x, double *y, std::vector<double> coef, int n){
  
  int i = 0;
  int lmin = 0;
  int lmax = 0;
  int rmin = 0;
  int rmax = 0;
  int tmp_depth = 0;
  int depth = 0;
  
  std::vector<double> res(n); 
  
  
  for(i = 0; i < n; i++){
    res[i] = y[i] - x[i]*coef[0] - coef[1];
    if(res[i]>=0) {
      rmax++;
    } 
    if(res[i]<=0) {
      rmin++;
    }
  }
  depth = (lmin+rmax<=lmax+rmin)?lmin+rmax:lmax+rmin;
 

  
  for(i = 0; i < n; i++){
    if(res[i] == 0){
      lmin++; lmax++; rmax--; rmin--;
    }
    if(res[i]>0){
      rmax--; lmax++;
    } else{
      rmin--; lmin++;
    }
    
    if(lmin+rmax>lmax+rmin){
      tmp_depth = lmax+rmin;
    } else {
      tmp_depth = lmin + rmax;
    }
    depth = (depth<=tmp_depth)? depth : tmp_depth;

  }
  return depth;
}

std::vector<double> getCoefficient(double x1, double y1, double x2, double y2){
  std::vector<double> coeff(2);
  
  if(x1 == x2){
    coeff[0] = 0;
    coeff[1] = 0;
    return coeff;
  }
  
  coeff[0] = (y2-y1)/(x2-x1);
  coeff[1] = y1 - coeff[0] * x1;
  return coeff;
}
// [[Rcpp::export]]
NumericVector depth2dcpp(SEXP R_x, SEXP R_y) {
    
  
    NumericVector x(R_x), y(R_y);
    int n = x.size();
    double *px = REAL(R_x);
    double *py = REAL(R_y);
    int tmp_depth = 0;
    int depth = 0;
    std::vector<double> coef;
    std::vector<double> deep_coef;
  
    
    for(int i=0; i<n-1; i++){
      for(int j = i+1; j<n; j++){
        
         coef = getCoefficient(x[i],y[i],x[j],y[j]);
         tmp_depth = computeRegDepth(px,py,coef,n);
       
         
         if(tmp_depth > depth){
           depth = tmp_depth;
           deep_coef = coef;
         } 
      }
    }
    
    
    
    NumericVector yy   = NumericVector::create(deep_coef[0],deep_coef[1],depth) ;
    
    return yy ;
}
