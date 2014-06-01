#include "LocationScaleDepth.h"

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
//  Rcpp::NumericVector cY(ry);
//  arma::vec y(cY); 
//  y = arma::sort(y);

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
    //std::printf("Ubound value %i %i %i \n", d_up, d_low, d_up-d_low);
    while(iter < max_iter && fabs(difbound) > eps && d_up-1>d_low)
    {
      //std::printf("Ubound value %f \n", iter);
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


}


