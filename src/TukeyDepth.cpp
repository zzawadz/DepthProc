#include "TukeyDepth.h"


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
