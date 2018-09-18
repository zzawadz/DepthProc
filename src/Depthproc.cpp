#include "depthlib.h"
#include <RcppArmadillo.h>
using namespace Rcpp;

typedef std::map<double, double>::iterator MapIter;

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


// [[Rcpp::export]]
SEXP runifsphereCPP(double n, double p)
{
  arma::mat X = Utils::runifsphere(n, p);
  return wrap(X);
}

// [[Rcpp::export]]
SEXP covCPP(SEXP rX, int threads)
{
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);

  arma::mat cov = Utils::cov(X, threads);
  return wrap(cov);
}

// [[Rcpp::export]]
SEXP meanCPP(SEXP rX, int threads)
{
  Rcpp::NumericMatrix cX(rX);
  arma::mat X(cX.begin(), cX.nrow(), cX.ncol(), false);

  arma::rowvec meanr = Utils::mean(X, threads);
  return wrap(meanr);
}

// [[Rcpp::export]]
std::vector<double> refRank(std::vector<double> u, std::vector<double> x) {

  std::vector<double> rank_result(u.size());

  std::vector<size_t> order(u.size());

  std::iota(order.begin(), order.end(), 0);
  sort(order.begin(), order.end(),
       [&u](size_t i1, size_t i2) {return u[i1] < u[i2];});

  std::map<double, double> table;

  // counts number of each elements
  for(size_t i = 0; i < x.size(); i++) {
    MapIter iter = table.find(x[i]);

    if(iter != table.end()) {
      iter->second++;
    } else {
      table[x[i]] = 1.0;
    }
  }

  double total_rank = 0;
  MapIter tbl = table.begin();

  for(size_t i = 0; i < u.size(); i++) {

    double val = u[order[i]];

    while(tbl != table.end() && val > tbl->first) {

      double tmp = tbl->second;
      total_rank += tmp;
      tbl++;

      // Correction so that max avaiable rank
      // is equal to max rank of x
      if(tbl == table.end()) {
        total_rank -= 0.5 * tmp - 0.5;
      }

    }

    double rank = total_rank;

    if(tbl != table.end() && val == tbl->first) {
      rank += 0.5 + (0.5*tbl->second);
    }

    rank_result[order[i]] = rank;
  }

  return rank_result;
}


// [[Rcpp::export]]
SEXP CovLPCPP(SEXP X, double p, double a, double b)
{
  Rcpp::NumericMatrix cX(X);
  arma::mat aX(cX.begin(), cX.nrow(), cX.ncol(), false);

  arma::mat cov = RobCovLib::LPDepthCovarianceEstimator(aX, p, a, b);
  return wrap(cov);
}


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

