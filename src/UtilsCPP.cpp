#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>
#include <map>

using namespace Rcpp;
#include "Utils.h"

typedef std::map<double, double>::iterator MapIter;

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
