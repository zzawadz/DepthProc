#include <RcppArmadillo.h>

#ifndef ROBCOVLIBCONFIG_INCLUDES
#define ROBCOVLIBCONFIG_INCLUDES

#if !defined(ARMA_USE_LAPACK)
#define ARMA_USE_LAPACK
#endif

#if !defined(ARMA_USE_BLAS)
#define ARMA_USE_BLAS
#endif

#if !defined(ARMA_DEFAULT_OSTREAM)
  #define ARMA_DEFAULT_OSTREAM Rcpp::rout
#endif

/*#if !defined(ARMA_NO_DEBUG)
#define ARMA_NO_DEBUG
#endif*/


#endif
