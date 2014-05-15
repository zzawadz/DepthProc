// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// CovLPCPP
SEXP CovLPCPP(SEXP X, double p, double a, double b);
RcppExport SEXP depthproc_CovLPCPP(SEXP XSEXP, SEXP pSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< SEXP >::type X(XSEXP );
        Rcpp::traits::input_parameter< double >::type p(pSEXP );
        Rcpp::traits::input_parameter< double >::type a(aSEXP );
        Rcpp::traits::input_parameter< double >::type b(bSEXP );
        SEXP __result = CovLPCPP(X, p, a, b);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// depth2dcpp
NumericVector depth2dcpp(SEXP R_x, SEXP R_y);
RcppExport SEXP depthproc_depth2dcpp(SEXP R_xSEXP, SEXP R_ySEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< SEXP >::type R_x(R_xSEXP );
        Rcpp::traits::input_parameter< SEXP >::type R_y(R_ySEXP );
        NumericVector __result = depth2dcpp(R_x, R_y);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// depthMahCPP
SEXP depthMahCPP(SEXP ru, SEXP rX);
RcppExport SEXP depthproc_depthMahCPP(SEXP ruSEXP, SEXP rXSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< SEXP >::type ru(ruSEXP );
        Rcpp::traits::input_parameter< SEXP >::type rX(rXSEXP );
        SEXP __result = depthMahCPP(ru, rX);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// depthProjCPP
SEXP depthProjCPP(SEXP ru, SEXP rX, double nproj, double seed);
RcppExport SEXP depthproc_depthProjCPP(SEXP ruSEXP, SEXP rXSEXP, SEXP nprojSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< SEXP >::type ru(ruSEXP );
        Rcpp::traits::input_parameter< SEXP >::type rX(rXSEXP );
        Rcpp::traits::input_parameter< double >::type nproj(nprojSEXP );
        Rcpp::traits::input_parameter< double >::type seed(seedSEXP );
        SEXP __result = depthProjCPP(ru, rX, nproj, seed);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// depthLPCPP
SEXP depthLPCPP(SEXP ru, SEXP rX, double p, double a, double b, int threads);
RcppExport SEXP depthproc_depthLPCPP(SEXP ruSEXP, SEXP rXSEXP, SEXP pSEXP, SEXP aSEXP, SEXP bSEXP, SEXP threadsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< SEXP >::type ru(ruSEXP );
        Rcpp::traits::input_parameter< SEXP >::type rX(rXSEXP );
        Rcpp::traits::input_parameter< double >::type p(pSEXP );
        Rcpp::traits::input_parameter< double >::type a(aSEXP );
        Rcpp::traits::input_parameter< double >::type b(bSEXP );
        Rcpp::traits::input_parameter< int >::type threads(threadsSEXP );
        SEXP __result = depthLPCPP(ru, rX, p, a, b, threads);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// sampleDepthContForMuCPP
SEXP sampleDepthContForMuCPP(double d, double mu, SEXP rY);
RcppExport SEXP depthproc_sampleDepthContForMuCPP(SEXP dSEXP, SEXP muSEXP, SEXP rYSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< double >::type d(dSEXP );
        Rcpp::traits::input_parameter< double >::type mu(muSEXP );
        Rcpp::traits::input_parameter< SEXP >::type rY(rYSEXP );
        SEXP __result = sampleDepthContForMuCPP(d, mu, rY);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// sampleMaxDepthForMuCPP
SEXP sampleMaxDepthForMuCPP(double mu, const SEXP rY, int d_min, int max_iter, double eps);
RcppExport SEXP depthproc_sampleMaxDepthForMuCPP(SEXP muSEXP, SEXP rYSEXP, SEXP d_minSEXP, SEXP max_iterSEXP, SEXP epsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< double >::type mu(muSEXP );
        Rcpp::traits::input_parameter< const SEXP >::type rY(rYSEXP );
        Rcpp::traits::input_parameter< int >::type d_min(d_minSEXP );
        Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP );
        Rcpp::traits::input_parameter< double >::type eps(epsSEXP );
        SEXP __result = sampleMaxDepthForMuCPP(mu, rY, d_min, max_iter, eps);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// sampleMaxLocScaleDepthCPP
SEXP sampleMaxLocScaleDepthCPP(SEXP ry, double iter, double eps, double p_length);
RcppExport SEXP depthproc_sampleMaxLocScaleDepthCPP(SEXP rySEXP, SEXP iterSEXP, SEXP epsSEXP, SEXP p_lengthSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< SEXP >::type ry(rySEXP );
        Rcpp::traits::input_parameter< double >::type iter(iterSEXP );
        Rcpp::traits::input_parameter< double >::type eps(epsSEXP );
        Rcpp::traits::input_parameter< double >::type p_length(p_lengthSEXP );
        SEXP __result = sampleMaxLocScaleDepthCPP(ry, iter, eps, p_length);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// runifsphereCPP
SEXP runifsphereCPP(double n, double p, int seed);
RcppExport SEXP depthproc_runifsphereCPP(SEXP nSEXP, SEXP pSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< double >::type n(nSEXP );
        Rcpp::traits::input_parameter< double >::type p(pSEXP );
        Rcpp::traits::input_parameter< int >::type seed(seedSEXP );
        SEXP __result = runifsphereCPP(n, p, seed);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
