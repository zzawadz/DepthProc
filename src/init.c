#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP DepthProc_covCPP(SEXP, SEXP);
extern SEXP DepthProc_CovLPCPP(SEXP, SEXP, SEXP, SEXP);
extern SEXP DepthProc_depth2dcpp(SEXP, SEXP);
extern SEXP DepthProc_depthLPCPP(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP DepthProc_depthMahCPP(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP DepthProc_depthProjCPP(SEXP, SEXP, SEXP, SEXP);
extern SEXP DepthProc_depthTukeyCPP(SEXP, SEXP, SEXP, SEXP);
extern SEXP DepthProc_meanCPP(SEXP, SEXP);
extern SEXP DepthProc_modBandDepth(SEXP);
extern SEXP DepthProc_modBandDepthRef(SEXP, SEXP);
extern SEXP DepthProc_refRank(SEXP, SEXP);
extern SEXP DepthProc_runifsphereCPP(SEXP, SEXP);
extern SEXP DepthProc_sampleDepthContForMuCPP(SEXP, SEXP, SEXP);
extern SEXP DepthProc_sampleMaxDepthForMuCPP(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP DepthProc_sampleMaxLocScaleDepthCPP(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"DepthProc_covCPP",                    (DL_FUNC) &DepthProc_covCPP,                    2},
  {"DepthProc_CovLPCPP",                  (DL_FUNC) &DepthProc_CovLPCPP,                  4},
  {"DepthProc_depth2dcpp",                (DL_FUNC) &DepthProc_depth2dcpp,                2},
  {"DepthProc_depthLPCPP",                (DL_FUNC) &DepthProc_depthLPCPP,                6},
  {"DepthProc_depthMahCPP",               (DL_FUNC) &DepthProc_depthMahCPP,               5},
  {"DepthProc_depthProjCPP",              (DL_FUNC) &DepthProc_depthProjCPP,              4},
  {"DepthProc_depthTukeyCPP",             (DL_FUNC) &DepthProc_depthTukeyCPP,             4},
  {"DepthProc_meanCPP",                   (DL_FUNC) &DepthProc_meanCPP,                   2},
  {"DepthProc_modBandDepth",              (DL_FUNC) &DepthProc_modBandDepth,              1},
  {"DepthProc_modBandDepthRef",           (DL_FUNC) &DepthProc_modBandDepthRef,           2},
  {"DepthProc_refRank",                   (DL_FUNC) &DepthProc_refRank,                   2},
  {"DepthProc_runifsphereCPP",            (DL_FUNC) &DepthProc_runifsphereCPP,            2},
  {"DepthProc_sampleDepthContForMuCPP",   (DL_FUNC) &DepthProc_sampleDepthContForMuCPP,   3},
  {"DepthProc_sampleMaxDepthForMuCPP",    (DL_FUNC) &DepthProc_sampleMaxDepthForMuCPP,    5},
  {"DepthProc_sampleMaxLocScaleDepthCPP", (DL_FUNC) &DepthProc_sampleMaxLocScaleDepthCPP, 4},
  {NULL, NULL, 0}
};

void R_init_DepthProc(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
