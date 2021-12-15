// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// constrainedClustering
List constrainedClustering(NumericMatrix& rMatrix, IntegerMatrix& rLinks, double maxDelta, int maxIterations, int totalRestarts, int totalClusters);
RcppExport SEXP _HiCDOC_constrainedClustering(SEXP rMatrixSEXP, SEXP rLinksSEXP, SEXP maxDeltaSEXP, SEXP maxIterationsSEXP, SEXP totalRestartsSEXP, SEXP totalClustersSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type rMatrix(rMatrixSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix& >::type rLinks(rLinksSEXP);
    Rcpp::traits::input_parameter< double >::type maxDelta(maxDeltaSEXP);
    Rcpp::traits::input_parameter< int >::type maxIterations(maxIterationsSEXP);
    Rcpp::traits::input_parameter< int >::type totalRestarts(totalRestartsSEXP);
    Rcpp::traits::input_parameter< int >::type totalClusters(totalClustersSEXP);
    rcpp_result_gen = Rcpp::wrap(constrainedClustering(rMatrix, rLinks, maxDelta, maxIterations, totalRestarts, totalClusters));
    return rcpp_result_gen;
END_RCPP
}
// parseHiCFile
DataFrame parseHiCFile(std::string& fname, int resolution, std::string& name);
RcppExport SEXP _HiCDOC_parseHiCFile(SEXP fnameSEXP, SEXP resolutionSEXP, SEXP nameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string& >::type fname(fnameSEXP);
    Rcpp::traits::input_parameter< int >::type resolution(resolutionSEXP);
    Rcpp::traits::input_parameter< std::string& >::type name(nameSEXP);
    rcpp_result_gen = Rcpp::wrap(parseHiCFile(fname, resolution, name));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_HiCDOC_constrainedClustering", (DL_FUNC) &_HiCDOC_constrainedClustering, 6},
    {"_HiCDOC_parseHiCFile", (DL_FUNC) &_HiCDOC_parseHiCFile, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_HiCDOC(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
