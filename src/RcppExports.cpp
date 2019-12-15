// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// calc_distance_C
NumericVector calc_distance_C(NumericMatrix trainX, NumericVector testX);
RcppExport SEXP _dann_calc_distance_C(SEXP trainXSEXP, SEXP testXSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type trainX(trainXSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type testX(testXSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_distance_C(trainX, testX));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dann_calc_distance_C", (DL_FUNC) &_dann_calc_distance_C, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_dann(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}