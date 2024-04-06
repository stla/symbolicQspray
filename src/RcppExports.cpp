// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// SymbolicQspray_add
Rcpp::List SymbolicQspray_add(const Rcpp::List& Powers1, const Rcpp::List& Coeffs1, const Rcpp::List& Powers2, const Rcpp::List& Coeffs2);
RcppExport SEXP _symbolicQspray_SymbolicQspray_add(SEXP Powers1SEXP, SEXP Coeffs1SEXP, SEXP Powers2SEXP, SEXP Coeffs2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Powers1(Powers1SEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Coeffs1(Coeffs1SEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Powers2(Powers2SEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Coeffs2(Coeffs2SEXP);
    rcpp_result_gen = Rcpp::wrap(SymbolicQspray_add(Powers1, Coeffs1, Powers2, Coeffs2));
    return rcpp_result_gen;
END_RCPP
}
// SymbolicQspray_subtract
Rcpp::List SymbolicQspray_subtract(const Rcpp::List& Powers1, const Rcpp::List& Coeffs1, const Rcpp::List& Powers2, const Rcpp::List& Coeffs2);
RcppExport SEXP _symbolicQspray_SymbolicQspray_subtract(SEXP Powers1SEXP, SEXP Coeffs1SEXP, SEXP Powers2SEXP, SEXP Coeffs2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Powers1(Powers1SEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Coeffs1(Coeffs1SEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Powers2(Powers2SEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Coeffs2(Coeffs2SEXP);
    rcpp_result_gen = Rcpp::wrap(SymbolicQspray_subtract(Powers1, Coeffs1, Powers2, Coeffs2));
    return rcpp_result_gen;
END_RCPP
}
// SymbolicQspray_multiply
Rcpp::List SymbolicQspray_multiply(const Rcpp::List& Powers1, const Rcpp::List& Coeffs1, const Rcpp::List& Powers2, const Rcpp::List& Coeffs2);
RcppExport SEXP _symbolicQspray_SymbolicQspray_multiply(SEXP Powers1SEXP, SEXP Coeffs1SEXP, SEXP Powers2SEXP, SEXP Coeffs2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Powers1(Powers1SEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Coeffs1(Coeffs1SEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Powers2(Powers2SEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Coeffs2(Coeffs2SEXP);
    rcpp_result_gen = Rcpp::wrap(SymbolicQspray_multiply(Powers1, Coeffs1, Powers2, Coeffs2));
    return rcpp_result_gen;
END_RCPP
}
// SymbolicQspray_equality
bool SymbolicQspray_equality(const Rcpp::List& Powers1, const Rcpp::List& Coeffs1, const Rcpp::List& Powers2, const Rcpp::List& Coeffs2);
RcppExport SEXP _symbolicQspray_SymbolicQspray_equality(SEXP Powers1SEXP, SEXP Coeffs1SEXP, SEXP Powers2SEXP, SEXP Coeffs2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Powers1(Powers1SEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Coeffs1(Coeffs1SEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Powers2(Powers2SEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type Coeffs2(Coeffs2SEXP);
    rcpp_result_gen = Rcpp::wrap(SymbolicQspray_equality(Powers1, Coeffs1, Powers2, Coeffs2));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_symbolicQspray_SymbolicQspray_add", (DL_FUNC) &_symbolicQspray_SymbolicQspray_add, 4},
    {"_symbolicQspray_SymbolicQspray_subtract", (DL_FUNC) &_symbolicQspray_SymbolicQspray_subtract, 4},
    {"_symbolicQspray_SymbolicQspray_multiply", (DL_FUNC) &_symbolicQspray_SymbolicQspray_multiply, 4},
    {"_symbolicQspray_SymbolicQspray_equality", (DL_FUNC) &_symbolicQspray_SymbolicQspray_equality, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_symbolicQspray(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
