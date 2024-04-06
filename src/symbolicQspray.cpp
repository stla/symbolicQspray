#include "symbolicQspray.h"

using namespace RATIOOFQSPRAYS;

// -------------------------------------------------------------------------- //
Rcpp::List returnSymbolicQspray(SymbolicQspray SQ) { // to return a list to R
  symbolicQspray S = SQ.get();
  if(S.size() == 0) {
    return Rcpp::List::create(Rcpp::Named("powers") = R_NilValue,
                              Rcpp::Named("coeffs") = R_NilValue);
  } else {
    Rcpp::List Powers(S.size());
    powers pows;
    unsigned int row = 0, col = 0;
    Rcpp::List Coeffs(S.size());
    unsigned int i = 0;
    for(auto it = S.begin(); it != S.end(); ++it) {
      pows = it->first;
      Rcpp::IntegerVector Exponents(pows.size());
      col = 0;
      for(auto ci = pows.begin(); ci != pows.end(); ++ci) {
        Exponents(col++) = *ci;
      }
      Powers(row++) = Exponents;
      Coeffs(i++) = returnRatioOfQsprays(it->second);
    }
    return Rcpp::List::create(Rcpp::Named("powers") = Powers,
                              Rcpp::Named("coeffs") = Coeffs);
  }
}

// -------------------------------------------------------------------------- //
SymbolicQspray makeSymbolicQspray(
    const Rcpp::List& Powers, const Rcpp::List& Coeffs
) {
  symbolicQspray S;
  for(int i = 0; i < Powers.size(); i++) {
    Rcpp::IntegerVector Exponents = Powers(i);
    powers pows(Exponents.begin(), Exponents.end());
    Rcpp::List coeff = Coeffs(i);
    Rcpp::List numerator   = coeff["numerator"];
    Rcpp::List denominator = coeff["denominator"];
    S[pows] = makeRatioOfQsprays(numerator, denominator);
  }
  return SymbolicQspray(S);
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SymbolicQspray_add(
    const Rcpp::List& Powers1, const Rcpp::List& Coeffs1,
    const Rcpp::List& Powers2, const Rcpp::List& Coeffs2
) {
  SymbolicQspray sqspray1 = makeSymbolicQspray(Powers1, Coeffs1);
  SymbolicQspray sqspray2 = makeSymbolicQspray(Powers2, Coeffs2);
  return returnSymbolicQspray(sqspray1 + sqspray2);
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SymbolicQspray_subtract(
    const Rcpp::List& Powers1, const Rcpp::List& Coeffs1,
    const Rcpp::List& Powers2, const Rcpp::List& Coeffs2
) {
  SymbolicQspray sqspray1 = makeSymbolicQspray(Powers1, Coeffs1);
  SymbolicQspray sqspray2 = makeSymbolicQspray(Powers2, Coeffs2);
  return returnSymbolicQspray(sqspray1 - sqspray2);
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SymbolicQspray_multiply(
    const Rcpp::List& Powers1, const Rcpp::List& Coeffs1,
    const Rcpp::List& Powers2, const Rcpp::List& Coeffs2
) {
  SymbolicQspray sqspray1 = makeSymbolicQspray(Powers1, Coeffs1);
  SymbolicQspray sqspray2 = makeSymbolicQspray(Powers2, Coeffs2);
  return returnSymbolicQspray(sqspray1 * sqspray2);
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::List SymbolicQspray_power(
    const Rcpp::List& Powers, const Rcpp::List& Coeffs, unsigned int n
) {
  SymbolicQspray sqspray = makeSymbolicQspray(Powers, Coeffs);
  return returnSymbolicQspray(sqspray.power(n));
}

// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
bool SymbolicQspray_equality(
    const Rcpp::List& Powers1, const Rcpp::List& Coeffs1,
    const Rcpp::List& Powers2, const Rcpp::List& Coeffs2
) {
  SymbolicQspray sqspray1 = makeSymbolicQspray(Powers1, Coeffs1);
  SymbolicQspray sqspray2 = makeSymbolicQspray(Powers2, Coeffs2);
  return sqspray1 == sqspray2;
}
