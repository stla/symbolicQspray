The ‘symbolicQspray’ package
================
Stéphane Laurent
2024-04-07

*Symbolic multivariate polynomials.*

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/symbolicQspray/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/symbolicQspray/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

------------------------------------------------------------------------

A `symbolicQspray` object represents a multivariate polynomial whose
coefficients are fractions of polynomials. For example:

``` r
library(symbolicQspray)
f <- function(a1, a2, X1, X2, X3) {
  (a1/(a2^2+1)) * X1^2*X2  +  (a2+1) * X3
}
# "inner" variables:
a1 <- qlone(1)
a2 <- qlone(2)
X1 <- Qlone(1)
# "main" variables:
X2 <- Qlone(2)
X3 <- Qlone(3)
# the 'symbolicQspray':
( Qspray <- f(a1, a2, X1, X2, X3) )
## { [ a1 ] %//% [ a2^2 + 1 ] } * X1^2.X2  +  { [a2 + 1] } * X3
```

Arithmetic on `symbolicQspray` objects is available:

``` r
Qspray^2
## { [ a1^2 ] %//% [ a2^4 + 2*a2^2 + 1 ] } * X1^4.X2^2  +  { [ 2*a1.a2 + 2*a1 ] %//% [ a2^2 + 1 ] } * X1^2.X2.X3  +  { [a2^2 + 2*a2 + 1] } * X3^2
Qspray - Qspray
## 0
```

Substituting the “inner” variables (the variables occurring in the
ratios of polynomials) yields a `qspray` object:

``` r
a <- c(2, "3/2")
( qspray <- evalSymbolicQspray(Qspray, a = a) )
## 8/13*X1^2.X2 + 5/2*X3
```

Substituting the “main” variables yields a `ratioOfQsprays` object:

``` r
X <-  c(4, 3, "2/5")
( ratioOfQsprays <- evalSymbolicQspray(Qspray, X = X) )
## [ 48*a1 + 2/5*a2^3 + 2/5*a2^2 + 2/5*a2 + 2/5 ]  %//%  [ a2^2 + 1 ]
```

Checking the consistency:

``` r
evalSymbolicQspray(Qspray, a = a, X = X)
## Big Rational ('bigq') :
## [1] 397/13
evalQspray(qspray, X)
## Big Rational ('bigq') :
## [1] 397/13
evalRatioOfQsprays(ratioOfQsprays, a)
## Big Rational ('bigq') :
## [1] 397/13
```
