The ‘symbolicQspray’ package
================
Stéphane Laurent
2024-04-11

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
# "exterior" variables:
a1 <- qlone(1)
a2 <- qlone(2)
# "main" variables:
X1 <- Qlone(1)
X2 <- Qlone(2)
X3 <- Qlone(3)
# the 'symbolicQspray':
( Qspray <- f(a1, a2, X1, X2, X3) )
## { [ a1 ] %//% [ a2^2 + 1 ] } * X^2Y  +  { [ a2 + 1 ] } * Z
```

Arithmetic on `symbolicQspray` objects is available:

``` r
Qspray^2
## { [ a1^2 ] %//% [ a2^4 + 2*a2^2 + 1 ] } * X^4Y^2  +  { [ 2*a1.a2 + 2*a1 ] %//% [ a2^2 + 1 ] } * X^2YZ  +  { [ a2^2 + 2*a2 + 1 ] } * Z^2
Qspray - Qspray
## Warning in max(vapply(target@coeffs, numberOfVariables, integer(1L))): aucun
## argument pour max ; -Inf est renvoyé
## 0
```

Substituting the “exterior” variables (the variables occurring in the
ratios of polynomials) yields a `symbolicQspray` object:

``` r
a <- c(2, "3/2")
( Q <- evalSymbolicQspray(Qspray, a = a) )
## { [ 8/13 ] } * X^2Y  +  { [ 5/2 ] } * Z
```

Substituting the “main” variables yields a `ratioOfQsprays` object:

``` r
X <- gmp::as.bigq(c(4, 3, "2/5"))
( ratioOfQsprays <- evalSymbolicQspray(Qspray, X = X) )
## [ 48*x + 2/5*y^3 + 2/5*y^2 + 2/5*y + 2/5 ]  %//%  [ y^2 + 1 ]
```

Checking the consistency:

``` r
evalSymbolicQspray(Qspray, a = a, X = X)
## [ 397/13 ]
evalSymbolicQspray(Q, X = X)
## [ 397/13 ]
evalRatioOfQsprays(ratioOfQsprays, a)
## Big Rational ('bigq') :
## [1] 397/13
```

Actually we can more generally use some `ratioOfQsprays` objects for the
values of `X`, since a `symbolicQspray` object is a polynomial with
`ratioOfQsprays` coefficients:

``` r
X <- list(
  qlone(1)/(1+qlone(1)), qlone(2)/qlone(1)^2, (qlone(2) + qlone(3))/qlone(1)
)
evalSymbolicQspray(Qspray, a = a, X = X)
## [ 5/2*x^2y + 5/2*x^2z + 73/13*xy + 5*xz + 5/2*y + 5/2*z ]  %//%  [ x^3 + 2*x^2 + x ]
evalSymbolicQspray(Q, X = X)
## [ 5/2*x^2y + 5/2*x^2z + 73/13*xy + 5*xz + 5/2*y + 5/2*z ]  %//%  [ x^3 + 2*x^2 + x ]
```
