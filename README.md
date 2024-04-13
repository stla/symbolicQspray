The ‘symbolicQspray’ package
================
Stéphane Laurent
2024-04-13

*Symbolic multivariate polynomials.*

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/symbolicQspray/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stla/symbolicQspray/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

------------------------------------------------------------------------

A `symbolicQspray` object represents a multivariate polynomial whose
coefficients are fractions of polynomials with rational coefficients.
For example:

``` r
library(symbolicQspray)
f <- function(a1, a2, X1, X2, X3) {
  (a1/(a2^2+1)) * X1^2*X2  +  (a2+1) * X3  +  a1/a2
}
# "exterior" variables, the ones occuring in the coefficients:
a1 <- qlone(1)
a2 <- qlone(2)
# "main" variables:
X1 <- Qlone(1)
X2 <- Qlone(2)
X3 <- Qlone(3)
# the 'symbolicQspray':
( Qspray <- f(a1, a2, X1, X2, X3) )
## { [ a1 ] %//% [ a2^2 + 1 ] } * X^2.Y  +  { [ a2 + 1 ] } * Z  +  { [ a1 ] %//% [ a2 ] }
```

The fractions of polynomials such as the first coefficient `a1/(a2^2+1)`
in the above example are
[**ratioOfSprays**](https://github.com/stla/ratioOfQsprays) objects, and
the numerator and the denominator of a `ratioOfQsprays` are
[**qspray**](https://github.com/stla/qspray) objects.

Arithmetic on `symbolicQspray` objects is available:

``` r
Qspray^2
## { [ a1^2 ] %//% [ a2^4 + 2*a2^2 + 1 ] } * X^4.Y^2  +  { [ 2*a1.a2 + 2*a1 ] %//% [ a2^2 + 1 ] } * X^2.Y.Z  +  { [ 2*a1^2 ] %//% [ a2^3 + a2 ] } * X^2.Y  +  { [ a2^2 + 2*a2 + 1 ] } * Z^2  +  { [ 2*a1.a2 + 2*a1 ] %//% [ a2 ] } * Z  +  { [ a1^2 ] %//% [ a2^2 ] }
Qspray - Qspray
## 0
(Qspray - 1)^2
## { [ a1^2 ] %//% [ a2^4 + 2*a2^2 + 1 ] } * X^4.Y^2  +  { [ 2*a1.a2 + 2*a1 ] %//% [ a2^2 + 1 ] } * X^2.Y.Z  +  { [ 2*a1^2 - 2*a1.a2 ] %//% [ a2^3 + a2 ] } * X^2.Y  +  { [ a2^2 + 2*a2 + 1 ] } * Z^2  +  { [ 2*a1.a2 + 2*a1 - 2*a2^2 - 2*a2 ] %//% [ a2 ] } * Z  +  { [ a1^2 - 2*a1.a2 + a2^2 ] %//% [ a2^2 ] }
Qspray^2 - 2*Qspray + 1
## { [ a1^2 ] %//% [ a2^4 + 2*a2^2 + 1 ] } * X^4.Y^2  +  { [ 2*a1.a2 + 2*a1 ] %//% [ a2^2 + 1 ] } * X^2.Y.Z  +  { [ 2*a1^2 - 2*a1.a2 ] %//% [ a2^3 + a2 ] } * X^2.Y  +  { [ a2^2 + 2*a2 + 1 ] } * Z^2  +  { [ 2*a1.a2 + 2*a1 - 2*a2^2 - 2*a2 ] %//% [ a2 ] } * Z  +  { [ a1^2 - 2*a1.a2 + a2^2 ] %//% [ a2^2 ] }
```

## Evaluating a `symbolicQspray`

Substituting the “exterior” variables (the variables occurring in the
ratios of polynomials) yields another `symbolicQspray`:

``` r
a <- c(2, "3/2")
( Q <- evalSymbolicQspray(Qspray, a = a) )
## { [ 8/13 ] } * X^2.Y  +  { [ 5/2 ] } * Z  +  { [ 4/3 ] }
```

Substituting the “main” variables yields a `ratioOfQsprays` object:

``` r
X <- gmp::as.bigq(c(4, 3, "2/5"))
( ratioOfQsprays <- evalSymbolicQspray(Qspray, X = X) )
## [ x.y^2 + 48*x.y + x + 2/5*y^4 + 2/5*y^3 + 2/5*y^2 + 2/5*y ]  %//%  [ y^3 + y ]
```

Checking the consistency:

``` r
evalSymbolicQspray(Qspray, a = a, X = X)
## [ 1243/39 ]
evalSymbolicQspray(Q, X = X)
## [ 1243/39 ]
evalRatioOfQsprays(ratioOfQsprays, a)
## Big Rational ('bigq') :
## [1] 1243/39
f(gmp::as.bigq(a[1]), gmp::as.bigq(a[2]), X[1], X[2], X[3])
## Big Rational ('bigq') :
## [1] 1243/39
```

Actually we can more generally use some `ratioOfQsprays` objects for the
values of `X`, since a `symbolicQspray` object is a polynomial with
`ratioOfQsprays` coefficients:

``` r
X <- list(
  qlone(1)/(1+qlone(1)), qlone(2)/qlone(1)^2, (qlone(2) + qlone(3))/qlone(1)
)
evalSymbolicQspray(Qspray, a = a, X = X)
## [ 4/3*x^3 + 5/2*x^2.y + 5/2*x^2.z + 8/3*x^2 + 73/13*x.y + 5*x.z + 4/3*x + 5/2*y + 5/2*z ]  %//%  [ x^3 + 2*x^2 + x ]
evalSymbolicQspray(Q, X = X)
## [ 4/3*x^3 + 5/2*x^2.y + 5/2*x^2.z + 8/3*x^2 + 73/13*x.y + 5*x.z + 4/3*x + 5/2*y + 5/2*z ]  %//%  [ x^3 + 2*x^2 + x ]
```

## Querying a `symbolicQspray`

The package provides some functions to perform elementary queries on a
`symbolicQspray`:

``` r
numberOfVariables(Qspray)
## [1] 3
numberOfTerms(Qspray)
## [1] 3
getCoefficient(Qspray, c(2, 1))
## [ x ]  %//%  [ y^2 + 1 ]
getConstantTerm(Qspray)
## [ x ]  %//%  [ y ]
isUnivariate(Qspray)
## [1] FALSE
isConstant(Qspray)
## [1] FALSE
```

## Showing a `symbolicQspray`

You can change the way a `symbolicQspray` is printed by using
`showSymbolicQsprayOption`:

``` r
showSymbolicQsprayOption(Qspray, "a") <- "x"
showSymbolicQsprayOption(Qspray, "showMonomial") <- 
  showMonomialXYZ(c("A", "B", "C"))
showSymbolicQsprayOption(Qspray, "quotientBar") <- " / "
Qspray
## { [ x1 ] / [ x2^2 + 1 ] } * A^2.B  +  { [ x2 + 1 ] } * C  +  { [ x1 ] / [ x2 ] }
```

When this is possible, the result of an arithmetic operation between two
`symbolicQspray` objects inherits the show options of the first operand:

``` r
( Q <- rSymbolicQspray() ) # a random symbolicQspray
## { [ -4 ] %//% [ 5*a1^2.a3 + 3*a2^4 + 4 ] } * X^3.Y^2  +  { [ -a1^3 ] %//% [ -4*a1.a3^4 - a3^4 ] } * X^3  +  { [ 2*a1^4.a3 + 5*a3 ] %//% [ -3*a1^2.a3^3 - 2 ] } * Z^2
Qspray + Q
## { [ -4 ] / [ 5*x1^2.x3 + 3*x2^4 + 4 ] } * A^3.B^2  +  { [ -x1^3 ] / [ -4*x1.x3^4 - x3^4 ] } * A^3  +  { [ x1 ] / [ x2^2 + 1 ] } * A^2.B  +  { [ 2*x1^4.x3 + 5*x3 ] / [ -3*x1^2.x3^3 - 2 ] } * C^2  +  { [ x2 + 1 ] } * C  +  { [ x1 ] / [ x2 ] }
```

## Application to Jack polynomials

The **symbolicQspray** package is used by the
[**jack**](https://github.com/stla/jackR) package to compute the Jack
polynomials. The Jack polynomials are exactly the polynomials
represented by the `symbolicQspray` objects: their coefficients are
fractions of polynomials by definition, of one variable: the Jack
parameter.
