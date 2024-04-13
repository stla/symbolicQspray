The ‘symbolicQspray’ package
================
Stéphane Laurent
2024-04-13

***Symbolic multivariate polynomials.***

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

This is discutable actually: this polynomial has rational coefficients,
so one could consider it is a `qspray`. But on the other hand, one can
consider that the polynomial variables `X`, `Y` and `Z` have not changed
and still represent some indeterminate `ratioOfQsprays` fractions.
Comments ?..

Substituting the “main” variables yields a `ratioOfQsprays` object:

``` r
X <- gmp::as.bigq(c(4, 3, "2/5"))
( ratioOfQsprays <- evalSymbolicQspray(Qspray, X = X) )
## [ a1.a2^2 + 48*a1.a2 + a1 + 2/5*a2^4 + 2/5*a2^3 + 2/5*a2^2 + 2/5*a2 ] %//% [ a2^3 + a2 ]
```

Actually we can more generally use some `ratioOfQsprays` objects for the
values of `X`, since a `symbolicQspray` object is a polynomial with
`ratioOfQsprays` coefficients. We will do that, just after checking the
consistency:

``` r
evalSymbolicQspray(Qspray, a = a, X = X)
evalSymbolicQspray(Q, X = X)
## [ 1243/39 ]
evalRatioOfQsprays(ratioOfQsprays, a)
## Big Rational ('bigq') :
## [1] 1243/39
f(gmp::as.bigq(a[1]), gmp::as.bigq(a[2]), X[1], X[2], X[3])
## Big Rational ('bigq') :
## [1] 1243/39
```

Now let’s use some `ratioOfQsprays` objects for the values of `X`:

``` r
X <- list(
  qlone(1)/(1+qlone(1)), qlone(2)/qlone(1)^2, (qlone(2) + qlone(3))/qlone(1)
)
evalSymbolicQspray(Qspray, a = a, X = X)
evalSymbolicQspray(Q, X = X)
## [ 4/3*a1^3 + 5/2*a1^2.a2 + 5/2*a1^2.a3 + 8/3*a1^2 + 73/13*a1.a2 + 5*a1.a3 + 4/3*a1 + 5/2*a2 + 5/2*a3 ] %//% [ a1^3 + 2*a1^2 + a1 ]
```

This is consistent, but again, there’s a discutable point here: are we
allowed to use `qlone(3)` (that is, `a3`) in the values of `X`, since
the coefficients of `Qspray` involve `a1` and `a2` only? Here we cannot
know: only the context can tell us in which space the coefficients of
`Qspray` are living.

Also note that evaluating the `ratioOfQsprays` object
`evalSymbolicQspray(Qspray, X = X)` at `a` makes no sense…

Actually my motivation to do this package was inspired by the **Jack
polynomials**. In the context of Jack polynomials, the variables `X`,
`Y` and `Z` represent indeterminate *numbers*, and the coefficients are
*numbers depending on a parameter* (the Jack parameter), and it turns
out that they are fractions of polynomials of this parameter. So in this
context, the answers to the above questions are clear: we don’t have to
replace the `X`, `Y` and `Z` with `rationalQsprays` objects.

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
## { [ -a1 + a2 + 5*a3^2 ] %//% [ -3*a1^2 ] } * X.Y.Z
Qspray + Q
## { [ x1 ] / [ x2^2 + 1 ] } * A^2.B  +  { [ -x1 + x2 + 5*x3^2 ] / [ -3*x1^2 ] } * A.B.C  +  { [ x2 + 1 ] } * C  +  { [ x1 ] / [ x2 ] }
```

## Application to Jack polynomials

The **symbolicQspray** package is used by the
[**jack**](https://github.com/stla/jackR) package to compute the Jack
polynomials. The Jack polynomials are exactly the polynomials
represented by the `symbolicQspray` objects: their coefficients are
fractions of polynomials by definition, of one variable: the Jack
parameter.
