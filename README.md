The ‘symbolicQspray’ package
================
Stéphane Laurent
2024-04-16

***Multivariate polynomials with symbolic coefficients.***

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
# "exterior" variables, the ones occurring in the coefficients:
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
ratios of polynomials) yields a `qspray`:

``` r
a <- c(2, "3/2")
( qspray <- evalSymbolicQspray(Qspray, a = a) )
## 8/13*X^2.Y + 5/2*Z + 4/3
```

Substituting the “main” variables yields a `ratioOfQsprays` object:

``` r
X <- c(4, 3, "2/5")
( ratioOfQsprays <- evalSymbolicQspray(Qspray, X = X) )
## [ a1.a2^2 + 48*a1.a2 + a1 + 2/5*a2^4 + 2/5*a2^3 + 2/5*a2^2 + 2/5*a2 ] %//% [ a2^3 + a2 ]
```

There is a discutable point here. A `symbolicQspray` object represents a
polynomial with `ratioOfQsprays` coefficients. So one can consider that
the polynomial variables `X`, `Y` and `Z` represent some indeterminate
`ratioOfQsprays` fractions, and that it should be possible to replace
them with `ratioOfQsprays` objects. However this is not allowed. We will
discuss that, just after checking the consistency:

``` r
evalSymbolicQspray(Qspray, a = a, X = X)
## Big Rational ('bigq') :
## [1] 1243/39
evalQspray(qspray, X)
## Big Rational ('bigq') :
## [1] 1243/39
evalRatioOfQsprays(ratioOfQsprays, a)
## Big Rational ('bigq') :
## [1] 1243/39
a <- gmp::as.bigq(a); X <- gmp::as.bigq(X)
f(a[1], a[2], X[1], X[2], X[3])
## Big Rational ('bigq') :
## [1] 1243/39
```

Now let’s turn to our promised discussion. Why is replacing the values
of the polynomial variables with some `ratioOfQsprays` objects not
allowed?

Actually my motivation to do this package was inspired by the **Jack
polynomials**. In the context of Jack polynomials, the variables `X`,
`Y` and `Z` represent indeterminate *numbers*, and the coefficients are
*numbers depending on a parameter* (the Jack parameter), and it turns
out that they are fractions of polynomials of this parameter. So I
consider that a `symbolicQspray` is *not* a polynomial on the field of
fractions of polynomials: I consider it is a polynomial with *rational
coefficients depending on some parameters*.

By the way, I’m wondering whether I should rename `symbolicQspray` to
`parametricQspray`. Comments?..

Also note that evaluating the `ratioOfQsprays` object
`evalSymbolicQspray(Qspray, X = X)` at `a` would make no sense if we
took some `ratioOfQsprays` objects for the values of `X`.

## Querying a `symbolicQspray`

The package provides some functions to perform elementary queries on a
`symbolicQspray`:

``` r
numberOfVariables(Qspray)
## [1] 3
numberOfTerms(Qspray)
## [1] 3
getCoefficient(Qspray, c(2, 1))
## [ a1 ] %//% [ a2^2 + 1 ]
getConstantTerm(Qspray)
## [ a1 ] %//% [ a2 ]
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
## { [ 4*a2^3 ] %//% [ 3*a1^2.a2 + 4*a2^2.a3^2 + a3^4 ] } * X^3.Z^3  +  { [ a2^4 ] %//% [ -3*a1^2.a2 - 2*a1 ] } * X^2.Y^4.Z^3
Qspray + Q
## { [ 4*x2^3 ] / [ 3*x1^2.x2 + 4*x2^2.x3^2 + x3^4 ] } * A^3.C^3  +  { [ x2^4 ] / [ -3*x1^2.x2 - 2*x1 ] } * A^2.B^4.C^3  +  { [ x1 ] / [ x2^2 + 1 ] } * A^2.B  +  { [ x2 + 1 ] } * C  +  { [ x1 ] / [ x2 ] }
```

This behavior is the same as the ones implemented in **qspray** and in
**ratioOfQsprays**. You should be familiar with these two packages in
order to use **symbolicQspray**.

## Application to Jack polynomials

The **symbolicQspray** package is used by the
[**jack**](https://github.com/stla/jackR) package to compute the Jack
polynomials. The Jack polynomials are exactly the polynomials
represented by the `symbolicQspray` objects: their coefficients are
fractions of polynomials by definition, of one variable: the Jack
parameter.
