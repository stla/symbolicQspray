#' @include symbolicQspray.R
NULL

setGeneric("numberOfVariables")
setGeneric("numberOfTerms")
setGeneric("getCoefficient")
setGeneric("getConstantTerm")
setGeneric("isConstant")
setGeneric("isUnivariate")
setGeneric("isQzero")
setGeneric("isQone")

#' @name numberOfVariables
#' @aliases numberOfVariables,symbolicQspray-method
#' @docType methods
#' @importFrom qspray numberOfVariables
#' @title Number of variables in a 'symbolicQspray' polynomial
#' @description Number of variables involved in a \code{symbolicQspray} object.
#'
#' @param x a \code{symbolicQspray} object
#'
#' @return An integer.
#' @export
#' @note The number of variables in the \code{symbolicQspray} object
#'   \code{Qlone(d)} is \code{d}, not \code{1}.
setMethod(
  "numberOfVariables", "symbolicQspray",
  function(x) {
    as.integer(max(0L, arity(x)))
  }
)

#' @name numberOfTerms
#' @aliases numberOfTerms,symbolicQspray-method
#' @docType methods
#' @importFrom qspray numberOfTerms
#' @title Number of terms in a 'symbolicQspray' polynomial
#' @description Number of terms in the polynomial defined by a
#'   \code{symbolicQspray} object.
#'
#' @param qspray a \code{symbolicQspray} object
#'
#' @return An integer.
#' @export
setMethod(
  "numberOfTerms", "symbolicQspray",
  function(qspray) {
    length(qspray@powers)
  }
)

#' @name getCoefficient
#' @aliases getCoefficient,symbolicQspray,numeric-method
#' @docType methods
#' @importFrom qspray getCoefficient
#' @title Get a coefficient in a 'symbolicQspray' polynomial
#' @description Get the coefficient corresponding to the given sequence of
#'   exponents.
#'
#' @param qspray a \code{symbolicQspray} object
#' @param exponents a vector of exponents
#'
#' @return The coefficient as a \code{ratioOfQsprays} object.
#' @export
#' @importFrom gmp as.bigq
#' @importFrom ratioOfQsprays showRatioOfQspraysOption<-
#'
#' @examples
#' library(qspray)
#' x <- qlone(1)
#' y <- qlone(2)
#' p <- 2*x^2 + 3*y - 5
#' getCoefficient(p, 2)       # coefficient of x^2
#' getCoefficient(p, c(2, 0)) # same as getCoefficient(p, 2)
#' getCoefficient(p, c(0, 1)) # coefficient of y (= x^0.y^1)
#' getCoefficient(p, 0)       # the constant term
#' getCoefficient(p, 3)       # coefficient of x^3
setMethod(
  "getCoefficient", c("symbolicQspray", "numeric"),
  function(qspray, exponents) {
    stopifnot(isExponents(exponents))
    exponents <- removeTrailingZeros(exponents)
    n <- arity(qspray)
    if(length(exponents) > n) {
      coeff <- as.ratioOfQsprays(0L)
    } else {
      powers <- vapply(qspray@powers, function(pows) {
        toString(grow(pows, n))
      }, character(1L))
      i <- match(toString(grow(exponents, n)), powers)
      if(is.na(i)) {
        coeff <- as.ratioOfQsprays(0L)
      } else {
        coeff <- qspray@coeffs[[i]]
      }
    }
    sSQ <- getShowSymbolicQspray(qspray)
    showRatioOfQspraysOption(coeff, "showRatioOfQsprays") <-
      attr(sSQ, "showRatioOfQsprays")
    coeff
  }
)

#' @name getConstantTerm
#' @aliases getConstantTerm,symbolicQspray-method
#' @docType methods
#' @importFrom qspray getConstantTerm
#' @title Get the constant term of a 'symbolicQspray' polynomial
#' @description Get the constant term of a \code{symbolicQspray} polynomial.
#'
#' @param qspray a \code{symbolicQspray} object
#'
#' @return A \code{ratioOfQsprays} object.
#' @export
setMethod(
  "getConstantTerm", "symbolicQspray",
  function(qspray) {
    getCoefficient(qspray, integer(0L))
  }
)

#' @name isConstant
#' @aliases isConstant,symbolicQspray-method
#' @docType methods
#' @importFrom qspray isConstant
#' @title Whether a 'symbolicQspray' polynomial is constant
#' @description Checks whether a \code{symbolicQspray} object defines a constant
#'   polynomial.
#'
#' @param x a \code{symbolicQspray} object
#'
#' @return A Boolean value.
#' @export
setMethod(
  "isConstant", "symbolicQspray",
  function(x) {
    numberOfVariables(x) == 0L
  }
)

#' @name isUnivariate
#' @aliases isUnivariate,symbolicQspray-method
#' @docType methods
#' @importFrom qspray isUnivariate
#' @title Whether a 'symbolicQspray' polynomial is univariate
#' @description Checks whether a \code{symbolicQspray} object defines a
#'   univariate polynomial.
#'
#' @param x a \code{symbolicQspray} object
#'
#' @return A Boolean value.
#' @export
#' @note It is considered that a constant \code{symbolicQspray} is univariate.
setMethod(
  "isUnivariate", "symbolicQspray",
  function(x) {
    numberOfVariables(x) %in% c(0L, 1L)
  }
)

#' @name isQzero
#' @aliases isQzero,symbolicQspray-method
#' @docType methods
#' @importFrom qspray isQzero
#' @title Whether a 'symbolicQspray' polynomial is null
#' @description Checks whether a \code{symbolicQspray} object defines the zero
#'   polynomial.
#'
#' @param qspray a \code{symbolicQspray} object
#'
#' @return A Boolean value.
#' @export
setMethod(
  "isQzero", "symbolicQspray",
  function(qspray) {
    length(qspray@powers) == 0L
  }
)

#' @name isQone
#' @aliases isQone,symbolicQspray-method
#' @docType methods
#' @importFrom qspray isQone
#' @title Whether a 'symbolicQspray' polynomial is the unit polynomial
#' @description Checks whether a \code{symbolicQspray} object defines the unit
#'   polynomial.
#'
#' @param qspray a \code{symbolicQspray} object
#'
#' @return A Boolean value.
#' @export
setMethod(
  "isQone", "symbolicQspray",
  function(qspray) {
    length(qspray@powers) == 1L && getConstantTerm(qspray) == 1L
  }
)

#' @title Number of parameters
#' @description Number of parameters of a \code{symbolicQspray} polynomial,
#'   i.e. the number of variables occurring in its coefficients.
#'
#' @param Qspray a \code{symbolicQspray} object
#'
#' @return An integer, the number of parameters involved in (the
#'   coefficients of) \code{Qspray}.
#' @export
#' @importFrom ratioOfQsprays numberOfVariables
#'
#' @examples
#' JP <- JacobiPolynomial(4) # Jacobi polynomials have two parameters
#' numberOfParameters(JP)
numberOfParameters <- function(Qspray) {
  max(c(0L, vapply(Qspray@coeffs, numberOfVariables, integer(1L))))
}

#' @title Whether the coefficients of a 'symbolicQspray' polynomially depend
#'   on its parameters
#' @description Checks whether the dependence of the coefficients of a
#'   \code{symbolicQspray} polynomial on their parameters is polynomial.
#'
#' @param Qspray a \code{symbolicQspray} object
#'
#' @return A Boolean value. The coefficients of a \code{symbolicQspray}
#'   polynomial always are fractions of polynomials. This function checks
#'   whether they are polynomials.
#' @export
#' @importFrom ratioOfQsprays isPolynomial
#'
#' @examples
#' JP <- JacobiPolynomial(4)
#' hasPolynomialCoefficientsOnly(JP)
hasPolynomialCoefficientsOnly <- function(Qspray) {
  all(vapply(Qspray@coeffs, isPolynomial, logical(1L)))
}
