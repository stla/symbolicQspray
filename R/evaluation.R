#' @title Evaluation of a 'symbolicQspray' object
#' @description Evaluates a \code{symbolicQspray} object by substituting some
#'   values to the inner variables or the main variables or both.
#'
#' @param Qspray a \code{symbolicQspray} object
#' @param a values to be substituted to the inner variables
#' @param X values to be substituted to the main variables
#'
#' @return If both \code{a} and \code{X} are \code{NULL}, this returns the
#'   input \code{symbolicQspray} object; otherwise, if \code{a} is not
#'   \code{NULL}, this returns a \code{qspray} object, and if \code{X} is not
#'   \code{NULL}, this returns a \code{ratioOfQsprays} object.
#' @export
#' @importFrom qspray evalQspray
#' @importFrom ratioOfQsprays evalRatioOfQsprays
#' @importFrom gmp c_bigq
#'
#' @examples
#' library(symbolicQspray)
#' a1 <- qlone(1)
#' a2 <- qlone(2)
#' X1 <- Qlone(1)
#' X2 <- Qlone(2)
#' X3 <- Qlone(3)
#' ( Qspray <- (a1 + 2)*X1^2*X2 + (a2/(a1^2+a2))*X1*X2*X3 )
#' a <- c(2, 3)
#' X <- c(4, 3, 2)
#' ( qspray <- evalSymbolicQspray(Qspray, a = a) )
#' ( roq <- evalSymbolicQspray(Qspray, X = X) )
#' evalSymbolicQspray(Qspray, a = a, X = X)
#' evalQspray(qspray, X)
#' evalRatioOfQsprays(roq, a)
#' # More generally, X is a list of `ratioOfQsprays` objects
#' X <- list(qlone(1)/(1+qlone(1)), qlone(2)/(qlone(1)^2), qlone(3)/qlone(1))
#' evalSymbolicQspray(Qspray, a = a, X = X)   # ratioOfQsprays
#' ( Q <- evalSymbolicQspray(Qspray, a = a) ) # symbolicQspray
#' evalSymbolicQspray(Q, X = X)               # ratioOfQsprays
evalSymbolicQspray <- function(Qspray, a = NULL, X = NULL) {
  if(!is.null(a)) {
    coeffs <- c_bigq(lapply(Qspray@coeffs, evalRatioOfQsprays, values_re = a))
    Q <- new(
      "symbolicQspray",
      powers = Qspray@powers,
      coeffs = lapply(coeffs, as.ratioOfQsprays)
    )
    if(is.null(X)) {
      attr(Q, "showOpts") <- attr(Qspray, "showOpts")
      Q
    } else {
      # evalQspray(qspray, values_re = X)
      monomials <- lapply(Q@powers, function(exponents) {
        if(length(exponents) != 0L) {
          powers <- lapply(which(exponents != 0L), function(i) {
            X[[i]]^exponents[i]
          })
          Reduce(`*`, powers)
        } else {
          as.ratioOfQsprays(1L)
        }
      })
      coeffs <- Q@coeffs
      roq <- as.ratioOfQsprays(0L)
      for(i in seq_along(coeffs)) {
        roq <- coeffs[[i]] * monomials[[i]] + roq
      }
      #attr(roq, "showOpts") <- attr(coeffs[[1]], "showOpts")
      roq
    }
  } else if(!is.null(X)){
    monomials <- lapply(Qspray@powers, function(exponents) {
      if(length(exponents) != 0L) {
        powers <- lapply(which(exponents != 0L), function(i) {
          X[[i]]^exponents[i]
        })
        Reduce(`*`, powers)
      } else {
        as.ratioOfQsprays(1L)
      }
    })
    coeffs <- Qspray@coeffs
    roq <- as.ratioOfQsprays(0L)
    for(i in seq_along(coeffs)) {
      roq <- coeffs[[i]] * monomials[[i]] + roq
    }
    #attr(roq, "showOpts") <- attr(coeffs[[1]], "showOpts")
    roq
  } else {
    Qspray
  }
}
