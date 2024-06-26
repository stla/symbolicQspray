#' @title Evaluation of a 'symbolicQspray' object
#' @description Evaluates a \code{symbolicQspray} object by substituting some
#'   values to the exterior variables or the main variables or both.
#'
#' @param Qspray a \code{symbolicQspray} object
#' @param a vector of values to be substituted to the exterior variables:
#'   these values must be coercable to \code{bigq} numbers
#' @param X list of values to be substituted to the main variables: these
#'   values must be coercable to \code{ratioOfQsprays} objects
#'
#' @return If both \code{a} and \code{X} are \code{NULL}, this returns the
#'   input \code{symbolicQspray} object; otherwise, if \code{a} is not
#'   \code{NULL}, this returns a \code{symbolicQspray} object, and if
#'   \code{X} is not \code{NULL}, this returns a \code{ratioOfQsprays} object.
#' @export
#' @importFrom ratioOfQsprays evalRatioOfQsprays showRatioOfQspraysOption<-
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
#' ( Q <- evalSymbolicQspray(Qspray, a = a) )
#' ( roq <- evalSymbolicQspray(Qspray, X = X) )
#' evalSymbolicQspray(Qspray, a = a, X = X)
#' evalSymbolicQspray(Q, X = X)
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
      passShowAttributes(Qspray, Q)
    } else { # both 'a' and 'X' are not NULL
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
      sSQ <- getShowSymbolicQspray(Qspray)
      showRatioOfQspraysOption(roq, "showRatioOfQsprays") <-
        attr(sSQ, "showRatioOfQsprays")
      roq
    }
  } else if(!is.null(X)){ # 'a' is NULL
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
    sSQ <- getShowSymbolicQspray(Qspray)
    showRatioOfQspraysOption(roq, "showRatioOfQsprays") <-
      attr(sSQ, "showRatioOfQsprays")
    roq
  } else { # both 'a' and 'X' are NULL
    Qspray
  }
}

test_that("evaluation", {
  f <- function(a1, a2, X1, X2, X3) {
    ((a1/(a2^2+1)) * X1^2*X2)^2  +  (a2+1) * X3  +  5L
  }
  a1 <- qlone(1)
  a2 <- qlone(2)
  X1 <- Qlone(1)
  X2 <- Qlone(2)
  X3 <- Qlone(3)
  Qspray <- f(a1, a2, X1, X2, X3)
  library(gmp)
  a <- as.bigq(c(2L, 3L))
  X <- as.bigq(c(-4L, 3L, 2L))
  Q <- evalSymbolicQspray(Qspray, a = a)
  expect_true(Q == f(a[1], a[2], X1, X2, X3))
  ratioOfQsprays <- evalSymbolicQspray(Qspray, X = X)
  expect_true(ratioOfQsprays == f(a1, a2, X[1], X[2], X[3]))
  result <- evalSymbolicQspray(Qspray, a = a, X = X)
  expect_true(result == evalSymbolicQspray(Q, X = X))
  expect_true(result == evalRatioOfQsprays(ratioOfQsprays, a))
  #
  X <- list(qlone(1)/(1+qlone(1)), qlone(2)/(qlone(1)^2), qlone(3)/qlone(1))
  result <- evalSymbolicQspray(Qspray, a = a, X = X)
  Q <- evalSymbolicQspray(Qspray, a = a)
  expect_true(evalSymbolicQspray(Q, X = X) == result)
})
