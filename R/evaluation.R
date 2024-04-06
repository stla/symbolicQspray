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
#' ( qspray <- evalSymbolicQspray(Qspray, a = c(2, 3)) )
#' ( roq <- evalSymbolicQspray(Qspray, X = c(4, 3, 2)) )
#' evalSymbolicQspray(Qspray, a = c(2, 3), X = c(4, 3, 2))
#' evalQspray(qspray, c(4, 3, 2))
#' evalRatioOfQsprays(roq, c(2, 3))
evalSymbolicQspray <- function(Qspray, a = NULL, X = NULL) {
  if(!is.null(a)) {
    coeffs <- c_bigq(lapply(Qspray@coeffs, evalRatioOfQsprays, values_re = a))
    qspray <- new(
      "qspray",
      powers = Qspray@powers,
      coeffs = as.character(coeffs)
    )
    if(is.null(X)) {
      if(is.null(attr(Qspray, "X"))) {
        attr(qspray, "x") <- "X"
      } else {
        attr(qspray, "X") <- attr(Qspray, "X")
      }
      qspray
    } else {
      evalQspray(qspray, values_re = X)
    }
  } else if(!is.null(X)){
    monomials <- lapply(Qspray@powers, function(exponents) {
      new("qspray", powers = list(exponents), coeffs = "1")
    })
    scalars <- lapply(monomials, evalQspray, values_re = X)
    coeffs <- Qspray@coeffs
    roq <- as.ratioOfQsprays(0L)
    for(i in seq_along(scalars)) {
      roq <- roq + scalars[[i]] * coeffs[[i]]
    }
    if(is.null(attr(Qspray, "a"))) {
      attr(roq, "x") <- "a"
    } else {
      attr(roq, "x") <- attr(Qspray, "a")
    }
    roq
  } else {
    Qspray
  }
}
