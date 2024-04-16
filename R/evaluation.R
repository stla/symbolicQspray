#' @title Evaluation of a 'symbolicQspray' object
#' @description Evaluates a \code{symbolicQspray} object by substituting some
#'   values to the exterior variables or the main variables or both.
#'
#' @param Qspray a \code{symbolicQspray} object
#' @param a vector of values to be substituted to the exterior variables:
#'   these values must be coercable to \code{bigq} numbers
#' @param X vector of values to be substituted to the main variables: these
#'   values must be coercable to \code{bigq} numbers
#'
#' @return If both \code{a} and \code{X} are \code{NULL}, this returns the
#'   input \code{symbolicQspray} object; otherwise, if \code{a} is not
#'   \code{NULL}, this returns a \code{qspray} object, and if
#'   \code{X} is not \code{NULL}, this returns a \code{ratioOfQsprays} object.
#' @export
#' @importFrom ratioOfQsprays evalRatioOfQsprays showRatioOfQspraysOption<-
#' @importFrom qspray showQsprayOption<- evalQspray
#' @importFrom gmp c_bigq as.bigq
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
evalSymbolicQspray <- function(Qspray, a = NULL, X = NULL) {
  if(!is.null(a)) {
    coeffs <- c_bigq(lapply(Qspray@coeffs, evalRatioOfQsprays, values_re = a))
    qspray <- new(
      "qspray",
      powers = Qspray@powers,
      coeffs = as.character(coeffs)
    )
    if(is.null(X)) { # 'X' is NULL and 'a' is not NULL
      sSQ <- getShowSymbolicQspray(Qspray)
      showQsprayOption(qspray, "showQspray") <- attr(sSQ, "showQspray")
      qspray
    } else { # both 'a' and 'X' are not NULL
      evalQspray(qspray, values_re = X)
    }
  } else if(!is.null(X)){ # 'X' is not NULL and 'a' is NULL
    X <- as.bigq(X)
    if(anyNA(X)) {
      stop("Invalid values in `X`.")
    }
    monomials <- lapply(Qspray@powers, function(exponents) {
      if(length(exponents) != 0L) {
        powers <- lapply(which(exponents != 0L), function(i) {
          X[[i]]^exponents[i]
        })
        Reduce(`*`, powers)
      } else {
        as.bigq(1L)
      }
    })
    coeffs <- Qspray@coeffs
    roq <- as.ratioOfQsprays(0L)
    for(i in seq_along(coeffs)) {
      roq <- roq + monomials[[i]]*coeffs[[i]]
    }
    sSQ <- getShowSymbolicQspray(Qspray)
    showRatioOfQspraysOption(roq, "showRatioOfQsprays") <-
      attr(sSQ, "showRatioOfQsprays")
    roq
  } else { # both 'a' and 'X' are NULL
    Qspray
  }
}
