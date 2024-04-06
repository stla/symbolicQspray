#' @title Print a 'symbolicQspray' object
#' @description Print a \code{symbolicQspray} object given a function to print
#'   a \code{ratioOfQsprays} object.
#'
#' @param showRatioOfQsprays a function which prints a \code{ratioOfQsprays}
#'   object
#' @param var a string, usually a letter, to denote the unindexed variables
#'
#' @return A functions which prints a \code{symbolicQspray} object.
#' @export
#'
#' @seealso \code{\link{showSymbolicQsprayCanonical}}
showSymbolicQspray <- function(showRatioOfQsprays, var = "X") {
  function(qspray) {
    if(length(qspray@coeffs) == 0L) {
      return("0")
    }
    qspray <- orderedQspray(qspray)
    monomials <- vapply(qspray@powers, function(x) {
      paste0(vapply(seq_along(x), function(i) {
        e <- x[i]
        if(e != 0L) {
          if(e == 1L) {
            sprintf("%s%d", var, i)
          } else {
            sprintf("%s%d^%d", var, i, e)
          }
        } else {
          ""
        }
      }, character(1L)), collapse = ".")
    }, FUN.VALUE = character(1L))
    coeffs <- paste0(
      "{ ",
      vapply(qspray@coeffs, showRatioOfQsprays, character(1L)),
      " }"
    )
    nterms <- length(coeffs)
    if(monomials[nterms] == "") {
      toPaste <- c(
        paste0(coeffs[-nterms], " * ", monomials[-nterms]),
        coeffs[nterms]
      )
    } else {
      toPaste <- paste0(coeffs, " * ", monomials)
    }
    paste0(toPaste, collapse = "  +  ")
  }
}

#' @title Print a 'symbolicQspray' object
#' @description Print a \code{symbolicQspray} object.
#'
#' @param a a string, usually a letter, to denote the unindexed variables
#'   of the \code{ratioOfQsprays} coefficients
#' @param X a string, usually a letter, to denote the unindexed variables
#' @param quotientBar a string for the quotient bar between the numerator and
#'   the denominator of a \code{ratioOfQsprays} object, including surrounding
#'   spaces, e.g. \code{"/"}
#' @param ... arguments other than \code{var} passed to
#'   \code{showSymbolicQspray} (currently there is no such argument)
#'
#' @return A function which prints \code{symbolicQspray} objects.
#' @export
#' @importFrom ratioOfQsprays showRatioOfQspraysCanonical
#'
#' @note The \code{show} method for \code{symbolicQspray} objects uses
#'   \code{showSymbolicQsprayCanonical("a", "X", " \%//\% ")} by default.
#'   But this can be controlled as follows. If a
#'   \code{symbolicQspray} object has an attribute \code{"a"}, then the value
#'   of this attribute will replace \code{"a"} in the \code{show} output.
#'   If it has a \code{"X"} attribute, then the value of this attribute will
#'   replace \code{"X"}.
#'   It is also possible to control the \code{quotientBar} argument by
#'   assigning a \code{"quotientBar"} attribute to the \code{symbolicQspray}
#'   object to be printed.
#'
#' @examples
#' Qspray <- rSymbolicQspray()
#' showSymbolicQsprayCanonical(quotientBar = " / ")
showSymbolicQsprayCanonical <- function(
    a = "a", X = "X", quotientBar = " %//% ", ...
) {
  showSymbolicQspray(
    showRatioOfQspraysCanonical(var = a, quotientBar = quotientBar),
    var = X, ...
  )
}
