#' @title Print a 'symbolicQspray' object
#' @description Print a \code{symbolicQspray} object given a function to print
#'   a \code{ratioOfQsprays} object.
#'
#' @param showRatioOfQsprays a function which prints a \code{ratioOfQsprays}
#'   object
#' @param X a string, usually a letter, to denote the unindexed variables
#'
#' @return A functions which prints a \code{symbolicQspray} object.
#' @export
#'
#' @seealso \code{\link{showSymbolicQsprayCanonical}}
showSymbolicQspray <- function(showRatioOfQsprays, showMonomial) {
  f <- function(qspray) {
    if(length(qspray@coeffs) == 0L) {
      return("0")
    }
    qspray <- orderedQspray(qspray)
    monomials <- vapply(qspray@powers, showMonomial, FUN.VALUE = character(1L))
    coeffs <- paste0(
      "{ ",
      vapply(qspray@coeffs, showRatioOfQsprays, character(1L)),
      " }"
    )
    nterms <- length(coeffs)
    if(monomials[nterms] == "") {
      toPaste <- c(
        sprintf("%s * %s", coeffs[-nterms], monomials[-nterms]),
        coeffs[nterms]
      )
    } else {
      toPaste <- paste0(coeffs, " * ", monomials)
    }
    paste0(toPaste, collapse = "  +  ")
  }
  attr(f, "showRatioOfQsprays") <- showRatioOfQsprays
  attr(f, "showMonomial")       <- showMonomial
  f
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
#' @importFrom qspray showMonomialCanonical
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
#' set.seed(421)
#' Qspray <- rSymbolicQspray()
#' showSymbolicQsprayCanonical(quotientBar = " / ")(Qspray)
showSymbolicQsprayCanonical <- function(
    a = "a", X = "X", quotientBar = " %//% ", ...
) {
  f <- showSymbolicQspray(
    showRatioOfQspraysCanonical(var = a, quotientBar = quotientBar),
    showMonomialCanonical(X), ...
  )
  attr(f, "showRatioOfQsprays") <-
    showRatioOfQspraysCanonical(var = a, quotientBar = quotientBar)
  attr(f, "showMonomial")       <- showMonomialCanonical(X)
  # attr(f, "canonical") <- TRUE
  # attr(attr(f, "canonical"), "a") <- a
  # attr(attr(f, "canonical"), "X") <- X
  # attr(attr(f, "canonical"), "quotientBar") <- quotientBar
  f
}

#' @title Set show options to a 'symbolicQspray' object
#' @description Set some attributes to a \code{symbolicQspray} object
#'   to control the way it is displayed. See the note in the
#'   documentation of \code{\link{showSymbolicQsprayCanonical}} for details.
#'
#' @param Qspray a \code{symbolicQspray} object
#' @param a value for the \code{"a"} attribute
#' @param X value for the \code{"X"} attribute
#' @param quotientBar value for the \code{"quotientBar"} attribute
#'
#' @return The input \code{symbolicQspray} object with new attributes.
#' @export
#'
#' @examples
#' ( Qspray <- rSymbolicQspray() )
#' withAttributes(Qspray, a = "x", X = "A", quotientBar = " / ")
withAttributes <- function(
    Qspray, a = "a", X = "X", quotientBar = " %//% "
) {
  attr(Qspray, "a") <- a
  attr(Qspray, "X") <- X
  attr(Qspray, "quotientBar") <- quotientBar
  Qspray
}

#' @title Set show option to a 'qspray' object
#' @description Set show option to a \code{qspray} object
#'
#' @param x a \code{qspray} object
#' @param which which option to set; this can be \code{"x"},
#'   \code{"showMonomial"}, or \code{"showQspray"}
#' @param value the value of the option
#'
#' @return This returns the updated \code{qspray}.
#' @export
#'
#' @examples
#' Qspray <- rSymbolicQspray()
#' showSymbolicQsprayOption(Qspray, "a") <- "x"
#' showSymbolicQsprayOption(Qspray, "X") <- "A"
#' showSymbolicQsprayOption(Qspray, "quotientBar") <- " / "
#' Qspray
`showSymbolicQsprayOption<-` <- function(x, which, value) {
  which <-
    match.arg(
      which,
      c("a", "X", "quotientBar", "showMonomial", "showRatioOfQsprays", "showSymbolicQspray")
    )
  showOpts <- attr(x, "showOpts") %||% TRUE
  if(which == "showSymbolicQspray") {
    f <- value
    attr(showOpts, "showRatioOfQsprays") <-
      attr(showOpts, "showRatioOfQsprays") %||%
      attr(f, "showRatioOfQsprays") %||%
      showRatioOfQspraysCanonical(
        attr(showOpts, "a") %||% "a",
        quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
      )
    attr(showOpts, "showMonomial") <-
      attr(showOpts, "showMonomial") %||%
      attr(f, "showMonomial") %||%
      showMonomialCanonical(
        attr(showOpts, "X") %||% "X"
      )
  } else if(which == "showRatioOfQsprays") {
    f <- showSymbolicQspray(
      showRatioOfQsprays = value,
      showMonomial = showMonomialCanonical(
        attr(showOpts, "X") %||% attr(value, "X") %||% "X"
      )
    )
    attr(showOpts, "showMonomial") <-
      attr(showOpts, "showMonomial") %||%
      showMonomialCanonical(
        attr(showOpts, "X") %||% "X"
      )
  } else if(which == "showMonomial") {
    f <- showSymbolicQspray(
      showRatioOfQsprays = attr(showOpts, "showRatioOfQsprays") %||%
        showRatioOfQspraysCanonical(
          attr(showOpts, "a") %||% "a",
          quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
        )
    )
  }
  # # attr(showOpts, "a") <- attr(showOpts, "a") %||% "a"
  # attr(showOpts, which) <- value
  # if(which %in% c("a", "X", "quotientBar")) {
  #   f <- showSymbolicQsprayCanonical(
  #     a = attr(showOpts, "a") %||% "a",
  #     X = attr(showOpts, "X") %||% "X",
  #     quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
  #   )
  #   attr(showOpts, "showRatioOfQsprays") <-
  #     showRatioOfQspraysCanonical(
  #       attr(showOpts, "a") %||% "a",
  #       quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
  #     )
  #   # attr(whichOpts, "showQspray") <-
  #   #   showQsprayCanonical(attr(showOpts, "X") %||% "X")
  # } else if(which == "showRatioOfQsprays") {
  #   f <- showSymbolicQspray(
  #     showRatioOfQsprays = value,
  #     X = attr(showOpts, "X") %||% "X"
  #   )
  #   # attr(whichOpts, "showQspray") <-
  #   #   showQsprayCanonical(attr(showOpts, "X") %||% "X")
  # } else {
  #   f <- value
  # }
  attr(showOpts, "showSymbolicQspray") <- f
  attr(x, "showOpts") <- showOpts
  x
}

getShowSymbolicQspray <- function(Qspray) {
  showOpts <- attr(Qspray, "showOpts")
  attr(showOpts, "showSymbolicQspray") %||%
    showSymbolicQspray(
      showRatioOfQsprays =
        attr(showOpts, "showRatioOfQsprays") %||%
        showRatioOfQspraysCanonical(
          attr(showOpts, "a") %||% "a",
          quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
        ),
      showMonomial =
        attr(showOpts, "showMonomial") %||%
        showMonomialCanonical(
          attr(showOpts, "X") %||% "X"
        )
    )
}

getShowSymbolicQsprayCoefficient <- function(Qspray) {
  showOpts <- attr(Qspray, "showOpts")
  attr(showOpts, "showRatioOfQsprays") %||%
    showRatioOfQspraysCanonical(
      attr(showOpts, "a") %||% "a",
      quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
    )
}
