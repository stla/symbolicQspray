#' @title Print a 'symbolicQspray' object
#' @description Print a \code{symbolicQspray} object given a function to print
#'   a \code{ratioOfQsprays} object.
#'
#' @param showRatioOfQsprays a function which prints a \code{ratioOfQsprays}
#'   object
#' @param X a string, usually a letter, to denote the non-indexed variables
#'
#' @return A function which prints a \code{symbolicQspray} object.
#' @export
#'
#' @seealso \code{\link{showSymbolicQsprayX1X2X3}},
#'   \code{\link{showSymbolicQsprayXYZ}}.
showSymbolicQspray <- function(showRatioOfQsprays, showMonomial) {
  f <- function(qspray) {
    if(isQzero(qspray)) {
      return("0")
    }
    qspray <- orderedQspray(qspray)
    monomials <- vapply(qspray@powers, showMonomial, FUN.VALUE = character(1L))
    coeffs <- paste0(
      "{ ",
      vapply(qspray@coeffs, showRatioOfQsprays, character(1L)),
      " }"
    )
    nterms <- numberOfTerms(qspray)
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
#' @param a a string, usually a letter, to denote the non-indexed variables
#'   of the \code{ratioOfQsprays} coefficients
#' @param X a string, usually a letter, to denote the non-indexed variables
#' @param quotientBar a string for the quotient bar between the numerator and
#'   the denominator of a \code{ratioOfQsprays} object, including surrounding
#'   spaces, e.g. \code{"/"}
#' @param ... arguments other than \code{showRatioOfQsprays} and
#'   \code{showMonomial} passed to \code{\link{showSymbolicQspray}}
#'   (currently there is no such argument)
#'
#' @return A function which prints \code{symbolicQspray} objects.
#' @export
#' @importFrom ratioOfQsprays showRatioOfQspraysX1X2X3
#' @importFrom qspray showMonomialX1X2X3
#'
#' @examples
#' set.seed(421)
#' Qspray <- rSymbolicQspray()
#' showSymbolicQsprayX1X2X3(quotientBar = " / ")(Qspray)
showSymbolicQsprayX1X2X3 <- function(
    a = "a", X = "X", quotientBar = " %//% ", ...
) {
  f <- showSymbolicQspray(
    showRatioOfQspraysX1X2X3(var = a, quotientBar = quotientBar),
    showMonomialX1X2X3(X), ...
  )
  attr(f, "showRatioOfQsprays") <-
    showRatioOfQspraysX1X2X3(var = a, quotientBar = quotientBar)
  attr(f, "showMonomial")       <- showMonomialX1X2X3(X)
  # attr(f, "X1X2X3") <- TRUE
  # attr(attr(f, "X1X2X3"), "a") <- a
  # attr(attr(f, "X1X2X3"), "X") <- X
  # attr(attr(f, "X1X2X3"), "quotientBar") <- quotientBar
  f
}

#' @title Print a 'symbolicQspray' object
#' @description Print a \code{symbolicQspray} object.
#'
#' @param a a string, usually a letter, to denote the non-indexed variables
#'   of the \code{ratioOfQsprays} coefficients
#' @param letters a vector of strings, usually some letters, to denote the
#'   variables of the polynomial
#' @param quotientBar a string for the quotient bar between the numerator and
#'   the denominator of a \code{ratioOfQsprays} object, including surrounding
#'   spaces, e.g. \code{" / "}
#' @param ... arguments other than \code{showRatioOfQsprays} and
#'   \code{showMonomial} passed to \code{\link{showSymbolicQspray}}
#'   (currently there is no such argument)
#'
#' @return A function which prints \code{symbolicQspray} objects.
#' @export
#' @importFrom ratioOfQsprays showRatioOfQspraysX1X2X3
#' @importFrom qspray showMonomialXYZ
#'
#' @examples
#' set.seed(421)
#' Qspray <- rSymbolicQspray()
#' showSymbolicQsprayX1X2X3(quotientBar = " / ")(Qspray)
showSymbolicQsprayXYZ <- function(
    a = "a", letters = c("X", "Y", "Z"), quotientBar = " %//% ", ...
) {
  showSymbolicQspray(
    showRatioOfQspraysX1X2X3(a = a, quotientBar = quotientBar),
    showMonomialXYZ(letters), ...
  )
}

#' @title Set show option to a 'qspray' object
#' @description Set show option to a \code{symbolicQspray} object
#'
#' @param x a \code{symbolicQspray} object
#' @param which which option to set; this can be \code{"a"}, \code{"X"}
#'   \code{"showMonomial"}, \code{"showRatioOfQsprays"} or
#'   \code{"showSymbolicQspray"}
#' @param value the value for the option
#'
#' @return This returns the updated \code{symbolicQspray}.
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
      c(
        "a", "X", "quotientBar", "showMonomial",
        "showRatioOfQsprays", "showSymbolicQspray", "inheritable"
      )
    )
  showOpts <- attr(x, "showOpts") %||% TRUE
  attr(showOpts, which) <- value
  if(which == "inheritable") {
    attr(x, "showOpts") <- showOpts
    return(x)
  }
  inheritable <- attr(showOpts, "inheritable") %||% FALSE
  univariate <- isUnivariate(x)
  if(univariate) {
    sM0 <- showMonomialXYZ(attr(showOpts, "X") %||% "X")
    inheritable <- FALSE
  } else {
    trivariate <- numberOfVariables(x) <= 3L
    if(trivariate && is.null(attr(showOpts, "X"))) {
      sM0 <- showMonomialXYZ(c("X", "Y", "Z"))
      inheritable <- FALSE
    } else {
      sM0 <- showMonomialX1X2X3(attr(showOpts, "X") %||% "X")
    }
  }
  univariate <- all(vapply(x@coeffs, isUnivariate, logical(1L)))
  if(univariate) {
    sROQ0 <- showRatioOfQspraysXYZ(
      letters = attr(showOpts, "a") %||% "a",
      quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
    )
    inheritable <- FALSE
  } else {
    sROQ0 <- showRatioOfQspraysX1X2X3(
      attr(showOpts, "a") %||% "a",
      quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
    )
  }
  if(which == "a" || which == "quotientBar") {
    sM <- attr(showOpts, "showMonomial") %||% sM0
    sROQ <- sROQ0
    f <- showSymbolicQspray(sROQ, sM)
  } else if(which == "X") {
    sROQ <- attr(showOpts, "showRatioOfQsprays") %||% sROQ0
    sM <- sM0
    f <- showSymbolicQspray(sROQ, sM)
  } else if(which == "showSymbolicQspray") {
    f <- value
    sM <- attr(showOpts, "showMonomial") %||% attr(f, "showMonomial")
    sROQ <- attr(showOpts, "showRatioOfQsprays") %||%
      attr(f, "showRatioOfQsprays")
  } else if(which == "showRatioOfQsprays") {
    sM <- attr(showOpts, "showMonomial") %||% sM0
    sROQ <- value
    f <- showSymbolicQspray(
      showRatioOfQsprays = sROQ,
      showMonomial = sM
    )
  } else if(which == "showMonomial") {
    sM <- value
    sROQ <- attr(showOpts, "showRatioOfQsprays") %||% sROQ0
    f <- showSymbolicQspray(
      showRatioOfQsprays = sROQ,
      showMonomial = sM
    )
  }
  sM   -> attr(showOpts, "showMonomial")
  sROQ -> attr(showOpts, "showRatioOfQsprays")
  attr(showOpts, "showSymbolicQspray") <- f
  attr(showOpts, "inheritable") <- inheritable
  attr(x, "showOpts") <- showOpts
  x
}

setDefaultShowSymbolicQsprayOption <- function(qspray) {
  trivariate <- numberOfVariables(qspray) <= 3L
  if(trivariate){
    showSymbolicQsprayOption(qspray, "showMonomial") <-
      showMonomialXYZ(c("X", "Y", "Z"))
    showSymbolicQsprayOption(qspray, "inheritable") <- FALSE
  } else {
    showSymbolicQsprayOption(qspray, "inheritable") <- TRUE
    showSymbolicQsprayOption(qspray, "X") <- "X"
  }
  invisible(qspray)
}

getShowSymbolicQspray <- function(Qspray) {
  showOpts <- attr(Qspray, "showOpts")
  sSQ <- attr(showOpts, "showSymbolicQspray")
  if(is.null(sSQ)) {
    Qspray <- setDefaultShowSymbolicQsprayOption(Qspray)
    sSQ <- attr(attr(Qspray, "showOpts"), "showSymbolicQspray")
  }
  sSQ
}

getShowSymbolicQsprayCoefficient <- function(Qspray) {
  showOpts <- attr(Qspray, "showOpts")
  attr(showOpts, "showRatioOfQsprays") %||%
    showRatioOfQspraysX1X2X3(
      attr(showOpts, "a") %||% "a",
      quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
    )
}
