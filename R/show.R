#' @title Print a 'symbolicQspray' object
#' @description Print a \code{symbolicQspray} object given a function to print
#'   a \code{ratioOfQsprays} object.
#'
#' @param showRatioOfQsprays a function which prints a \code{ratioOfQsprays}
#'   object
#' @param showMonomial a function which prints a monomial, such as
#'  \code{\link[qspray:showMonomialXYZ]{showMonomialXYZ()}} (and not
#'  \code{showMonomialXYZ}!)
#' @param lbrace,rbrace used to enclose the coefficients
#' @param addition used to separate the terms
#' @param multiplication used to separate the coefficient and the monomial
#'   within a term
#'
#' @return A function which prints a \code{symbolicQspray} object.
#' @export
#'
#' @seealso \code{\link{showSymbolicQsprayX1X2X3}},
#'   \code{\link{showSymbolicQsprayXYZ}}.
#'
#' @examples
#' set.seed(421)
#' ( Qspray <- rSymbolicQspray() )
#' showRatioOfQsprays <-
#'   showRatioOfQspraysXYZ(c("a", "b", "c"), quotientBar = " / ")
#' showMonomial <- showMonomialX1X2X3("X")
#' f <- showSymbolicQspray(showRatioOfQsprays, showMonomial, "{{{", "}}}")
#' f(Qspray)
showSymbolicQspray <- function(
  showRatioOfQsprays, showMonomial, lbrace = "{ ", rbrace = " }",
  addition = "  +  ", multiplication = " * "
) {
  f <- function(qspray) {
    if(isQzero(qspray)) {
      return("0")
    }
    qspray <- orderedQspray(qspray)
    monomials <- vapply(qspray@powers, showMonomial, FUN.VALUE = character(1L))
    coeffs <- paste0(
      lbrace,
      vapply(qspray@coeffs, showRatioOfQsprays, character(1L)),
      rbrace
    )
    nterms <- numberOfTerms(qspray)
    if(monomials[nterms] == "") {
      toPaste <- c(
        sprintf("%s%s%s", coeffs[-nterms], multiplication, monomials[-nterms]),
        coeffs[nterms]
      )
    } else {
      toPaste <- paste0(coeffs, multiplication, monomials)
    }
    paste0(toPaste, collapse = addition)
  }
  attr(f, "showRatioOfQsprays") <- showRatioOfQsprays
  attr(f, "showMonomial")       <- showMonomial
  attr(f, "inheritable") <-
    c("SROQ" = isTRUE(attr(showRatioOfQsprays, "inheritable")),
      "SM" = isTRUE(attr(showMonomial, "inheritable")))
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
    showRatioOfQspraysX1X2X3(a, quotientBar = quotientBar),
    showMonomialX1X2X3(X), ...
  )
  attr(f, "showRatioOfQsprays") <-
    showRatioOfQspraysX1X2X3(a, quotientBar = quotientBar)
  attr(f, "showMonomial")       <- showMonomialX1X2X3(X)
  attr(f, "inheritable") <- TRUE
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
#' @param which which option to set; this can be \code{"a"}, \code{"X"},
#'   \code{"quotientBar"}, \code{"showMonomial"}, \code{"showRatioOfQsprays"} or
#'   \code{"showSymbolicQspray"}
#' @param value the value for the option
#'
#' @return This returns the updated \code{symbolicQspray}.
#' @export
#'
#' @examples
#' set.seed(421)
#' Qspray <- rSymbolicQspray()
#' showSymbolicQsprayOption(Qspray, "a") <- "x"
#' showSymbolicQsprayOption(Qspray, "X") <- "A"
#' showSymbolicQsprayOption(Qspray, "quotientBar") <- " / "
#' Qspray
#' showSymbolicQsprayOption(Qspray, "showRatioOfQsprays") <-
#'   showRatioOfQspraysXYZ()
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
  if(!is.element(which, c("showMonomial", "showRatioOfQsprays"))) {
    attr(showOpts, which) <- value
    if(which == "inheritable") {
      attr(x, "showOpts") <- showOpts
      return(x)
    }
  }
  sSQ  <- attr(showOpts, "showSymbolicQspray")
  sMdefault <- showMonomialX1X2X3(attr(showOpts, "X") %||% "X")
  attr(sMdefault, "showUnivariate") <-
    attr(sMdefault, "showTrivariate") <- showMonomialXYZ(c("X", "Y", "Z"))
  sM   <- attr(sSQ, "showMonomial") %||% sMdefault
  sROQ <- attr(sSQ, "showRatioOfQsprays") %||%
    showRatioOfQspraysX1X2X3(
      attr(showOpts, "a") %||% "a",
      quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
    )
  if(which == "a" || which == "quotientBar") {
    sROQ <- showRatioOfQspraysX1X2X3(
      attr(showOpts, "a") %||% "a",
      quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
    )
  } else if(which == "showRatioOfQsprays") {
    sROQ <- value
  } else if(which == "X") {
    sM <- showMonomialX1X2X3(value)
    attr(sM, "showUnivariate") <- showMonomialXYZ(value)
  } else if(which == "showMonomial") {
    sM <- value
  }
  if(which != "showSymbolicQspray") {
    sM0 <- sM
    sMU0 <- attr(sM0, "showUnivariate") %||% sM0
      # showMonomialXYZ(attr(showOpts, "X") %||% "X")
    attr(sM0, "showUnivariate") <- sMU0
    sMT0 <- attr(sM0, "showTrivariate")
    if(is.null(sMT0)) {
      sMT0 <- sM0
      # if(is.null(attr(showOpts, "X"))) {
      #   sMT0 <- showMonomialXYZ(c("X", "Y", "Z"))
      # } else {
      #   sMT0 <- showMonomialX1X2X3(attr(showOpts, "X"))
      # }
    }
    attr(sM0, "showTrivariate") <- sMT0
    inheritableM <- isTRUE(attr(sM0, "inheritable")) # is it true?..
    sROQ0 <- sROQ
    inheritableROQ <- isTRUE(attr(sROQ0, "inheritable")) # is it true?..
    sROQU0 <- attr(sROQ0, "showUnivariate")
    if(is.null(sROQU0)) {
      sROQU0 <- showRatioOfQspraysXYZ(
        attr(showOpts, "a") %||% "a",
        quotientBar = attr(showOpts, "quotientBar") %||% " %//% "
      )
    }
    attr(sROQ0, "showUnivariate") <- sROQU0
    f <- function(Qspray) {
      univariate <- isUnivariate(Qspray)
      trivariate <- numberOfVariables(Qspray) <= 3L
      if(univariate) {
        sM <- sMU0
      } else if(trivariate) {
        sM <- sMT0
      } else {
        sM <- sM0
      }
      univariate <- all(vapply(Qspray@coeffs, isUnivariate, logical(1L)))
      if(univariate) {
        sROQ <- sROQU0
      } else {
        sROQ <- sROQ0
      }
      showSymbolicQspray(sROQ, sM)(Qspray)
    }
    inheritable <- c("SROQ" = inheritableROQ, "SM" = inheritableM)
    attr(f, "showRatioOfQsprays") <- sROQ0
    attr(f, "showMonomial") <- sM0
  } else {
    f <- value
    inheritable <- attr(f, "inheritable")
  }
  attr(showOpts, "showSymbolicQspray") <- f
  attr(showOpts, "inheritable") <- inheritable
  attr(x, "showOpts") <- showOpts
  x
}

setDefaultShowSymbolicQsprayOption <- function(Qspray) {
  # sM <- showMonomialX1X2X3("X")
  # attr(sM, "showUnivariate") <-
  #   attr(sM, "showTrivariate") <- showMonomialXYZ(c("X", "Y", "Z"))
  # showSymbolicQsprayOption(Qspray, "showMonomial") <- sM
  showSymbolicQsprayOption(Qspray, "quotientBar") <- " %//% "
  invisible(Qspray)
}

getShowSymbolicQspray <- function(Qspray) {
  showOpts <- attr(Qspray, "showOpts")
  sSQ <- attr(showOpts, "showSymbolicQspray")
  if(is.null(sSQ)) {
    Qspray <- setDefaultShowSymbolicQsprayOption(Qspray)
    sSQ <- attr(attr(Qspray, "showOpts"), "showSymbolicQspray")
    attr(sSQ, "inheritable") <- TRUE
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
