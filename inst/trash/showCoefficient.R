setGeneric(
  "showCoefficient"
)

#' @name showCoefficient
#' @aliases showCoefficient,symbolicQspray-method
#' @docType methods
#' @title Function which prints the coefficients
#' @description This method is for internal usage. For a \code{qspray}
#'   object, it returns a function which takes as argument a coefficient
#'   and which returns a string displaying this coefficient surrounded by
#'   curly braces.
#'
#' @param x a \code{symbolicQspray} object
#'
#' @return A function which associates a character string to the coefficients.
#' @export
#' @importFrom qspray showCoefficient
setMethod(
  "showCoefficient", "symbolicQspray",
  function(x) {
    f <- getShowSymbolicQsprayCoefficient(x)
    function(coeff) {
      sprintf("{ %s }", f(coeff))
    }
  }
)


getShowSymbolicQsprayCoefficient <- function(Qspray) {
  showOpts <- attr(Qspray, "showOpts")
  sSQ      <- attr(showOpts, "showSymbolicQspray")
  if(is.null(sSQ)) {
    Qspray <- setDefaultShowSymbolicQsprayOption(Qspray)
    sSQ <- attr(attr(Qspray, "showOpts"), "showSymbolicQspray")
  }
  attr(sSQ, "showRatioOfQsprays")
}
