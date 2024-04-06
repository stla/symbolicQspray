#' @useDynLib symbolicQspray, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom qspray orderedQspray
#' @importFrom ratioOfQsprays as.ratioOfQsprays
#' @importFrom methods setMethod setClass new show
#' @importFrom gmp as.bigq
#' @importFrom utils capture.output
#' @include symbolicQspray.R
NULL

setClass(
  "symbolicQspray",
  slots = c(powers = "list", coeffs = "list")
)

showSymbolicQspray <- function(qspray) {
  if(length(qspray@coeffs) == 0L) {
    return("0")
  }
  qspray <- orderedQspray(qspray)
  monomials <- vapply(qspray@powers, function(x) {
    paste0("X", x)
  }, FUN.VALUE = character(1L))
  coeffs <- vapply(qspray@coeffs, capture.output, character(1L))
  paste0(paste0(coeffs, " ", monomials), collapse = " + ")
}

setMethod(
  "show", "symbolicQspray",
  function(object) {
    cat(showSymbolicQspray(object), "\n")
  }
)
