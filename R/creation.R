#' @title Polynomial variable
#' @description Create a polynomial variable for a \code{symbolicQspray}.
#'
#' @param n nonnegative integer, the index of the variable
#'
#' @return A \code{qspray} object.
#' @export
#' @examples
#' Qlone(2)
Qlone <- function(n) {
  stopifnot(isNonnegativeInteger(n))
  powers    <- integer(n)
  powers[n] <- 1L
  new(
    "symbolicQspray",
    powers = list(powers),
    coeffs = list(as.ratioOfQsprays(1L))
  )
}

#' @title Random 'symbolicQspray'
#' @description Generates a random \code{symbolicQspray} object.
#'
#' @return A \code{symbolicQspray} object.
#' @importFrom qspray rQspray
#' @importFrom ratioOfQsprays rRatioOfQsprays
rSymbolicQspray <- function() {
  qspray <- rQspray()
  powers <- qspray@powers
  nterms <- length(powers)
  coeffs <- replicate(nterms, rRatioOfQsprays())
  new(
    "symbolicQspray",
    powers = powers,
    coeffs = coeffs
  )
}

