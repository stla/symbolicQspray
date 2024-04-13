#' @include symbolicQspray.R
NULL

setGeneric("permuteVariables")

#' @name permuteVariables
#' @aliases permuteVariables,symbolicQspray,numeric-method
#' @docType methods
#' @title Permute variables
#' @description Permute the variables of a \code{symbolicQspray} polynomial.
#'
#' @param x a \code{symbolicQspray} object
#' @param permutation a permutation
#'
#' @return A \code{symbolicQspray} object.
#' @export
#' @importFrom qspray permuteVariables
#'
#' @examples
#' f <- function(a1, a2, X, Y, Z) {
#'   (a1^2 + 5*a2) / (a1 + 1) * X^2*Y  +  (3*a1 - a2) / a2 * Y^3
#' }
#' a1 <- qlone(1)
#' a2 <- qlone(2)
#' X <- Qlone(1)
#' Y <- Qlone(2)
#' Z <- Qlone(3)
#' Qspray <- f(a1, a2, X, Y, Z)
#' perm <- c(3, 1, 2)
#' permuteVariables2(Qspray, perm) == f(a1, a2, Z, X, Y) # should be TRUE
setMethod(
  "permuteVariables", c("symbolicQspray", "numeric"),
  function(x, permutation) {
    stopifnot(isPermutation(permutation))
    m <- arity(x)
    n <- length(permutation)
    if(m > n) {
      stop("Invalid permutation.")
    }
    permutation[permutation] <- seq_along(permutation)
    M <- powersMatrix(x)
    for(. in seq_len(n - m)) {
      M <- cbind(M, 0L)
    }
    M <- M[, permutation]
    powers <- apply(M, 1L, removeTrailingZeros, simplify = FALSE)
    out <- new("symbolicQspray", powers = powers, coeffs = x@coeffs)
    passShowAttributes(x, out)
  }
)

setGeneric("swapVariables")

#' @name swapVariables
#' @aliases swapVariables,symbolicQspray,numeric-method
#' @docType methods
#' @title Swap variables
#' @description Swap two variables of a \code{symbolicQspray}.
#'
#' @param x a \code{symbolicQspray} object
#' @param i,j indices of the variables to be swapped
#'
#' @return A \code{symbolicQspray} object.
#' @export
#' @importFrom qspray swapVariables
#'
#' @examples
#' library(symbolicQspray)
#' f <- function(a1, a2, X, Y, Z) {
#'   (a1^2 + 5*a2) / (a1 + 1) * X^2*Y  +  (3*a1 - a2) / a2 * Y^3
#' }
#' a1 <- qlone(1)
#' a2 <- qlone(2)
#' X <- Qlone(1)
#' Y <- Qlone(2)
#' Z <- Qlone(3)
#' Qspray <- f(a1, a2, X, Y, Z)
#' swapVariables2(Qspray, 2, 3) == f(a1, a2, X, Z, Y) # should be TRUE
setMethod(
  "swapVariables", c("symbolicQspray", "numeric"),
  function(x, i, j) {
    stopifnot(isNonnegativeInteger(i), isNonnegativeInteger(j))
    m <- arity(x)
    n <- max(m, i, j)
    permutation <- seq_len(n)
    permutation[i] <- j
    permutation[j] <- i
    M <- powersMatrix(x)
    for(. in seq_len(n - m)) {
      M <- cbind(M, 0L)
    }
    M <- M[, permutation]
    powers <- apply(M, 1L, removeTrailingZeros, simplify = FALSE)
    out <- new("symbolicQspray", powers = powers, coeffs = x@coeffs)
    passShowAttributes(x, out)
  }
)
