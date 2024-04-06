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

as_symbolicQspray_scalar <- function(x) {
  x <- as.ratioOfQsprays(x)
  if(x == 0L) {
    new("symbolicQspray", powers = list(), coeffs = list())
  } else {
    new("symbolicQspray", powers = list(integer(0L)), coeffs = list(x))
  }
}

setGeneric(
  "as.symbolicQspray", function(x) {
    NULL
  }
)

#' @name as.symbolicQspray
#' @aliases as.symbolicQspray,character-method as.symbolicQspray,qspray-method as.symbolicQspray,ratioOfQsprays-method as.symbolicQspray,symbolicQspray-method as.symbolicQspray,numeric-method as.symbolicQspray,bigz-method as.symbolicQspray,bigq-method
#' @exportMethod as.symbolicQspray
#' @docType methods
#' @title Coercion to a 'symbolicQspray' object
#'
#' @param x a \code{symbolicQspray} object or an object for which
#'   \code{\link[ratioOfQsprays]{as.ratioOfQsprays}} is applicable
#'
#' @return A \code{symbolicQspray} object.
#' @export
#'
#' @examples
#' as.symbolicQspray(2)
#' as.symbolicQspray("1/3")
setMethod(
  "as.symbolicQspray", "character",
  function(x) {
    as_symbolicQspray_scalar(x)
  }
)

#' @rdname as.symbolicQspray
setMethod(
  "as.symbolicQspray", "qspray",
  function(x) {
    as_symbolicQspray_scalar(x)
  }
)

#' @rdname as.symbolicQspray
setMethod(
  "as.symbolicQspray", "ratioOfQsprays",
  function(x) {
    as_symbolicQspray_scalar(x)
  }
)

#' @rdname as.symbolicQspray
setMethod(
  "as.symbolicQspray", "symbolicQspray",
  function(x) {
    x
  }
)

#' @rdname as.symbolicQspray
setMethod(
  "as.symbolicQspray", "numeric",
  function(x) {
    as_symbolicQspray_scalar(x)
  }
)

#' @rdname as.symbolicQspray
setMethod(
  "as.symbolicQspray", "bigz",
  function(x) {
    as_symbolicQspray_scalar(x)
  }
)

#' @rdname as.symbolicQspray
setMethod(
  "as.symbolicQspray", "bigq",
  function(x) {
    as_symbolicQspray_scalar(x)
  }
)


#' @name symbolicQspray-unary
#' @title Unary operators for 'symbolicQspray objects
#' @description Unary operators for \code{symbolicQspray} objects.
#' @aliases +,symbolicQspray,missing-method -,symbolicQspray,missing-method
#' @param e1 object of class \code{symbolicQspray}
#' @param e2 nothing
#' @return A \code{symbolicQspray} object.
setMethod(
  "+",
  signature(e1 = "symbolicQspray", e2 = "missing"),
  function(e1, e2) e1
)
#' @rdname symbolicQspray-unary
setMethod(
  "-",
  signature(e1 = "symbolicQspray", e2 = "missing"),
  function(e1, e2) {
    new(
      "symbolicQspray",
      powers = e1@powers, coeffs = lapply(e1@coeffs, function(x) -x)
    )
  }
)


symbolicQsprayPower <- function(qspray, n) {
  stopifnot(isPositiveInteger(n))
  SymbolicQspray_power(
    e1@powers, lapply(e1@coeffs, ratioOfQsprays_as_list, n)
  )
}
symbolicQspray_arith_scalar <- function(e1, e2) {
  switch(
    .Generic,
    "+" = e1 + as.symbolicQspray(e2),
    "-" = e1 - as.symbolicQspray(e2),
    "*" = e1 * as.symbolicQspray(e2),
    "/" = e1 * as.symbolicQspray(1L/as.ratioOfQsprays(e2)),
    "^" = symbolicQsprayPower(e2),
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
}
scalar_arith_symbolicQspray <- function(e1, e2) {
  switch(
    .Generic,
    "+" = as.symbolicQspray(e1) + e2,
    "-" = -as.symbolicQspray(e1) + e2,
    "*" = as.symbolicQspray(e1) * e2,
    stop(gettextf(
      "Binary operator %s not defined for these two objects.", dQuote(.Generic)
    ))
  )
}
qspray_from_list <- function(x) {
  powers <- x[["powers"]]
  if(is.null(powers)) {
    new("qspray", powers = list(), coeffs = character(0L))
  }
  else {
    new("qspray", powers = powers, coeffs = x[["coeffs"]])
  }
}
qspray_as_list <- function(x) {
  list("powers" = x@powers, "coeffs" = x@coeffs)
}
ratioOfQsprays_from_list <- function(x) {
  new(
    "ratioOfQsprays",
    numerator   = qspray_from_list(x[["numerator"]]),
    denominator = qspray_from_list(x[["denominator"]])
  )
}
ratioOfQsprays_as_list <- function(x) {
  list(
    "numerator"   = qspray_as_list(x@numerator),
    "denominator" = qspray_as_list(x@denominator)
  )
}
symbolicQspray_from_list <- function(x) {
  powers <- x[["powers"]]
  if(is.null(powers)) {
    new("symbolicQspray", powers = list(), coeffs = list())
  }
  else {
    new(
      "symbolicQspray",
      powers = powers,
      coeffs = lapply(x[["coeffs"]], ratioOfQsprays_from_list)
    )
  }
}
symbolicQspray_arith_symbolicQspray <- function(e1, e2) {
  switch(
    .Generic,
    "+" = symbolicQspray_from_list(
      SymbolicQspray_add(
        e1@powers, lapply(e1@coeffs, ratioOfQsprays_as_list),
        e2@powers, lapply(e2@coeffs, ratioOfQsprays_as_list)
      )
    ),
    "-" = symbolicQspray_from_list(
      SymbolicQspray_subtract(
        e1@powers, lapply(e1@coeffs, ratioOfQsprays_as_list),
        e2@powers, lapply(e2@coeffs, ratioOfQsprays_as_list)
      )
    ),
    "*" = symbolicQspray_from_list(
      SymbolicQspray_mult(
        e1@powers, lapply(e1@coeffs, ratioOfQsprays_as_list),
        e2@powers, lapply(e2@coeffs, ratioOfQsprays_as_list)
      )
    ),
    stop(gettextf(
      "Binary operator %s not defined for symbolicQspray objects.", dQuote(.Generic)
    ))
  )
}
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "symbolicQspray"),
  symbolicQspray_arith_symbolicQspray
)
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "ratioOfQsprays"),
  symbolicQspray_arith_scalar
)
setMethod(
  "Arith",
  signature(e1 = "ratioOfQsprays", e2 = "symbolicQspray"),
  scalar_arith_symbolicQspray
)
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "qspray"),
  symbolicQspray_arith_scalar
)
setMethod(
  "Arith",
  signature(e1 = "qspray", e2 = "symbolicQspray"),
  scalar_arith_symbolicQspray
)
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "character"),
  symbolicQspray_arith_scalar
)
setMethod(
  "Arith",
  signature(e1 = "character", e2 = "symbolicQspray"),
  scalar_arith_symbolicQspray
)
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "bigz"),
  symbolicQspray_arith_scalar
)
setMethod(
  "Arith",
  signature(e1 = "bigz", e2 = "symbolicQspray"),
  scalar_arith_symbolicQspray
)
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "bigq"),
  symbolicQspray_arith_scalar
)
setMethod(
  "Arith",
  signature(e1 = "bigq", e2 = "symbolicQspray"),
  scalar_arith_symbolicQspray
)
setMethod(
  "Arith",
  signature(e1 = "symbolicQspray", e2 = "numeric"),
  symbolicQspray_arith_scalar
)
setMethod(
  "Arith",
  signature(e1 = "numeric", e2 = "symbolicQspray"),
  scalar_arith_symbolicQspray
)
