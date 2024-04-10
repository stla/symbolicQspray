passShowAttributes <- function(source, target) {
  showOpts <- attr(source, "showOpts")
  if( # if source has been created with as.symbolicQspray
    inherits(source, "ratioOfQsprays")
  ) {
    sROQ <- attr(showOpts, "showRatioOfQsprays")
    if(!is.null(sROQ)) {
      showSymbolicQsprayOption(target, "showRatioOfQsprays") <- sROQ
    }
    return(target)
  }
  inheritable <- isTRUE(attr(showOpts, "inheritable"))
  if(!inheritable) {
    test <- numberOfVariables(source) >= numberOfVariables(target)
    if(test) {
      n1 <- max(vapply(source@coeffs, numberOfVariables, integer(1L)))
      n2 <- max(vapply(target@coeffs, numberOfVariables, integer(1L)))
      test <- n1 >= n2
    }
  }
  if(inheritable || test) {
    attr(target, "showOpts") <- showOpts
  }
  target
}

arity <- function(qspray) {
  suppressWarnings(max(lengths(qspray@powers)))
}

#' @title (internal) Make a 'symbolicQspray' object from a list
#' @description This function is for internal usage. It is exported because
#'   it is also used for internal usage in others packages.
#'
#' @param qspray_as_list list returned by the Rcpp function
#'   \code{returnSymbolicQspray}
#'
#' @return A \code{symbolicQspray} object.
#' @export
#' @importFrom ratioOfQsprays ratioOfQsprays_from_list
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

qspray_as_list <- function(x) {
  list("powers" = x@powers, "coeffs" = x@coeffs)
}

ratioOfQsprays_as_list <- function(x) {
  list(
    "numerator"   = qspray_as_list(x@numerator),
    "denominator" = qspray_as_list(x@denominator)
  )
}

`%||%` <- function(x, y) {
  if(is.null(x)) y else x
}

isPositiveInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x
}

isNonnegativeInteger <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x && x != 0
}
