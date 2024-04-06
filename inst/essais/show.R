showQspray <- function(showMonomial, compact = FALSE) {
  function(qspray) {
    if(length(qspray@coeffs) == 0L) {
      return("0")
    }
    qspray <- orderedQspray(qspray)
    nterms <- length(qspray@coeffs)
    constantTerm <- getConstantTerm(qspray)
    monomials <- vapply(qspray@powers, showMonomial, FUN.VALUE = character(1L))
    coeffs <- gmp::as.bigq(qspray@coeffs)
    plus <- vapply(coeffs, function(x) x >= 0L, FUN.VALUE = logical(1L))
    plusSign <- ifelse(compact, "+", " + ")
    minusSign <- ifelse(compact, "-", " - ")
    signs <- c(ifelse(plus[-1L], plusSign, minusSign), "")
    abscoeffs <- as.character(abs(coeffs))
    terms <- paste0(
      ifelse(abscoeffs == "1", "", paste0(abscoeffs, "*")), monomials
    )
    if(constantTerm != 0L) {
      terms[nterms] <- as.character(abs(constantTerm))
    }
    leader <- if(plus[1L]) "" else "-"
    paste0(c(leader, c(rbind(terms, signs))), collapse = "")
  }
}

showMonomialCanonical <- function(var) {
  function(exponents) {
    paste0(vapply(seq_along(exponents), function(i) {
      e <- exponents[i]
      if(e != 0L) {
        if(e == 1L) {
          sprintf("%s%d", var, i)
        } else {
          sprintf("%s%d^%d", var, i, e)
        }
      } else {
        ""
      }
    }, character(1L)), collapse = "")
  }
}

showQsprayCanonical <- function(var, ...) {
  showQspray(showMonomialCanonical(var), ...)
}

showMonomial <- showMonomialCanonical("a")

qspray <- rQspray()

showQspray(showMonomial)(qspray)
showQspray(showMonomial)(qspray+1)
showQspray(showMonomial)(qspray+2)
showQspray(showMonomial)(qspray-3)

showRatioOfQsprays <- function(showQspray, quotientBar = "  %//%  ") {
  function(roq) {
    if(isQone(roq@denominator)) {
      sprintf(
        "[%s]", showQspray(roq@numerator)
      )
    } else {
      sprintf(
        "[ %s ]%s[ %s ]",
        showQspray(roq@numerator),
        quotientBar,
        showQspray(roq@denominator)
      )
    }
  }
}

showRatioOfQspraysCanonical <- function(var, quotientBar = "  %//%  ", ...) {
  showRatioOfQsprays(showQsprayCanonical(var), quotientBar = quotientBar, ...)
}


roq <- rRatioOfQsprays()
showRatioOfQsprays(showQspray(showMonomial))(roq)

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
      }, character(1L)), collapse = "")
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
    paste0(toPaste, collapse = " + ")
  }
}

#Qspray <- rSymbolicQspray()
showSymbolicQspray(showRatioOfQsprays(showQspray(showMonomial)))(Qspray)

showSymbolicQsprayCanonical <- function(
  a = "a", X = "X", quotientBar = "  %//%  ", ...
) {
  showSymbolicQspray(
    showRatioOfQspraysCanonical(var = a, quotientBar = quotientBar),
    var = X, ...
  )
}

showSymbolicQsprayCanonical("a", "X")(Qspray)
