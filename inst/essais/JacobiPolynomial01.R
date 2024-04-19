library(symbolicQspray)

X <- Qlone(1)
alpha <- qlone(1)
beta <- qlone(2)

P1 <- (alpha + 1L) + (alpha + beta + 2L)*(X - 1L)/2L

showSymbolicQsprayOption(P1, "showRatioOfQsprays") <-
  showRatioOfQspraysXYZ(c("alpha", "beta"))
P1

P2 <- (alpha + 1L)*(alpha + 2L)/2L +
  (alpha + 2L)*(alpha + beta + 3L) * (X - 1L)/2L +
  (alpha + beta + 3L)*(alpha + beta + 4L)/2L * ((X - 1L)/2L)^2L

JacobiPolynomial <- function(n) {
  if(n == 0) {
    as.symbolicQspray(1L)
  } else if(n == 1) {
    X <- Qlone(1)
    alpha <- qlone(1)
    beta <- qlone(2)
    (alpha + 1L) + (alpha + beta + 2L)*(X - 1L)/2L
  } else {
    X <- Qlone(1)
    alpha <- qlone(1)
    beta <- qlone(2)
    a <- n + alpha
    b <- n + beta
    c <- a + b
    K <- 2*n * (c - n) * (c - 2)
    lambda1 <- ((c - 1) * (c * (c - 2) * X + (a - b) * (c - 2*n))) / K
    lambda2 <- (2 * (a - 1) * (b - 1) * c) / K
    (lambda1 * JacobiPolynomial(n - 1) - lambda2 * JacobiPolynomial(n - 2))
  }
}

P2 <- JacobiPolynomial(2)
showSymbolicQsprayOption(P2, "showRatioOfQsprays") <-
  showRatioOfQspraysXYZ(c("alpha", "beta"))
P2
