library(symbolicQspray)

X     <- Qlone(1)
alpha <- qlone(1)
beta  <- qlone(2)

n <- 5
JP <- JacobiPolynomial(n)
JPprime      <- derivSymbolicQspray(JP, 1)
JPprimeprime <- derivSymbolicQspray(JP, 1, 2)

shouldBeZero <- (1L - X^2)*JPprimeprime +
  (beta - alpha - (alpha + beta + 2L)*X)*JPprime -
  n*(n + alpha + beta + 1L)*JP

shouldBeZero
