library(symbolicQspray)

f <- function(a, X1, X2) {
  (a/(a^2+1)) * X1*X2^2  +  (a+1) * X1
}

a <- qlone(1)
X1 <- Qlone(1)
X2 <- Qlone(2)

Qspray <- f(a, X1, X2)

Qspray

Qspray^2

Qspray - Qspray
