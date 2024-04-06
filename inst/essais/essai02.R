library(symbolicQspray)

f <- function(a1, a2, X1, X2, X3) {
  (a1/(a2^2+1)) * X1^2*X2  +  (a2+1) * X3
}

a1 <- qlone(1)
a2 <- qlone(2)
X1 <- Qlone(1)
X2 <- Qlone(2)
X3 <- Qlone(3)

( Qspray <- f(a1, a2, X1, X2, X3) )
Qspray^2
Qspray - Qspray

( qspray <- evalSymbolicQspray(Qspray, a = c(2, 3)) )
( ratioOfQsprays <- evalSymbolicQspray(Qspray, X = c(4, 3, 2)) )
evalSymbolicQspray(Qspray, a = c(2, 3), X = c(4, 3, 2))
evalQspray(qspray, c(4, 3, 2))
evalRatioOfQsprays(ratioOfQsprays, c(2, 3))
