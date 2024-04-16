test_that("evaluation", {
  f <- function(a1, a2, X1, X2, X3) {
    ((a1/(a2^2+1)) * X1^2*X2)^2  +  (a2+1) * X3  +  5L
  }
  a1 <- qlone(1)
  a2 <- qlone(2)
  X1 <- Qlone(1)
  X2 <- Qlone(2)
  X3 <- Qlone(3)
  Qspray <- f(a1, a2, X1, X2, X3)
  library(gmp)
  a <- as.bigq(c(2L, 3L))
  X <- as.bigq(c(-4L, 3L, 2L))
  qspray <- evalSymbolicQspray(Qspray, a = a)
  expect_true(qspray == f(a[1], a[2], qlone(1), qlone(2), qlone(3)))
  ratioOfQsprays <- evalSymbolicQspray(Qspray, X = X)
  expect_true(ratioOfQsprays == f(a1, a2, X[1], X[2], X[3]))
  result <- evalSymbolicQspray(Qspray, a = a, X = X)
  expect_true(result == evalQspray(qspray, values_re = X))
  expect_true(result == evalRatioOfQsprays(ratioOfQsprays, a))
})
