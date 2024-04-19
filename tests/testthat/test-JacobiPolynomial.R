test_that("Jacobi polynomial", {
  obtained <- JacobiPolynomial(2)
  X     <- Qlone(1)
  alpha <- qlone(1)
  beta  <- qlone(2)
  expected <- (alpha + 1L)*(alpha + 2L)/2L +
    (alpha + 2L)*(alpha + beta + 3L) * (X - 1L)/2L +
    (alpha + beta + 3L)*(alpha + beta + 4L)/2L * ((X - 1L)/2L)^2L
  expect_true(obtained == expected)
})
