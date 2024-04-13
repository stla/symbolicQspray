test_that("show - default", {
  set.seed(421)
  Q <- rSymbolicQspray()
  expect_identical(
    Print(Q),
    "{ [ 2*a1^2.a3^3 + 5*a1^2 + 5*a3 ] %//% [ -2*a1^4.a3^3 + 3*a2^4 ] } * X^3.Y  +  { [ 5*a1^4.a3^4 - a1^2.a2.a3^3 ] %//% [ 3*a1^2 - 3*a2^2.a3^3 ] } * Y^3 "
  )
  expect_identical(
    Print(Q + Qlone(4)),
    "{ [ 2*a1^2.a3^3 + 5*a1^2 + 5*a3 ] %//% [ -2*a1^4.a3^3 + 3*a2^4 ] } * X1^3.X2  +  { [ 5*a1^4.a3^4 - a1^2.a2.a3^3 ] %//% [ 3*a1^2 - 3*a2^2.a3^3 ] } * X2^3  +  { [ 1 ] } * X4 "
  )
})

test_that("show options", {
  set.seed(421)
  Q <- rSymbolicQspray()
  showSymbolicQsprayOption(Q, "a") <- "x"
  expect_identical(
    Print(Q),
    "{ [ 2*x1^2.x3^3 + 5*x1^2 + 5*x3 ] %//% [ -2*x1^4.x3^3 + 3*x2^4 ] } * X^3.Y  +  { [ 5*x1^4.x3^4 - x1^2.x2.x3^3 ] %//% [ 3*x1^2 - 3*x2^2.x3^3 ] } * Y^3 "
  )
  showSymbolicQsprayOption(Q, "X") <- "A"
  expect_identical(
    Print(Q),
    "{ [ 2*x1^2.x3^3 + 5*x1^2 + 5*x3 ] %//% [ -2*x1^4.x3^3 + 3*x2^4 ] } * A1^3.A2  +  { [ 5*x1^4.x3^4 - x1^2.x2.x3^3 ] %//% [ 3*x1^2 - 3*x2^2.x3^3 ] } * A2^3 "
  )
  showSymbolicQsprayOption(Q, "showMonomial") <-
    showMonomialXYZ(c("U","V","W"), collapse = "%")
  expect_identical(
    Print(Q),
    "{ [ 2*x1^2.x3^3 + 5*x1^2 + 5*x3 ] %//% [ -2*x1^4.x3^3 + 3*x2^4 ] } * U^3%V  +  { [ 5*x1^4.x3^4 - x1^2.x2.x3^3 ] %//% [ 3*x1^2 - 3*x2^2.x3^3 ] } * V^3 "
  )
  expect_identical(
    Print(Q * Qlone(4)),
    "{ [ 2*x1^2.x3^3 + 5*x1^2 + 5*x3 ] %//% [ -2*x1^4.x3^3 + 3*x2^4 ] } * U1^3%U2%U4  +  { [ 5*x1^4.x3^4 - x1^2.x2.x3^3 ] %//% [ 3*x1^2 - 3*x2^2.x3^3 ] } * U2^3%U4 "
  )
})

test_that("show - univariate", {
  Q <- (qlone(1) / (1+qlone(1))) * Qlone(1)
  expect_identical(
    Print(Q),
    "{ [ a ] %//% [ a + 1 ] } * X "
  )
  showSymbolicQsprayOption(Q, "a") <- "w"
  expect_identical(
    Print(Q),
    "{ [ w ] %//% [ w + 1 ] } * X "
  )
  showSymbolicQsprayOption(Q, "X") <- "A"
  expect_identical(
    Print(Q),
    "{ [ w ] %//% [ w + 1 ] } * A "
  )
  Q <- (qlone(1) / (qlone(1)+qlone(2))) * Qlone(1)
  expect_identical(
    Print(Q),
    "{ [ a1 ] %//% [ a1 + a2 ] } * X "
  )
  R <- ((qlone(1)+qlone(2))) * Qlone(1)
  expect_identical(
    Print(Q*R),
    "{ [ a ] } * X^2 "
  )
  showSymbolicQsprayOption(Q, "showRatioOfQsprays") <-
    showRatioOfQspraysX1X2X3("a")
  expect_identical(
    Print(Q*R),
    "{ [ a1 ] } * X^2 "
  )
})
