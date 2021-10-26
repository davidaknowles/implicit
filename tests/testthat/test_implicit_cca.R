
test_that( "Correct function of implicit product matrices in CCA", {
  P1 = 70
  P2 = 100
  N = 50

  X = matrix(runif(N * P1), N)
  Y = matrix(runif(N * P2), N)

  XY <- t(scale(X)) %*% scale(Y)
  i = irlba(XY)

  XY_implicit = Matrix_Product(Center_Scaled_Matrix(X),
                               Center_Scaled_Matrix(Y))

  i_fast = irlba(XY_implicit)
  err = max(abs(i$d - i_fast$d))
  expect_true(err < 1e-10)
})
