
test_that( "Correct function of implicit product matrices", {
  P1 = 70
  P2 = 100
  N = 30

  X = matrix(runif(N * P1), N)
  Y = matrix(runif(N * P2), N)

  X_explicit = scale(X)
  Y_explicit = scale(Y)

  # regular matrices
  xy_test = Matrix_Product(X, Y)
  right_test = runif(P2)
  err = max(abs(xy_test %*% right_test -  t(X) %*% Y %*% right_test))
  expect_true(err < 1e-10)
  left_test = runif(P1)
  err = max(abs(left_test %*% xy_test - left_test %*% t(X) %*% Y))
  expect_true(err < 1e-10)

  # implicitly scaled matrices
  X_implicit = Center_Scaled_Matrix(X)
  Y_implicit = Center_Scaled_Matrix(Y)

  mid_test = runif(N)
  err = max(abs(mid_test %*% X_implicit - mid_test %*% scale(X)))
  expect_true(err < 1e-10)
  err = max(abs(X_implicit %*% left_test - scale(X) %*% left_test ))
  expect_true(err < 1e-10)
})
