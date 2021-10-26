
test_that( "Correct function of implicitly scaled matrices", {
  N = 50
  P = 30
  X = matrix(runif(N * P), N)

  # check scaling
  X_scaled = Scaled_Matrix(X)
  left_test = runif(N)
  err = max(abs(left_test %*% X_scaled - left_test %*% scale(X, center = F))) # small, yay!
  expect_true(err < 1e-10)
  right_test = runif(P)
  err = max(abs(X_scaled %*% right_test - scale(X, center = F) %*% right_test )) # small, yay!
  expect_true(err < 1e-10)

  # centering
  X_centered = Centered_Matrix(X)
  err = max(abs(left_test %*% X_centered - left_test %*% scale(X, scale = F))) # small, yay!
  expect_true(err < 1e-10)
  err = max(abs(X_centered %*% right_test - scale(X, scale = F) %*% right_test )) # small, yay!
  expect_true(err < 1e-10)

  # both
  X_centered_scaled = Center_Scaled_Matrix(X)
  err = max(abs(left_test %*% X_centered_scaled - left_test %*% scale(X))) # small, yay!
  expect_true(err < 1e-10)
  err = max(abs(X_centered_scaled %*% right_test - scale(X) %*% right_test )) # small, yay!
  expect_true(err < 1e-10)
})
