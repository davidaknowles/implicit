require(irlba)

P1 = 700
P2 = 1000
N = 500

X = matrix(runif(N * P1), N)
Y = matrix(runif(N * P2), N)

X_explicit = scale(X)
Y_explicit = scale(Y)
XY <- t(X_explicit) %*% Y_explicit
i = irlba(XY) 

xy_test = Matrix_Product(X, Y)
right_test = runif(P2)
max(abs(xy_test %*% right_test -  t(X) %*% Y %*% right_test))
left_test = runif(P1)
max(abs(left_test %*% xy_test - left_test %*% t(X) %*% Y))

X_implicit = Center_Scaled_Matrix(X)
Y_implicit = Center_Scaled_Matrix(Y)

mid_test = runif(N)
max(abs(mid_test %*% X_implicit - mid_test %*% scale(X)))
max(abs(X_implicit %*% left_test - scale(X) %*% left_test ))

XY_implicit = Matrix_Product(X_implicit, Y_implicit) # new("matrix_product_scaled", a=X_implicit, b=Y_implicit)

max(abs(XY_implicit %*% right_test - XY %*% right_test))
max(abs(left_test %*% XY_implicit - left_test %*% XY))

i_fast = irlba(XY_implicit)  

max(abs(i$d - i_fast$d))



N = 5000
P1 = 1000

# c(100, 200, 500, 1000, 2000, 5000, 10000)
res_ = foreach(P2 = c(20000, 50000, 1e5), .combine = bind_rows) %do% {
  X = matrix(runif(N * P1), N)
  Y = matrix(runif(N * P2), N)
  
  naive_time = system.time( {
    X_explicit = scale(X)
    Y_explicit = scale(Y)
    XY <- t(X_explicit) %*% Y_explicit
    i = irlba(XY)
  } )
  
  smart_time = system.time( {
    X_implicit = get_center_scaled(X)
    Y_implicit = get_center_scaled(Y)
    XY_implicit = new("matrix_product_scaled", a=X_implicit, b=Y_implicit)
    i_fast = irlba(XY_implicit)  
  })
  
  r = tibble(P2 = P2, naive = naive_time[1], smart = smart_time[1])
  print(r)
  r
}

res %>% gather(meth, time, -P2) %>% 
  ggplot(aes(P2, time, col = meth)) + geom_point() + geom_line()


require(profmem)
naive_prof = profmem( {
  X_explicit = scale(X)
  Y_explicit = scale(Y)
  XY = t(X_explicit) %*% Y_explicit
  i = irlba(XY)
} )
sum(naive_prof$bytes) / 1000 / 1000

smart_prof = profmem( {
  X_implicit = get_center_scaled(X)
  Y_implicit = get_center_scaled(Y)
  XY_implicit = new("matrix_product_scaled", a=X_implicit, b=Y_implicit)
  i_fast = irlba(XY_implicit)  
})
sum(smart_prof$bytes) / 1000 / 1000

X_implicit = get_center_scaled(X)

