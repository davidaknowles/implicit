
#' @import irlba
#' @export
implicit_cca = function(X, Y, ...) {
  XY_implicit = Matrix_Product(Center_Scaled_Matrix(X),
                               Center_Scaled_Matrix(Y))
  irlba(XY_implicit, ...)
}

