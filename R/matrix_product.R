

setClass("matrix_product",
         slots=c(a="ANY", # in practice must be a matrix type
                 b="ANY"))
setMethod("%*%",
          signature(x="matrix_product", y="numeric"),
          function(x, y) {
            t(as.numeric(x@b %*% y) %*% x@a)
          })
setMethod("%*%",
          signature(x="numeric", y="matrix_product"),
          function(x, y) as.numeric(y@a %*% x) %*% y@b)
setMethod("dim", signature(x="matrix_product"),
          function(x) c(ncol(x@a),ncol(x@b)))

#' @export
Matrix_Product = function(A, B) {
  new("matrix_product", a=A, b=B)
}
