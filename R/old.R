setClass("centered_scaled_matrix", contains="matrix", slots=c(scale="numeric"))
setMethod("%*%", signature(x="centered_scaled_matrix", y="numeric"),
          function(x, y) {
            y_scaled = y / x@scale
            x@.Data %*% y_scaled - as.numeric(x@center %*% y_scaled)
          })
setMethod("%*%", signature(x="numeric", y="centered_scaled_matrix"),
          function(x, y) {
            (x %*% y@.Data - sum(x) * y@center ) / y@scale
          })

get_center_scaled = function(A) {
  new("centered_scaled_matrix",
      A, 
      center = apply(A, 2, mean),
      scale = apply(A, 2, sd))
}


setClass("matrix_product_scaled", 
         slots=c(a="ANY", b="ANY"))
setMethod("%*%", 
          signature(x="matrix_product_scaled", y="numeric"),
          function(x, y) {
            t(as.numeric(x@b %*% y) %*% x@a) # as.numeric instead of t to get correct signature
          })
setMethod("%*%", 
          signature(x="numeric", y="matrix_product_scaled"),
          function(x, y) { 
            as.numeric(y@a %*% x) %*% y@b # as.numeric instead of t to get correct signature
          } )
setMethod("dim", signature(x="matrix_product_scaled"),
          function(x) c(ncol(x@a),ncol(x@b)))
