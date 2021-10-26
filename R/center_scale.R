
setClass("scaled_matrix",
         slots=c(data = "ANY", scale="numeric"))
setMethod("%*%", signature(x="scaled_matrix", y="numeric"),
          function(x, y) (x@data %*% (y / x@scale)))
setMethod("%*%", signature(x="numeric", y="scaled_matrix"),
          function(x, y) ((x %*% y@data) / y@scale))
setMethod("dim", signature(x="scaled_matrix"),
          function(x) dim(x@data))

#' @import methods
#' @export
Scaled_Matrix = function(A,
                         scale = sqrt(apply(A*A, 2, sum)/(nrow(A)-1))) {
  new("scaled_matrix", data = A, scale = scale)
}

setClass("centered_matrix",
         slots=c(data = "ANY", center="numeric"))
setMethod("%*%", signature(x="centered_matrix", y="numeric"),
          function(x, y) ( x@data %*% y - as.numeric(x@center %*% y)))
setMethod("%*%", signature(x="numeric", y="centered_matrix"),
          function(x, y) (x %*% y@data - sum(x) * y@center ))
setMethod("dim", signature(x="centered_matrix"),
          function(x) dim(x@data))

#' @export
Centered_Matrix = function(A,
                           center = apply(A, 2, mean)) {
  new("centered_matrix", data = A, center = center)
}

#' @importFrom stats sd
#' @export
Center_Scaled_Matrix = function(A,
                                center = apply(A, 2, mean),
                                scale = apply(A, 2, sd)) {
  Scaled_Matrix(Centered_Matrix(A, center = center), scale = scale)
}
