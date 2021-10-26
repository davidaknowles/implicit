# implicit

This is an R package to allow performing PCA and CCA with implicitly scaled matrices. 
For example, `X` (dimension N x P1) and `Y` (dimension N x P2) can be sparse matrices and
we would like to perform PCA on `scale(X)` or (diagonal) CCA on `scale(X)` and `scale(Y)`. 
The challenge is that `scale(X)` is no longer sparse. 
`implicit` implements `scaled_matrix` and `centered_matrix` which implicitly scale and/or center
an underlying matrix and allows left and right multiplication with a vector 
(the key operation for fast SVD using `irlba`). This avoids ever creating large dense matrices
but gives the same results up to (approximately) machine precision. 

