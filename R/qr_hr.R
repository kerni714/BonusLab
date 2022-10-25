#' QR factorization with Householder reflection
#' 
#' This function performs QR factorisation with Householder reflection.
#' See e.g. Some	Notes	on	Least	Squares, QR-factorization, SVD and
#' Fitting. Ove	Edlund, Department of Science and Mathematics
#' January 23, 2013. C0002M Numerical Analysis.
#'
#' @param A n x p (p <= n) matrix for which to do the QR factorization 
#'
#' @return list with Q and R matrices
#' @export
#'
#' @examples
#' data(iris)
#' formula <- Sepal.Length ~ Petal.Length + Petal.Width
#' data <- iris
#' X <-stats::model.matrix(object=formula, data=data)
#' QR <- qr_hr(X)

qr_hr <- function (A) {
  
  #- Check of input
  stopifnot(is.matrix(A),is.numeric(A),dim(A)[2]<=dim(A)[1])
  
  #- Find number of rows and columns
  n <- dim(A)[1]
  p <- dim(A)[2]
  
  #- Initialize Q matrix
  Q <- diag(n)
  
  #- Apply Householder reflection algorithm
  i <- 1
  
  while (i <= p ) {
    v <- as.matrix(cbind(rep(0,n)))
    v[i:n] <- A[i:n,i]
    v[i] <- v[i] + sign(v[i])*norm(v,"2")
    H <- diag(n) - 2*(v%*%t(v))/(t(v)%*%v)[1,1]
    A <- H%*%A
    Q <- Q%*%H
    i <- i +1
  }
  Q <- Q[,1:p]
  A_sub <- A[1:p,]
  A_sub[lower.tri(A_sub, diag=FALSE)] <- 0
  R <- A_sub
  
  ans <- list(Q=Q, R=R)
  return(ans)
}
