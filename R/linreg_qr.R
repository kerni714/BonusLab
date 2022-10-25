#' Linreg_qr
#' 
#' Linreg_qr is used to carry out multiple linear regression, with qr factorization
#' to estimate the coeffcients and the variance of the coefficients.
#'
#' @param formula an object of class "formula"
#' @param data a dataframe containing the variables in the formula
#'
#' @return returns object of class "linreg"
#' @export
#'
#' @examples
#' data(iris)
#' linres <- linreg(formula=Sepal.Length ~ Petal.Length + Petal.Width, data=iris)

linreg_qr <- function(formula, data) {
  #- Checks part I
  stopifnot(class(formula)=="formula",is.data.frame(data))
  
  #- Retrieve name of dataset
  data1 <- format(deparse(substitute(data)))
  
  #- Retrieve y
  y <- data[,all.vars(expr=formula)[1]] 
  
  #- Checks part II: y numeric
  stopifnot(is.numeric(y))
  
  X <-stats::model.matrix(object=formula, data=data)
  
  #- Estimate beta_hat (w. qr factorization)
  QR <- qr_hr(X)
  beta_hat_1 <- solve(QR$R,t(QR$Q)%*%y)
  beta_hat <- as.vector(beta_hat_1)
  names(beta_hat) <- dimnames(beta_hat_1)[[1]]
  
  y_hat <- X%*%beta_hat
  y_hat <- as.vector(y_hat)
  
  #- Residuals
  e_hat <- y-y_hat
  
  #- Degrees of freedom
  n <- length(y)
  p <- length(beta_hat)
  df <- n-p
  
  #- Residual variance
  var_hat <- ((t(e_hat)%*%e_hat)/df)[1,1]
  
  #- Estimate variance beta_hat (w. qr factorization)
  XtX <- t(X)%*%X
  I <- diag(dim(XtX)[1])
  QR <- qr_hr(XtX)
  b <- solve(QR$R,t(QR$Q)%*%I)
  
  beta_hat_var <- diag(var_hat[[1]]*solve(QR$R,t(QR$Q)%*%I))
  names(beta_hat_var) <- colnames(X)
  
  #- T-values for the coefficients
  t_beta <- beta_hat/sqrt(beta_hat_var)
  
  #- Calculate p-values
  p_values <- 2*stats::pt(abs(t_beta),df, lower.tail = FALSE)
  
  # Collect results
  linres <- list(#call = call,
                 formula = formula,
                 data1 = data1,
                 coefficients = beta_hat,
                 fitted_values = y_hat,
                 residuals = e_hat,
                 df = df,
                 residual_variance = var_hat,
                 coefficients_variance = beta_hat_var,
                 coefficients_tvalues = t_beta,
                 coefficients_pvalues = p_values
  )
  class(linres) <- "linreg"
  return(linres)
}
