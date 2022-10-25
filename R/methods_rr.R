#' Print
#'
#' Prints model call and coefficients
#'
#' @param x Refers to the object return by the linear regression function
#' @param ... arguments to be passed to methods
#'
#' @return Call, Coefficients
#' @export
#'
#' @examples data(iris)
#'           s<-linreg(formula=Petal.Length~Species, data=iris)
#'           print(s)
#'
print.ridgereg <- function(x,...){

  stopifnot(class(x="ridgereg"))

  cat("", sep="\n\n")

  cat("Call:")

  cat("", sep="\n\n")

  print(x$call)

  cat("", sep="\n\n")

  cat("Coefficients:")
  cat("", sep="\n\n")
  print(x$coefficients)
}
#' Residual of the linear regression function
#'
#' Returns the residuals
#'
#' @param object Refers to the res variable return by the linear regression function
#' @param ... arguments to be passed to methods
#'
#' @return object$residuals
#' @export
#'
#' @examples data(iris)
#'           s<-linreg(formula=Petal.Length~Species, data= iris)
#'           resid(s)


#' Predicted values
#'
#' Returns the predicted values
#'
#' @param object Refers to the object return by the linear regression function
#'
#' @return predicted values
#' @export
#'
#' @examples data(iris)
#'           res<-ridgereg(formula=Petal.Length~Species, data= iris)
#'           pred(res)
predict.ridgereg <- function(object,x,...){

  stopifnot(class(object)=="ridgereg")

  beta_hat_1_p <- as.matrix(object$coefficients[-1])
  beta_hat_0 <- as.matrix(object$coefficients[1])

  if (nargs()==1) {
    y_pred <- object$coefficients
  }
  else if (nargs()>1) {
    stopifnot(is.data.frame(x),(names(x) %in% names(object$coefficients)[-1]))
    x <- as.matrix(x)
    y_pred <- as.vector(sweep(x%*%beta_hat_1_p,2,beta_hat_0,'+'))
  }
  return(y_pred)
}

#' Coefficients
#'
#' Returns the coefficients
#'
#' @param object Refers to the object return by the linear regression function
#' @param ... arguments to be passed to methods
#'
#' @return object$coefficients
#' @export
#'
#' @examples data(iris)
#'           res<-ridgereg(formula=Petal.Length~Species, data= iris)
#'           coef(res)
coef.ridgereg <- function(object,...){
  stopifnot(class(object="ridgereg"))
  return(object$coefficients)
}

rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
