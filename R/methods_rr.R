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
#'           s<-ridgereg(formula=Petal.Length~Species, data=iris,lambda=2)
#'           print(s)
#'
print.ridgereg <- function(x,...){

  stopifnot(class(x)=="ridgereg")

  cat("", sep="\n\n")

  cat("Call:")

  cat("", sep="\n\n")

  print(x$call)

  cat("", sep="\n\n")

  cat("Coefficients:")
  cat("", sep="\n\n")
  print(x$coefficients)
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
#'           res<-ridgereg(formula=Petal.Length~Species, data= iris, lambda=2)
#'           coef(res)
coef.ridgereg <- function(object,...){
  stopifnot(class(object)=="ridgereg")
  return(object$coefficients)
}

#' Predicted values
#'
#' Returns the predicted values
#'
#' @param object Refers to the object return by the linear regression function
#' @param x new data to be predicted
#' @param ... arguments to be passed to methods
#'
#' @return predicted values
#' @export
#'
#' @examples data(iris)
#'           res<-ridgereg(formula=Petal.Length~Species, data= iris, lambda=2)
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

#' Predicted values
#'
#' Returns the predicted values
#'
#' @param object Refers to the object return by the linear regression function
#' @param x new data to be predicted
#' @param ... arguments to be passed to methods
#'
#' @return predicted values
#' @export
#'
#' @examples data(iris)
#'           res<-ridgereg(formula=Petal.Length~Species, data= iris, lambda=2)
#'           pred(res)
predr <- function(object,x,...){

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

#' RMSE
#'
#' @param error residuals in model
#'
#' @return rmse value
#' @export
#'
#' @examples
#' data(iris)
#' ridgeres <- ridgereg(formula=Sepal.Length ~ Petal.Length + Petal.Width,
#' data=iris, lambda=2)
#' x = data.frame(Petal.Length=iris$Petal.Length,Petal.Width=iris$Petal.Width)
#' rr_error <- iris$Sepal.Length-predict(ridgeres,x)
#' rmse_ridge <- rmse(rr_error)
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
