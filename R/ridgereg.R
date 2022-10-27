#' ridgereg
#'
#' ridgereg is used to carry out ridge regression.
#'
#' @param formula an object of class "formula"
#' @param data a dataframe containing the variables in the formula
#' @param lambda regularisation parameter
#'
#' @return returns object of class "ridgereg"
#' @export
#'
#' @examples
#' data(iris)
#' ridgeres <- ridgereg(formula=Sepal.Length ~ Petal.Length + Petal.Width,
#' data=iris, lambda=2)

ridgereg <- function(formula, data, lambda) {
  #- Checks part I
  stopifnot(class(formula)=="formula",is.data.frame(data))

  if (sum(is.na(data)) > 0) {
    stop("There are missing values (NA) in the data")
  }

  #- Retrieve y
  y <- data[,all.vars(expr=formula)[1]]

  #- Checks part II: y numeric
  stopifnot(is.numeric(y))

  #- Retrieve name of dataset
  data1 <- format(deparse(substitute(data)))

  X <-stats::model.matrix(object=formula, data=data)

  #- Normalise X (except intercept/b0) --------------------#
  #X_sc <- apply(X[,-1],2,scale)

  #- Calculate mean and sd of X cols
  mean_X <- colMeans(X[,-1])
  sd_X <- apply(X[,-1],2,stats::sd)

  #- Center X
  X_c <- sweep(X[,-1],2,mean_X)
  #- Scale by standard deviation
  X_sc <- sweep(X_c,2,sd_X,"/")

  #print("sd_X:");print(sd_X)

  #- Estimate beta_hat
  XtXinv <- solve(t(X_sc)%*%X_sc+lambda*diag(ncol(X_sc)))
  Xty <- t(X_sc)%*%y

  beta_hat_1_p_sc <- XtXinv%*%Xty
  #print("beta_hat_1_p_sc: ");print(beta_hat_1_p_sc)
  beta_hat_1_p <- beta_hat_1_p_sc/sd_X
  #print("beta_hat_1_p: ");print(beta_hat_1_p)

  #- Estimate b0
  beta0_hat <- mean(y) - t(beta_hat_1_p) %*% mean_X

  beta_hat <- as.vector(c(beta0_hat,beta_hat_1_p))
  names(beta_hat) <- c("Intercept",dimnames(beta_hat_1_p)[[1]])

  #- Fitted values
  y_hat <- X%*%beta_hat
  y_hat <- as.vector(y_hat)

  #- Residuals
  e_hat <- y - y_hat

  # Collect results
  ridgereg <- list(call = call,
                 formula = formula,
                 data1=data1,
                 coefficients = beta_hat,
                 fitted_values = y_hat,
                 residuals = e_hat
  )
  class(ridgereg) <- "ridgereg"
  return(ridgereg)
}
