#' Linear regression functions
#'
#' @param formula Y dependent variable and X independent variable of the data frame
#' @param data Data frame input by the user
#'
#' @return res
#' @export
#'
#' @examples data(iris)
#'           linreg(Petal.Length~Species, iris)
linreg <- function(formula, data){
  stopifnot(class(formula) == "formula") #check is it's a formula object
  stopifnot(is.data.frame(data)) # check that the data is a data.frame


#With model matrix we are extracting the X matrix
  X <- stats::model.matrix(formula,data)         # independent variable
  y <- data[all.vars(formula)[1]]         # we took the all the columns of the Y variable(dependant)
  y1<- as.matrix(y)                       # we transform the Y's dependant variable from a data.frame format to a matrix, for us to be able to make operations

# Calculation of formulas
    #Beta
  B_m <-  solve(t(X)%*%X)%*%t(X)%*%y1

  B<- as.vector(B_m)
  names(B) <- dimnames(B_m)[[1]]          #adding the names to the vector

   #fitted values
   y_h <- X%*%B                            #returns the fitted values of all the dependant variables, so it has 1 column and the determine number of rows from all.vars


  #The residuals
  e <- y1 - y_h                           #this could be interpreted as the error or noise


  #The degrees of freedom
  Xn <- nrow(X)                           #we took the number of rows
  Xp <- ncol(X)                           #we took the number of columns
  df <- Xn- Xp                            # using the formula, n is the number of observations and p is the number of parameters in the model


  #The residual variance
  Rvar <- (t(e)%*%e)/df                   #e is the residuals or errors

  #The variance of the regression coefficients
  varB <- as.numeric(Rvar)*solve(t(X)%*%X) #remember solve is to calculate the inverse matrix
                                            #as numeric allows to perform the multiplication between the matrix, other wise using it as a matrix of 1,1 it does not compute
                                           #at the same time the the multiplication form * can be performed
  #The t-values for each coefficient
  tB <- B/sqrt(diag(varB))                  #it has to be only the diagonal values of varB, because otherwise you took the variance and covariance of the formula
  #return(tB)                              # notice the difference of the notation between varB and varB with hat on the varB Lab4 pdf
  #The p  values
  p_v <- 2*stats::pt(abs(as.vector(tB)),df,lower.tail = FALSE)

  data1 <- format(deparse(substitute(data))) # extract the name of the data frame, before adding the parameter to the list

  call<-match.call()                                          #deparse() transform unvaluated expression to strings in conjunction with substitute helps to extract labels from a data frame, helpful for plot labels
  res <- list(call=call,formula=formula,data1=data1,coefficients=B,fitted_values= as.vector(y_h), df=df,residuals=as.vector(e)  ,residual_variance=as.numeric(Rvar),
                 coefficients_variance= diag(varB), coefficients_tvalues=as.vector(tB), coefficients_pvalues=p_v) #, coefficients_palues=p_values
  class(res) ="linreg"                      #creating the type of class
  return(res)                               #you must return the list of your function to be able to implement methods outside the function in a S3 structur

}

#####methods


