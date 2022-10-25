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
print.linreg <- function(x,...){
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
residuals.linreg <- function(object, ...){


  return(object$residuals)

}

#' Predicted values
#'
#' Returns the predicted values
#'
#' @param object Refers to the object return by the linear regression function
#'
#' @return object$fitted_values
#' @export
#'
#' @examples data(iris)
#'           s<-linreg(formula=Petal.Length~Species, data= iris)
#'           pred(s)
pred <- function(object){


  return(object$fitted_values)
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
#'           s<-linreg(formula=Petal.Length~Species, data= iris)
#'           coef(s)
coef.linreg <- function(object,...){

  return(object$coefficients)
}
#' Summary of linear regression function
#'
#' Returns a summary of the results from the linear regression: estimated
#' coefficients, their standard errors, tvalues and p-values, as well as
#' the residual variance.
#'
#' @param object Refers to the object return by the linear regression function
#' @param ... arguments to be passed to methods
#'
#' @return data frame with the final computations
#' @export
#'
#' @examples data(iris)
#'           s<-linreg(formula=Petal.Length~Species, data= iris)
#'           summary(s)
summary.linreg <- function(object,...){

  df <- object$df
  res_std_err <- round(sqrt(object$residual_variance),4)
  estimates <- coef.linreg(object)
  estimates_print <- format(estimates, digits=4)
  stderr <- round(sqrt(object$coefficients_variance),5)
  tvalues <- round(object$coefficients_tvalues,2)
  pvalues <- object$coefficients_pvalues
  pvalues_print <- format(object$coefficients_pvalues,digit=3)

  sign_level <- vector(length=length(estimates))
  sign_level[pvalues<0.1] <- "."
  sign_level[pvalues<0.05] <- "*"
  sign_level[pvalues<0.01] <- "**"
  sign_level[pvalues<0.001] <- "***"

  coeffs <- as.data.frame(cbind(estimates_print,stderr,tvalues,pvalues_print,sign_level))
  colnames(coeffs) <- c("Estimate","Std.Error","t value"," Pr(>|t|)","")
  cat("\n","Coefficients","\n")
  print(format(coeffs, justify="left"))
  cat("\n","Residual standard error:",res_std_err,"on",df,"degrees of freedom")

}
#' Plot
#'
#' @param x linreg object
#' @param ... arguments to be passed to methods
#'
#' @return Makes two plots
#' @export
#'
#' @examples
#' data(iris)
#' linres <- linreg(formula=Sepal.Length ~ Petal.Length + Petal.Width, data=iris)
#' plot(linres)
plot.linreg <- function(x, ...) {
  #----------------------------------------------------------------------------#
  #- Set data for plotting
  #----------------------------------------------------------------------------#
  df <- data.frame(cbind(pred(x), residuals.linreg(x)))

  #----------------------------------------------------------------------------#
  #- Data management for plotting
  #----------------------------------------------------------------------------#
  #- Standardized residuals
  resid_std <- residuals.linreg(x)/sqrt(x$residual_variance)
  resid_std_abs_sqrt <- sqrt(abs(resid_std))
  #- Square root of the absolute value of standardized residuals
  df$resid_std_abs_sqrt <- resid_std_abs_sqrt
  df$ij <- row.names(df)
  names(df) <- c("pred","resid","resid_std_abs_sqrt","ij")

  #- Find top third value
  third_topval <- resid_std_abs_sqrt[order(resid_std_abs_sqrt, decreasing = TRUE)][3]

  #----------------------------------------------------------------------------#
  #- Make plots
  #----------------------------------------------------------------------------#

  ggplot2::theme_set(ggplot2::theme_bw())

  #- Common plot parameters
  cpt <- paste0("linreg(",format(x$formula),")")
  xaxis <- "pred"
  xlab <- "Fitted values"
  cutoff_y_var <- "resid_std_abs_sqrt"
  cutoff_y_val <- third_topval
  id <- "ij"

  #- Plot 1
  title1 <- "Residuals Vs Fitted"
  yaxis1 <- "resid"
  ylab1 <- "Residuals"

  pl1 <- make_plot(df=df, title=title1, cpt=cpt, xaxis=xaxis, yaxis=yaxis1, xlab=xlab,  ylab=ylab1,
                   cutoff_y_var=cutoff_y_var, cutoff_y_val=cutoff_y_val, id=id)

  plot(pl1)

  #- Plot 2
  title2 <- "Scale-Location"
  yaxis2 <- "resid_std_abs_sqrt"
  ylab2 <- expression(sqrt("|standardised residuals|"))

  pl2 <- make_plot(df=df, title=title2, cpt=cpt, xaxis=xaxis, yaxis=yaxis2, xlab=xlab,  ylab=ylab2,
                   cutoff_y_var=cutoff_y_var, cutoff_y_val=cutoff_y_val, id=id)

  plot(pl2)

}

#' Make_plot
#' 
#' Help function to the plot.linreg function
#'
#' @param df data frame that contains the data to be plotted
#' @param title title of the plot
#' @param cpt caption
#' @param xaxis x axis variable
#' @param yaxis y axis variable
#' @param xlab label for the x axis
#' @param ylab label for the y axis
#' @param cutoff_y_var cutoff variable for which the cutoff_y_val should be 
#'                     applied (see cutoff_y_val)
#' @param cutoff_y_val cutoff value for which id labels should be displayed
#'                     for the variable as given in cutoff_y_var
#' @param id variable that holds the id values of the observations
#'
#' @return ggplot object
#' @export

make_plot <- function(df, title, cpt, xaxis, yaxis, xlab, ylab, cutoff_y_var,
                      cutoff_y_val, id) {

  resid_plot <- df[,yaxis]
  m <- mean(resid_plot)
  pred_plot <- df[,xaxis]
  id_plot <- df[,id]
  cutoff_y_var <- df[,cutoff_y_var]
  ylim_min <- min(0, round(min(resid_plot),4)-0.1)
  ylim_max <- round(max(resid_plot),4 + 0.1)

  #- Calculate summary statistics
  y_means <- stats::aggregate(resid_plot, list(pred_plot), FUN=mean)
  names(y_means) <- c("pred_plot","resid_plot")
  y_medians <- stats::aggregate(resid_plot, list(pred_plot), FUN=stats::median)
  names(y_medians) <- c("pred_plot","resid_plot")

  pl <-
    ggplot2::ggplot(data = df) +
    ggplot2::aes(x=pred_plot, y=resid_plot) +
    ggplot2::geom_point(shape = 1) +

    ggplot2::ylim(ylim_min, ylim_max) +

    ggplot2::geom_text(ggplot2::aes(label=ifelse((cutoff_y_var>=cutoff_y_val),
                                                 as.character(id_plot),'')),
                       hjust=-0.2, vjust=-0.2,
                       check_overlap = TRUE, size=3) +

    #- Add mean and medians line plots
    ggplot2::geom_line(data=y_means, ggplot2::aes(color="mean")) +
    ggplot2::geom_line(data=y_medians, ggplot2::aes(color="median")) +
    ggplot2::scale_color_manual(name = "", values = c("mean" = "red",
                                                      "median" = "darkblue")) +

    #- Title, axes and caption
    ggplot2::labs(title=title, y=ylab, x=xlab, caption=paste0(cpt)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.caption = ggplot2::element_text(hjust = 0.5))

  return(pl)
}
