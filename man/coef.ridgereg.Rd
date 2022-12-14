% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_rr.R
\name{coef.ridgereg}
\alias{coef.ridgereg}
\title{Coefficients}
\usage{
\method{coef}{ridgereg}(object, ...)
}
\arguments{
\item{object}{Refers to the object return by the linear regression function}

\item{...}{arguments to be passed to methods}
}
\value{
object$coefficients
}
\description{
Returns the coefficients
}
\examples{
data(iris)
          res<-ridgereg(formula=Petal.Length~Species, data= iris, lambda=2)
          coef(res)
}
