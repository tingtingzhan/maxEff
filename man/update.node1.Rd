% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splitd.R
\name{update.node1}
\alias{update.node1}
\title{Regression Models with Optimal Dichotomizing Predictors}
\usage{
\method{update}{node1}(object, newdata, ...)
}
\arguments{
\item{object}{an \link{node1} object, as an element of the \link[stats]{listof} return from functions \code{\link[=add_dummy]{add_dummy()}} or \code{\link[=add_dummy_partition]{add_dummy_partition()}}}

\item{newdata}{\link[base]{data.frame}, candidate \link[base]{numeric} predictors \eqn{x}'s must have the same \link[base]{name} and \link[base]{dim}ension as the training data. If missing, the training data is used}

\item{...}{additional parameters, currently not in use}
}
\value{
Function \code{\link[=update.node1]{update.node1()}} returns a updated regression model.
}
\description{
Regression models with optimal dichotomizing predictor(s), used either as boolean or continuous predictor(s).
}
\keyword{internal}
