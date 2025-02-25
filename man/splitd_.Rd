% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/splitd.R
\name{splitd_}
\alias{splitd_}
\title{Repeated Split-Dichotomized Regression Model}
\usage{
splitd_(ids, ...)
}
\arguments{
\item{ids}{\link[base]{list} of \link[base]{logical} \link[base]{vector}s, multiple copies of indices of repeated training-test sample splits.}

\item{...}{additional parameters of function \link{splitd}}
}
\value{
Function \link{splitd_} returns a \link[base]{list} of \link{splitd} objects.
}
\description{
Repeated Split-Dichotomized Regression Model
}
\details{
Function \link{splitd_} fits multiple \link{splitd} models on the response \eqn{y} and predictor \eqn{x}, based on each copy of the repeated training-test sample splits.
}
\keyword{internal}
