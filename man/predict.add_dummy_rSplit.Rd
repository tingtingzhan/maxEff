% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_dummy.R
\name{predict.add_dummy_rSplit}
\alias{predict.add_dummy_rSplit}
\title{Regression Models with Optimal Dichotomizing Predictors}
\usage{
\method{predict}{add_dummy_rSplit}(object, ...)
}
\arguments{
\item{object}{an \link{add_dummy_rSplit} object}

\item{...}{additional parameters of function \link{predict.splitd}, e.g., \code{newdata}}
}
\value{
Function \link{predict.add_dummy_rSplit} returns a \link[base]{list} of regression models.
}
\description{
Regression models with optimal dichotomizing predictor(s), used either as boolean or continuous predictor(s).
}
\examples{
# see ?add_dummy_rSplit
}
