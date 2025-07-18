% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_dummy.R
\name{S3_add_dummy}
\alias{S3_add_dummy}
\alias{subset.add_dummy}
\alias{predict.add_dummy}
\title{S3 Method Dispatches to \code{'add_dummy'} Class}
\usage{
\method{subset}{add_dummy}(x, subset, ...)

\method{predict}{add_dummy}(object, ...)
}
\arguments{
\item{x, object}{an object returned from functions \code{\link[=add_dummy_partition]{add_dummy_partition()}} or \code{\link[=add_dummy]{add_dummy()}}}

\item{subset}{\link[base]{language}}

\item{...}{additional parameters of function \code{\link[=update.node1]{update.node1()}}, e.g., \code{newdata}}
}
\value{
Function \code{\link[=subset.add_dummy]{subset.add_dummy()}} returns a \code{\link[=add_dummy]{add_dummy()}} object.

Function \code{\link[=predict.add_dummy]{predict.add_dummy()}} returns a \link[stats]{listof} regression models.
}
\description{
S3 Method Dispatches to \code{'add_dummy'} Class
}
\details{
Function \code{\link[=subset.add_dummy]{subset.add_dummy()}}, default subset \code{(p1>.15 & p1<.85)}.
See explanation of \eqn{p_1} in function \code{\link[=splitd]{splitd()}}.
}
\keyword{internal}
