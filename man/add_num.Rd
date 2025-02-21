% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_num.R
\name{add_num}
\alias{add_num}
\title{add_num}
\usage{
add_num(
  start.model,
  x,
  data = eval(start.model$call$data),
  mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()),
  ...
)
}
\arguments{
\item{start.model}{a regression model (e.g., \link[stats]{lm}, \link[stats]{glm}, or \link[survival]{coxph}, etc.)}

\item{x}{one-sided \link[stats]{formula} to specify
the \link[base]{numeric} predictors \eqn{x}'s as the columns of one \link[base]{matrix} column in \code{data}}

\item{data}{\link[base]{data.frame}}

\item{mc.cores}{\link[base]{integer} scalar, see function \link[parallel]{mclapply}}

\item{...}{additional parameters, currently of no use}
}
\value{
Function \link{add_num} ..
}
\description{
add_num
}
\details{
Function \link{add_num} ..
}
\examples{
library(spatstat.grouped)
library(spatstat.grouped.data)
library(survival) # to help ?spatstat.geom::hyperframe understand ?survival::Surv

s = grouped_ppp(hladr + phenotype ~ OS + gender + age | patient_id/image_id, data = wrobel_lung)
sQ = s |>
 aggregate_quantile(by = ~ patient_id, probs = seq.int(from = 0, to = 1, by = .1))
dim(sQ)

sQ0 = sQ[1:100,] # training set
sQ1 = sQ[-(1:100),] # test set

(m = coxph(OS ~ gender, data = sQ0) |>
 add_num(x = ~ hladr.quantile) |>
 sort_by(y = abs(effsize)) |>
 head(n = 2L))
predict(m, newdata = sQ1)

}
