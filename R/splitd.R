
#' @title Split-Dichotomized Regression Model
#' 
#' @description
#' Split-dichotomized regression model.
#' 
#' @param start.model a regression model
#' 
#' @param x_ \link[base]{language}
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param id \link[base]{logical} \link[base]{vector}, indices of training (`TRUE`) and test (`FALSE`) subjects 
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @section Split-Dichotomized Regression Model:
#' 
#' Function [splitd()] performs a univariable regression model on the test set with a dichotomized predictor, using a dichotomizing rule determined by a recursive partitioning of the training set. 
#' Specifically, given a training-test sample split,
#' \enumerate{
#' \item find the *dichotomizing rule* \eqn{\mathcal{D}} of the predictor \eqn{x_0} given the response \eqn{y_0} in the training set (via function [node1()]);
#' \item fit a univariable regression model of the response \eqn{y_1} with the dichotomized predictor \eqn{\mathcal{D}(x_1)} in the test set.
#' }
#' Currently the Cox proportional hazards (\link[survival]{coxph}) regression for \link[survival]{Surv} response, logistic (\link[stats]{glm}) regression for \link[base]{logical} response and linear (\link[stats]{lm}) regression for \link[stats]{gaussian} response are supported.
#' 
#' @returns
#' 
#' Function [splitd()] returns a \link[base]{function}, 
#' the dichotomizing rule \eqn{\mathcal{D}} based on the training set \eqn{(y_0, x_0)}, 
#' with additional attributes
#' \describe{
#' \item{`attr(,'p1')`}{\link[base]{double} scalar, \eqn{p_1 = \text{Pr}(\mathcal{D}(x_1)=1)}}
#' \item{`attr(,'effsize')`}{\link[base]{double} scalar, univariable regression coefficient estimate of \eqn{y_1\sim\mathcal{D}(x_1)}}
#' }
#' 
#' @keywords internal
#' @importFrom rpart rpart
#' @importFrom stats update
#' @export
splitd <- function(start.model, x_, data, id, ...) {
  
  y <- start.model$y
  
  hc <- unclass(data)$hypercolumns
  data <- unclass(data)$df
  
  x <- eval(x_, envir = hc)
  
  # `id`: training set
  rule <- rpart(formula = y[id] ~ x[id], cp = .Machine$double.eps, maxdepth = 2L) |>
    node1()
  
  # `-id`: test set (`id` is `integer`)
  y_ <- y[-id]
  data_ <- data[-id, , drop = FALSE]
  dx_ <- tryCatch(rule(x[-id]), warning = identity)
  if (inherits(dx_, what = 'warning')) return(invisible()) # exception
  if ('x.' %in% names(data)) stop('do not allow `x.` as an original column in `data`')
  data_$x. <- dx_
  
  suppressWarnings(m_ <- update(start.model, formula. = . ~ . + x., data = data_))

  cf_ <- m_$coefficients[length(m_$coefficients)]
  
  attr(rule, which = 'p1') <- mean.default(dx_, na.rm = TRUE)
  attr(rule, which = 'x') <- x_
  attr(rule, which = 'effsize') <- if (is.finite(cf_)) unname(cf_) else NA_real_
  attr(rule, which = 'model') <- m_ # only model formula needed for [predict.node1]!!!
  # class(rule) <- c('splitd', class(rule)) # removed Spring 2025!!!
  # [predict.splitd()] will become [predict.node1()] !!!
  return(rule)
}















#' @title Regression Models with Optimal Dichotomizing Predictors
#' 
#' @description
#' Regression models with optimal dichotomizing predictor(s), used either as boolean or continuous predictor(s).
#' 
#' @param object an [node1] object, as an element of the \link[stats]{listof} return from functions [add_dummy()] or [add_dummy_partition()]
#' 
#' @param newdata \link[base]{data.frame}, candidate \link[base]{numeric} predictors \eqn{x}'s must have the same \link[base]{name} and \link[base]{dim}ension as the training data. If missing, the training data is used
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' Function [predict.node1()] returns a updated regression model.
#' 
#' @keywords internal
#' @importFrom stats predict update
#' @export predict.node1
#' @export
predict.node1 <- function(object, newdata, ...) {
  
  if ('x.' %in% names(newdata)) stop('do not allow existing name `x.` in `newdata`')
  hc <- unclass(newdata)$hypercolumns
  newd <- unclass(newdata)$df
  
  newd$x. <- object |>
    attr(which = 'x', exact = TRUE) |> # a 'langugage'!
    eval(envir = hc) |> 
    object() # dichotomize!
  
  object |>
    attr(which = 'model', exact = TRUE) |> 
    update(data = newd) |> 
    suppressWarnings()
  
}
