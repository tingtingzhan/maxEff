
#' @title Split-Dichotomized Regression Model
#' 
#' @description
#' Split-dichotomized regression model.
#' 
#' @param start.model a regression model
#' 
#' @param x_ \link[base]{language}
#' 
#' @param x \link[base]{numeric} \link[base]{vector}
#' 
#' @param data \link[spatstat.geom]{hyperframe}
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
splitd <- function(start.model, x_, x, data, id, ...) {
  
  y <- start.model$y
  
  data_df <- unclass(data)$df
  
  # `id`: training set
  rule <- rpart(formula = y[id] ~ x[id], cp = .Machine$double.eps, maxdepth = 2L) |>
    node1(nm = x_)
  
  # `-id`: test set (`id` is `integer`)
  y_ <- y[-id]
  data_ <- data_df[-id, , drop = FALSE]
  dx_ <- tryCatch(rule(x[-id]), warning = identity)
  if (inherits(dx_, what = 'warning')) return(invisible()) # exception
  if ('x.' %in% names(data_df)) stop('do not allow `x.` as an original column in `data`')
  data_$x. <- dx_
  
  m_ <- update(start.model, formula. = . ~ . + x., data = data_)

  cf <- m_$coefficients
  cf_ <- cf[length(cf)]
  
  attr(rule, which = 'p1') <- mean.default(dx_, na.rm = TRUE)
  
  attr(rule, which = 'effsize') <- if (is.finite(cf_)) unname(cf_) else NA_real_
  attr(rule, which = 'model') <- m_ # only model formula needed for [predict.add_dummy_]!!!
  # class(rule) <- c('splitd', class(rule)) # removed Spring 2025!!!
  return(rule)
}



