
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
#' Function [splitd] performs a univariable regression model on the test set with a dichotomized predictor, using a dichotomizing rule determined by a recursive partitioning of the training set. 
#' Specifically, given a training-test sample split,
#' \enumerate{
#' \item find the *dichotomizing rule* \eqn{\mathcal{D}} of the predictor \eqn{x_0} given the response \eqn{y_0} in the training set (via [rpart1]);
#' \item fit a univariable regression model of the response \eqn{y_1} with the dichotomized predictor \eqn{\mathcal{D}(x_1)} in the test set.
#' }
#' Currently the Cox proportional hazards (\link[survival]{coxph}) regression for \link[survival]{Surv} response, logistic (\link[stats]{glm}) regression for \link[base]{logical} response and linear (\link[stats]{lm}) regression for \link[stats]{gaussian} response are supported.
#' 
#' @returns
#' 
#' Function [splitd] returns a \link[base]{function}, 
#' the dichotomizing rule \eqn{\mathcal{D}} based on the training set \eqn{(y_0, x_0)}, 
#' with additional attributes
#' \describe{
#' \item{`attr(,'p1')`}{\link[base]{double} scalar, \eqn{p_1 = \text{Pr}(\mathcal{D}(x_1)=1)}}
#' \item{`attr(,'effsize')`}{\link[base]{double} scalar, univariable regression coefficient estimate of \eqn{y_1\sim\mathcal{D}(x_1)}}
#' \item{`attr(,'formula')`}{(optional), for function [predict.splitd]}
#' }
#' 
#' @keywords internal
#' @importFrom stats update
#' @export
splitd <- function(start.model, x_, data, id, ...) {
  
  y <- start.model$y
  x <- eval(x_, envir = data)
  
  # `id`: training set
  rule <- rpart1(y = y[id], x = x[id], check_degeneracy = TRUE)
  
  # `!id`: test set
  y_ <- y[!id]
  data_ <- data[!id, , drop = FALSE]
  dx_ <- tryCatch(rule(x[!id]), warning = identity)
  if (inherits(dx_, what = 'warning')) return(invisible()) # exception
  if ('x.' %in% names(data)) stop('do not allow `x.` as an original column in `data`')
  data_$x. <- dx_
  
  suppressWarnings(m_ <- update(start.model, formula. = . ~ . + x., data = data_))

  cf_ <- m_$coefficients[length(m_$coefficients)]
  
  attr(rule, which = 'p1') <- mean.default(dx_, na.rm = TRUE)
  attr(rule, which = 'x') <- x_
  attr(rule, which = 'effsize') <- if (is.finite(cf_)) unname(cf_) else NA_real_
  attr(rule, which = 'model') <- m_ # needed for [predict.splitd]
  class(rule) <- c('splitd', class(rule))
  return(rule)
}









#' @title Repeated Split-Dichotomized Regression Model
#' 
#' @param ids \link[base]{list} of \link[base]{logical} \link[base]{vector}s, multiple copies of indices of repeated training-test sample splits.  
#' 
#' @param ... additional parameters of function [splitd]
#' 
#' @details 
#' Function [splitd_] fits multiple [splitd] models on the response \eqn{y} and predictor \eqn{x}, based on each copy of the repeated training-test sample splits.
#' 
#' @returns
#' Function [splitd_] returns a \link[base]{list} of [splitd] objects.
#' 
#' @keywords internal
#' @export
splitd_ <- function(ids, ...) {
  ret_ <- lapply(ids, FUN = function(id) splitd(id = id, ...))
  ret <- ret_[lengths(ret_, use.names = FALSE) > 0L]
  class(ret) <- c('splitd.list', class(ret))
  return(ret)
}







#' @title Quantile of Split-Dichotomized Regression Models
#' 
#' @description
#' Quantile(s) of `'splitd.list'` object.
#' 
#' @param x a `'splitd.list'` object, returned from function [splitd_]
#' 
#' @param probs \link[base]{double} scalar or \link[base]{vector}, see \link[stats]{quantile}
#' 
#' @details
#' 
#' Function [quantile.splitd.list] is a method dispatch of the S3 generic function \link[stats]{quantile} on `'splitd.list'` object.
# finds the \link[stats]{quantile}
# of the univariable regression coefficient (i.e., effect size) of a dichotomized predictor,
# based on repeated given training-test sample splits.
#' Specifically,
#' 
#' \enumerate{
#' \item {collect the univariable regression coefficient estimate from each [splitd] model;}
#' \item {find the nearest-even (i.e., `type = 3`) \link[stats]{quantile} of the coefficients from Step 1;}
#' \item {the [splitd] models corresponding to the selected coefficient quantile(s) in Step 2, is returned.}
#' }
#' 
#' @returns
#' Function [quantile.splitd.list] returns a `'splitd.list'` object.
#' 
#' @keywords internal
#' @importFrom stats quantile
#' @method quantile splitd.list
#' @export quantile.splitd.list
#' @export
quantile.splitd.list <- function(x, probs, ...) {
  effsize <- vapply(x, FUN = attr, which = 'effsize', exact = TRUE, FUN.VALUE = NA_real_)
  q_ <- quantile(effsize, probs = probs, type = 3L, na.rm = TRUE)
  return(x[match(q_, table = effsize)])
}






#' @title Regression Models with Optimal Dichotomizing Predictors
#' 
#' @description
#' Regression models with optimal dichotomizing predictor(s), used either as boolean or continuous predictor(s).
#' 
#' @param object an [splitd] object
#' 
#' @param newdata \link[base]{data.frame}, candidate \link[base]{numeric} predictors \eqn{x}'s must have the same \link[base]{name} and \link[base]{dim}ension as the training data. If missing, the training data is used
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' Function [predict.splitd] returns a regression model, 
#' either a \link[survival]{coxph} model for \link[survival]{Surv} response, 
#' a \link[stats]{glm} for \link[base]{logical} response, 
#' or a \link[stats]{lm} model for \link[base]{numeric} response.
#' 
#' @keywords internal
#' @importFrom stats predict update
#' @export predict.splitd
#' @export
predict.splitd <- function(object, newdata, ...) {
  
  if ('x.' %in% names(newdata)) stop('do not allow existing name `x.` in `newdata`')
  x_ <- attr(object, which = 'x', exact = TRUE)
  newdata$x. <- object(newx = eval(x_, envir = newdata))
  
  m_ <- attr(object, which = 'model', exact = TRUE)
  suppressWarnings(update(m_, data = newdata))
  
}
