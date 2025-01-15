
#' @title Split-Dichotomized Regression Model
#' 
#' @description
#' Split-dichotomized regression model.
#' 
#' @param null.model a regression model
#' 
#' @param x \link[base]{numeric} \link[base]{vector}, predictor \eqn{x}
#' 
#' @param id \link[base]{logical} \link[base]{vector}, indices of training (`TRUE`) and test (`FALSE`) subjects 
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @section Split-Dichotomized Regression Model:
#' 
#' Function [.splitd] performs a univariable regression model on the test set with a dichotomized predictor, using a dichotomizing rule determined by a recursive partitioning of the training set. 
#' Specifically, given a training-test sample split,
#' \enumerate{
#' \item find the *dichotomizing rule* \eqn{\mathcal{D}} of the predictor \eqn{x_0} given the response \eqn{y_0} in the training set (via [rpartD]);
#' \item fit a univariable regression model of the response \eqn{y_1} with the dichotomized predictor \eqn{\mathcal{D}(x_1)} in the test set.
#' }
#' Currently the Cox proportional hazards (\link[survival]{coxph}) regression for \link[survival]{Surv} response, logistic (\link[stats]{glm}) regression for \link[base]{logical} response and linear (\link[stats]{lm}) regression for \link[stats]{gaussian} response are supported.
#' 
#' @returns
#' 
#' Function [.splitd] returns a \link[base]{function}, 
#' the dichotomizing rule \eqn{\mathcal{D}} based on the training set \eqn{(y_0, x_0)}, 
#' with additional attributes
#' \describe{
#' \item{`attr(,'p1')`}{\link[base]{double} scalar, \eqn{p_1 = \text{Pr}(\mathcal{D}(x_1)=1)}}
#' \item{`attr(,'cf')`}{\link[base]{double} scalar, univariable regression coefficient estimate of \eqn{y_1\sim\mathcal{D}(x_1)}}
#' \item{`attr(,'formula')`}{(optional), for function [predict.splitd]}
#' }
#' 
#' @keywords internal
#' @importFrom stats update
#' @export
.splitd <- function(null.model, x, id, ...) {
  
  y <- null.model$y
  
  # `id`: training set
  rule <- rpartD(y = y[id], x = x[id], check_degeneracy = TRUE)
  
  # `!id`: test set
  y_ <- y[!id]
  dx_ <- tryCatch(rule(x[!id]), warning = identity)
  if (inherits(dx_, what = 'warning')) return(invisible()) # exception
  
  suppressWarnings(m_ <- update(null.model, formula. = y ~ x, data = data.frame(y = y_, x = dx_)))
  
  #suppressWarnings(m_ <- if (inherits(y, what = 'Surv')) {
  #  coxph(formula = y_ ~ dx_)
  #} else if (is.logical(y) || all(y %in% c(0, 1))) {
  #  glm(formula = y_ ~ dx_, family = binomial(link = 'logit'))
  #} else lm(formula = y_ ~ dx_))
  
  cf_ <- m_$coefficients[length(m_$coefficients)]
  
  attr(rule, which = 'p1') <- mean.default(dx_, na.rm = TRUE)
  attr(rule, which = 'cf') <- if (is.finite(cf_)) unname(cf_) else NA_real_
  attr(rule, which = 'model') <- m_ # needed for [predict.splitd]
  class(rule) <- c('splitd', class(rule))
  return(rule)
}









#' @title Repeated Split-Dichotomized Regression Model
#' 
#' @param null.model see function [.splitd]
#' 
#' @param ids \link[base]{list} of \link[base]{logical} \link[base]{vector}s, multiple copies of indices of repeated training-test sample splits.  
#' 
#' @param ... additional parameters of function [.splitd]
#' 
#' @details 
#' Function [.splitd_] fits multiple [.splitd] models on the response \eqn{y} and predictor \eqn{x}, based on each copy of the repeated training-test sample splits.
#' 
#' @returns
#' Function [.splitd_] returns a \link[base]{list} of [.splitd] objects.
#' 
#' @keywords internal
#' @export
.splitd_ <- function(null.model, ids, ...) {
  ret_ <- lapply(ids, FUN = function(id) .splitd(null.model = null.model, id = id, ...))
  ret <- ret_[lengths(ret_) > 0L]
  class(ret) <- c('splitd.list', class(ret))
  return(ret)
}







#' @title Quantile of Split-Dichotomized Regression Models
#' 
#' @description
#' Quantile(s) of `'splitd.list'` object.
#' 
#' @param x a `'splitd.list'` object, returned from function [.splitd_]
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
#' \item {collect the univariable regression coefficient estimate from each [.splitd] model;}
#' \item {find the nearest-even (i.e., `type = 3`) \link[stats]{quantile} of the coefficients from Step 1;}
#' \item {the [.splitd] models corresponding to the selected coefficient quantile(s) in Step 2, is returned.}
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
  cf <- vapply(x, FUN = attr, which = 'cf', exact = TRUE, FUN.VALUE = NA_real_)
  q_ <- quantile(cf, probs = probs, type = 3L, na.rm = TRUE)
  return(x[match(q_, table = cf)])
}






#' @title Regression Models with Optimal Dichotomizing Predictors
#' 
#' @description
#' Regression models with optimal dichotomizing predictor(s), used either as boolean or continuous predictor(s).
#' 
#' @param object an [.splitd] object
#' 
#' @param newdata \link[base]{data.frame}, candidate \link[base]{numeric} predictors \eqn{x}'s must have the same \link[base]{name} and \link[base]{dim}ension as the training data. If missing, the training data is used
#' 
#' @param formula ..
#' 
# @param boolean \link[base]{logical} scalar, whether to use the *dichotomized* predictor (default, `TRUE`), or the continuous predictor (`FALSE`)
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
predict.splitd <- function(
    object, 
    newdata, 
    formula = attr(object, which = 'formula', exact = TRUE),
    # boolean = TRUE, # move to the non-dichotomized version
    ...
) {
  
  if (!length(formula)) stop('`formula` needed')
  y <- eval(formula[[2L]], envir = newdata)
  x <- eval(formula[[3L]], envir = newdata)
  #if (boolean) x <- object(newx = x)
  x <- object(newx = x)
  
  oldmodel <- attr(object, which = 'model', exact = TRUE)
  suppressWarnings(update(oldmodel, formula. = y ~ x, data = data.frame(y = y, x = x)))
  
}
