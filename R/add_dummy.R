

#' @title Additional \link[base]{logical} Predictor
#' 
#' @param start.model a regression model, e.g., 
#' \link[stats]{lm}, \link[stats]{glm}, or \link[survival]{coxph}, etc.
#' 
#' @param x one-sided \link[stats]{formula},
#' \link[base]{numeric} predictors \eqn{x}'s as the columns of one \link[base]{matrix} column in `data`
#' 
#' @param data (optional) \link[base]{data.frame} in the model \link[base]{call} of `start.model`
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @returns
#' Function [add_dummy()] returns an object of class `'add_dummy'`.
#' 
#' @keywords internal
#' @importFrom parallel mclapply
#' @importFrom rpart rpart
#' @importFrom stats update
#' @export
add_dummy <- function(
    start.model, 
    x, 
    data = eval(start.model$call$data),
    mc.cores = getOption('mc.cores'), 
    ...
) {
  
  tmp <- .prepare_add_(start.model = start.model, x = x, data = data)
  y <- tmp$y
  data_ <- tmp$data
  x_ <- tmp$x_
  xval <- tmp$xval
  
  sq <- x_ |>
    seq_along()
  
  # all `rule`s
  rule. <- sq |>  
    mclapply(mc.cores = mc.cores, FUN = \(i) {
      rpart(formula = y ~ xval[[i]], cp = .Machine$double.eps, maxdepth = 2L) |> # partition rule based on complete data
        node1(nm = x_[[i]])
    })
  
  # all dichotomized predictors
  x. <- sq |> 
    mclapply(mc.cores = mc.cores, FUN = \(i) {
      rule.[[i]](xval[[i]]) # partition rule applied to complete data
    })
  
  # only choose unique dichotomized predictors!!!
  out <- sq[!duplicated.default(x.)] |>  
    mclapply(mc.cores = mc.cores, FUN = \(i) {
    #lapply(FUN = \(i) {
      data_$x. <- x.[[i]]
      m_ <- update(start.model, formula. = . ~ . + x., data = data_)
      cf <- m_$coefficients
      cf_ <- cf[length(cf)]
      rule <- rule.[[i]]
      attr(rule, which = 'p1') <- mean.default(data_$x., na.rm = TRUE)
      attr(rule, which = 'effsize') <- if (is.finite(cf_)) unname(cf_) else NA_real_
      attr(rule, which = 'model') <- m_ # only model formula needed for [predict.add_dummy_]!!!
      class(rule) <- c('add_dummy_', class(rule))
      return(rule)
    })
  
  class(out) <- c('add_dummy', 'add_', 'listof', class(out))
  return(out)

}













#' @title [labels.add_dummy]
#' 
#' @param object a [add_dummy] object
#' 
#' @param ... ..
#' 
#' @returns
#' Function [labels.add_dummy()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @keywords internal
#' @export labels.add_dummy
#' @export
labels.add_dummy <- function(object, ...) {
  object |>
    vapply(FUN = labels.node1, FUN.VALUE = '')
}






#' @title [print.add_dummy]
#' 
#' @param x an object returned from functions 
#' [add_dummy_partition()] or [add_dummy()]
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' ..
#' 
#' @returns
#' Function [print.add_dummy()] does not have a returned value
#' 
#' @keywords internal
#' @export print.add_dummy
#' @export
print.add_dummy <- function(x, ...) {
  x |>
    labels.add_dummy() |>
    cat(sep = '\n')
}




if (FALSE) {
  unique.add_dummy <- function(x, ...) {
    i = 1L
    cf <- x |> 
      seq_along() |>
      lapply(FUN = \(i) {
        x[[i]] |>
          attr(which = 'model', exact = TRUE) |>
          coef()
      })
    
  }
  
}







#' @title S3 Method Dispatches to `'add_dummy'` Class
#' 
#' @param x an object returned from functions [add_dummy_partition()] or [add_dummy()]
#' 
#' @param subset \link[base]{language}
#' 
#' @param ... additional parameters of function [predict.add_dummy_()], e.g., `newdata`
#' 
#' @details
#' Function [subset.add_dummy()], default subset `(p1>.15 & p1<.85)`.
#' See explanation of \eqn{p_1} in function [splitd()].
#' 
#' @returns
#' Function [subset.add_dummy()] returns a [add_dummy()] object.
#' 
#' @keywords internal
#' @export subset.add_dummy
#' @export
subset.add_dummy <- function(x, subset, ...) {
  subset <- substitute(subset)
  v_sub <- all.vars(subset)
  if (!all(v_sub %in% c('p1'))) stop('criterion must be set on `p1`, for now')
  p1 <- vapply(x, FUN = attr, which = 'p1', exact = TRUE, FUN.VALUE = NA_real_)
  x[eval(subset)]
}




#' @title Regression Models with Optimal Dichotomizing Predictors
#' 
#' @description
#' Regression models with optimal dichotomizing predictor(s), used either as boolean or continuous predictor(s).
#' 
#' @param object an `add_dummy_` object, as an element of the \link[stats]{listof} return from functions [add_dummy()] or [add_dummy_partition()]
#' 
#' @param newdata \link[base]{data.frame}, candidate \link[base]{numeric} predictors \eqn{x}'s must have the same \link[base]{name} and \link[base]{dim}ension as the training data. If missing, the training data is used
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' Function [predict.add_dummy()] returns a \link[stats]{listof} regression models.
#' 
#' @keywords internal
#' @name predict_add_dummy
#' @export predict.add_dummy
#' @export
predict.add_dummy <- function(object, ...) {
  ret <- object |> 
    lapply(FUN = predict.add_dummy_, ...)
  names(ret) <- object |>
    labels.add_dummy()
  class(ret) <- 'listof'
  return(ret)
}


#' @rdname predict_add_dummy
#' 
#' @returns
#' Function [predict.add_dummy_()] returns a updated regression model.
#' 
#' @importFrom stats predict update
#' @export predict.add_dummy_
#' @export
predict.add_dummy_ <- function(object, newdata, ...) {
  
  if ('x.' %in% names(newdata)) stop('do not allow existing name `x.` in `newdata`')
  
  newd <- unclass(newdata)$df
  
  newd$x. <- object |>
    predict.node1(newdata = newdata)
  
  object |>
    attr(which = 'model', exact = TRUE) |> 
    update(data = newd)
  
}
