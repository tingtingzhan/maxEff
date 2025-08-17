

#' @title Additional Predictor as \link[base]{numeric}
#' 
#' @description
#' Additional predictor as \link[base]{numeric}.
#' 
#' @param start.model a regression model (e.g., \link[stats]{lm}, \link[stats]{glm}, or \link[survival]{coxph}, etc.)
#' 
#' @param x one-sided \link[stats]{formula} to specify 
#' the \link[base]{numeric} predictors \eqn{x}'s as the columns of one \link[base]{matrix} column in `data`
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details 
#' Function [add_numeric()] treats each additional predictor as a \link[base]{numeric} variable, 
#' and \link[stats]{update}s the starting model with each additional predictor.  
#' 
#' @returns 
#' Function [add_numeric()] returns an [add_numeric] object, 
#' which is a \link[stats]{listof} objects with an internal class `'add_numeric_'`.
#' 
#' @keywords internal
#' @importFrom parallel mclapply
#' @importFrom stats formula update
#' @export
add_numeric <- function(
    start.model, 
    x,
    data = eval(start.model$call$data),
    mc.cores = getOption('mc.cores'), 
    ...
) {
  
  tmp <- .prepare_add_(start.model = start.model, x = x, data = data)
  y <- tmp$y
  data_ <- tmp$data # 'data.frame'
  x_ <- tmp$x_
  xval <- tmp$xval

  out <- x_ |>
    seq_along() |>  
    mclapply(mc.cores = mc.cores, FUN = \(i) {
      x. <- x_[[i]]
      data_$x. <- xval[[i]]
      m_ <- update(start.model, formula. = . ~ . + x., data = data_)
      cf <- m_$coefficients
      cf_ <- cf[length(cf)]
      attr(x., which = 'effsize') <- if (is.finite(cf_)) unname(cf_) else NA_real_
      attr(x., which = 'model') <- m_ # needed for [predict.*]
      class(x.) <- c('add_numeric_', class(x.))
      return(x.)
    }) # 'list'
  
  class(out) <- c('add_numeric', 'add_', 'listof', class(out))
  return(invisible(out))
  
}



#' @title [print.add_numeric]
#' 
#' @param x a [add_numeric] object
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @export print.add_numeric
#' @export
print.add_numeric <- function(x, ...) {
  x |>
    vapply(FUN = deparse1, FUN.VALUE = '') |>
    cat(sep = '\n')
}





# @title [print.add_numeric_]
# @param x a `add_numeric_` object
# @param ... ..
# @note
# don't do this.  tzh want ?base::print.default to inspect.
# @keywords internal
# @export print.add_numeric_
# @export
#print.add_numeric_ <- function(x, ...) {
#  # 'add_numeric_' object is `language` with a bunch of attributes
#  x |>
#    deparse1() |>
#    cat('\n')
#}




#' @title Regression Models with Optimal Dichotomizing Predictors
#' 
#' @description
#' Regression models with optimal dichotomizing predictor(s), used either as boolean or continuous predictor(s).
#' 
#' @param object an [add_numeric] object
#' 
#' @param ... additional parameters of function `predict.add_numeric_`, e.g., `newdata`
#' 
#' @returns
#' Function [predict.add_numeric()] returns a \link[stats]{listof} regression models.
#' 
#' @keywords internal
#' @importFrom stats predict
#' @export predict.add_numeric
#' @export
predict.add_numeric <- function(object, ...) {
  ret <- object |> 
    lapply(FUN = predict.add_numeric_, ...)
  class(ret) <- 'listof'
  return(ret)
}



#' @export
predict.add_numeric_ <- function(object, newdata, ...) {
  
  if ('x.' %in% names(newdata)) stop('do not allow existing name `x.` in `newdata`')
  newd <- unclass(newdata)$df
  newd$x. <- with(data = newdata, ee = object) # ?spatstat.geom::with.hyperframe
  
  m_ <- attr(object, which = 'model', exact = TRUE)
  suppressWarnings(update(m_, data = newd))
  
}

