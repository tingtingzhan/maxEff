

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
#' Function [add_num()] treats each additional predictor as a \link[base]{numeric} variable, 
#' and \link[stats]{update}s the starting model with each additional predictor.  
#' 
#' @returns 
#' Function [add_num()] returns an [add_num] object, 
#' which is a \link[stats]{listof} objects with an internal class `'add_num_'`.
#' 
#' @keywords internal
#' @importFrom parallel mclapply detectCores
#' @importFrom stats formula update
#' @export
add_num <- function(
    start.model, 
    x,
    data = eval(start.model$call$data),
    mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), 
    ...
) {
  
  hc <- unclass(data)$hypercolumns
  
  tmp <- .prepare_add_(start.model = start.model, x = x, data = data)
  y <- tmp$y
  data <- tmp$data
  x_ <- tmp$x_
  
  out <- mclapply(x_, mc.cores = mc.cores, FUN = \(x.) {
  #out <- lapply(x_, FUN = \(x.) { 
    # (x. = x_[[1L]])
    data$x. <- eval(x., envir = hc)
    m_ <- update(start.model, formula. = . ~ . + x., data = data)
    cf_ <- m_$coefficients[length(m_$coefficients)]
    attr(x., which = 'effsize') <- if (is.finite(cf_)) unname(cf_) else NA_real_
    attr(x., which = 'model') <- m_ # needed for [predict.*]
    class(x.) <- c('add_num_', class(x.))
    return(x.)
  }) # class is 'list'
  
  # just to beautify!!
  names(out) <- vapply(x_, FUN = deparse1, FUN.VALUE = '')
  
  #class(out) <- c('add_num', 'add_', class(out))
  class(out) <- c('add_num', 'add_', 'listof')
  return(invisible(out))
  
}





#' @title Regression Models with Optimal Dichotomizing Predictors
#' 
#' @description
#' Regression models with optimal dichotomizing predictor(s), used either as boolean or continuous predictor(s).
#' 
#' @param object an [add_num] object
#' 
#' @param ... additional parameters of function `predict.add_num_`, e.g., `newdata`
#' 
#' @returns
#' Function [predict.add_num()] returns a \link[stats]{listof} regression models.
#' 
#' @keywords internal
#' @importFrom stats predict
#' @export predict.add_num
#' @export
predict.add_num <- function(object, ...) {
  ret <- object |> lapply(FUN = predict.add_num_, ...)
  class(ret) <- 'listof'
  return(ret)
}



#' @export
predict.add_num_ <- function(object, newdata, ...) {
  
  if ('x.' %in% names(newdata)) stop('do not allow existing name `x.` in `newdata`')
  hc <- unclass(newdata)$hypercolumns
  newd <- unclass(newdata)$df
  newd$x. <- eval(object, envir = hc)
  
  m_ <- attr(object, which = 'model', exact = TRUE)
  suppressWarnings(update(m_, data = newd))
  
}

