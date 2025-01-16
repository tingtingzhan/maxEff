

#' @title add_num
#' 
#' @description
#' add_num
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
#' 
#' Function [add_num] ..
#' 
#' @returns 
#' Function [add_num] ..
#' 
#' @examples 
#' library(spatstat.grouped)
#' library(spatstat.grouped.data)
#' library(survival) # to help ?spatstat.geom::hyperframe understand ?survival::Surv
#' 
#' s = grouped_ppp(hladr + phenotype ~ OS + gender + age | patient_id/image_id, data = wrobel_lung)
#' sQ = s |>
#'  aggregate_quantile(by = ~ patient_id, probs = seq.int(from = 0, to = 1, by = .1))
#' dim(sQ)
#' 
#' sQ0 = sQ[1:100,] # training set
#' sQ1 = sQ[-(1:100),] # test set
#' 
#' (m2 = coxph(OS ~ gender, data = sQ0) |>
#'  add_num(x = ~ hladr.quantile) |>
#'  sort_by(y = abs(cf)) |>
#'  head(n = 2L))
#' predict(m2, newdata = sQ1)
#' 
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
  
  fom0 <- formula(start.model)
  
  y <- start.model$y
  force(data)
  if (!is.data.frame(data)) stop('unavailable to retrieve `data` ?')
  if (length(y) != nrow(data)) stop('size of `start.model` and `x` do not match')
  
  if (!is.language(x) || is.symbol(x) || x[[1L]] != '~' || length(x) != 2L) stop('`x` must be one-sided formula')
  if (!is.symbol(x. <- x[[2L]])) stop('rhs(x) must be a symbol')
  if (!is.matrix(X <- eval(x., envir = data)) || !is.numeric(X)) stop('predictors must be stored in a `numeric` `matrix`')
  #if (anyNA(X)) # okay!
  x_ <- lapply(colnames(X), FUN = function(i) {
    call(name = '[', x., alist(i =)[[1L]], i)
  })
  
  #out <- mclapply(x_, mc.cores = mc.cores, FUN = function(p) {
  out <- lapply(x_, FUN = function(p) { 
    # (p = x_[[1L]])
    data$x. <- eval(p, envir = data)
    m_ <- update(start.model, formula. = . ~ . + x., data = data)
    cf_ <- m_$coefficients[length(m_$coefficients)]
    attr(p, which = 'cf') <- if (is.finite(cf_)) unname(cf_) else NA_real_
    attr(p, which = 'model') <- m_ # needed for [predict.*]
    class(p) <- c('add_num_', class(p))
    return(p)
  })
  
  # just to beautify!!
  names(out) <- vapply(x_, FUN = deparse1, FUN.VALUE = '')
  
  class(out) <- c('add_num', 'add_', class(out))
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
#' Function [predict.add_num] returns a \link[base]{list} of regression models, \link[survival]{coxph} model for \link[survival]{Surv} response, \link[stats]{glm} for \link[base]{logical} response, and \link[stats]{lm} model for \link[base]{numeric} response.
#' 
#' @examples
#' # see ?`Qindex-package`
#' @importFrom stats predict
#' @export predict.add_num
#' @export
predict.add_num <- function(object, ...) {
  return(lapply(object, FUN = predict.add_num_, ...))
}



#' @export
predict.add_num_ <- function(object, newdata, ...) {
  
  if ('x.' %in% names(newdata)) stop('do not allow existing name `x.` in `newdata`')
  newdata$x. <- eval(object, envir = newdata)
  
  m_ <- attr(object, which = 'model', exact = TRUE)
  suppressWarnings(update(m_, data = newdata))
  
}

