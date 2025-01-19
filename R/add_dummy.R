


# ## Dichotomizing Predictor(s) via Dichotomizing Split Sample [splitDichotom]
#
# Pipeline `splitDichotom |> subset |> sort_by |> head` identifies 
# the optimal dichotomizing predictors using repeated sample splits on the *training set*.
# 
# Function [predict.splitDichotom] .. testing set
# 




#' @title Dichotomizing Predictors via Repeated Sample Splits
#' 
#' @description
#' Dichotomizing predictors using repeated sample splits.
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
#' @param n,... additional parameters of function [rSplit] for function [add_dummy_rSplit].
#' For function `add_dummy`, these parameters are not in use
#' 
#' @details 
#' 
#' Function [add_dummy_rSplit] dichotomizes predictors via repeated sample splits. Specifically, 
#' 
#' \enumerate{
#' \item Generate multiple, i.e., repeated, training-test sample splits (via [rSplit])
#' \item For each candidate predictor \eqn{x_i}, find the ***median-split-dichotomized regression model*** based on the repeated sample splits, see functions [splitd_] and [quantile.splitd.list];
#' }
#' 
#' @returns 
#' Function [add_dummy_rSplit] returns an object of \link[base]{class} `'add_dummy_rSplit'`, which is a \link[base]{list} of dichotomizing \link[base]{function}s.
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
#' set.seed(234); m1 = coxph(OS ~ gender, data = sQ0) |>
#'  add_dummy_rSplit(x = ~ hladr.quantile, n = 20L) |> subset(subset = p1 > .15 & p1 < .85) |>
#'  sort_by(y = abs(cf)) |>
#'  head(n = 2L)
#' m1
#' #hladr.quantile[, "0.1"]>=0.08082
#' #hladr.quantile[, "0"]>=0.0259
#' predict(m1, newdata = sQ1)
#' 
#' m2 = coxph(OS ~ gender, data = sQ0) |>
#'  add_dummy(x = ~ hladr.quantile) |> subset(subset = p1 > .15 & p1 < .85) |> 
#'  sort_by(y = abs(cf)) |>
#'  head(n = 2L)
#' m2
#' 
#' @name add_dummy
#' @importFrom parallel mclapply detectCores
#' @importFrom stats formula
#' @export
add_dummy_rSplit <- function(
    start.model, 
    x,
    data = eval(start.model$call$data),
    n, 
    mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), 
    ...
) {
  
  tmp <- .prepare_add_(start.model = start.model, x = x, data = data)
  y <- tmp$y
  data <- tmp$data
  x_ <- tmp$x_
  
  ids <- rSplit(n = n, x = y, ...) # using same split for all predictors
  
  #out <- mclapply(x_, mc.cores = mc.cores, FUN = function(p) { 
  out <- lapply(x_, FUN = function(p) { # to debug
    # (p = x_[[1L]])
    tmp <- splitd_(start.model = start.model, x_ = p, data = data, ids = ids)
    quantile.splitd.list(tmp, probs = .5)[[1L]]
  })

  # just to beautify
  arg. <- vapply(x_, FUN = deparse1, FUN.VALUE = '')
  txt. <- vapply(out, FUN = attr, which = 'text', exact = TRUE, FUN.VALUE = '')
  names(out) <- paste0(arg., txt.)
  
  class(out) <- c('add_dummy_rSplit', 'add_dummy', 'add_', class(out))
  return(invisible(out))
  
}





# think carefully!
# how is this [add_dummy] different from \link[maxEff]{add_dummy_rSplit}
# this [add_dummy] should be moved to \pkg{maxEff} and be named [add_dummy] !!!!
# it's actually easier to copy code from ?maxEff::add_num

#' @rdname add_dummy
# @param rule (optional) a \link[base]{list}, returned value from function \link[maxEff]{rpartD_}
#' @details
#' 
#' First, obtain the dichotomizing rules \eqn{\mathbf{\mathcal{D}}} of predictors \eqn{x_1,\cdots,x_k} based on 
#' response \eqn{y} (via \link[maxEff]{rpartD}).
#' 
#' Then, \link[stats]{update} previous multivariable regression `start.model` 
#' with dichotomized predictors \eqn{\left(\tilde{x}_1,\cdots,\tilde{x}_k\right) = \mathcal{D}\left(x_1,\cdots,x_k\right)}. 
#' 
#' @returns
#' Function [add_dummy] returns an object of class `'add_dummy'`.
# \item{`attr(,'rule')`}{returned value from function \link[maxEff]{rpartD_}, 
# dichotomizing rules for the \eqn{k} predictors}
#' 
#' @importFrom stats terms update model.frame.default na.pass
#' @importFrom utils tail
#' @importFrom maxEff rpartD_
#' @export
add_dummy <- function(
    start.model, 
    x, 
    data = eval(start.model$call$data),
    #rule,
    mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), 
    ...
) {
  
  tmp <- .prepare_add_(start.model = start.model, x = x, data = data)
  y <- tmp$y
  data <- tmp$data
  x_ <- tmp$x_
  
  out <- mclapply(x_, mc.cores = mc.cores, FUN = function(p) {
  #out <- lapply(x_, FUN = function(p) { 
    # (p = x_[[1L]])
    xval <- eval(p, envir = data)
    rule <- rpartD(y = y, x = xval)
    data$x. <- rule(xval)
    m_ <- update(start.model, formula. = . ~ . + x., data = data)
    cf_ <- m_$coefficients[length(m_$coefficients)]
    attr(rule, which = 'p1') <- mean.default(data$x., na.rm = TRUE)
    attr(rule, which = 'x') <- p
    attr(rule, which = 'cf') <- if (is.finite(cf_)) unname(cf_) else NA_real_
    attr(rule, which = 'model') <- m_ # needed for [predict.*]
    #class(rule) <- c('add_dummy', class(rule)) # not sure yet
    return(rule)
  })
  
  # just to beautify!!
  names(out) <- vapply(x_, FUN = deparse1, FUN.VALUE = '')
  
  class(out) <- c('add_dummy', 'add_', class(out))
  return(invisible(out))

}










.prepare_add_ <- function(start.model, x, data, envir = parent.frame(), ...) {
  
  fom0 <- formula(start.model)
  
  y <- start.model$y
  if (!length(y)) stop('`start.model` response?')
  
  force(data)
  if (!is.data.frame(data)) stop('unavailable to retrieve `data` ?')
  
  # reduce `data` to match `start.model`
  if (length(y) != .row_names_info(data, type = 2L)) {
    start_na <- start.model$na.action
    if (!length(start_na)) stop('`start.model` `na.action` not available?')
    data <- data[-start_na, , drop = FALSE]
  }

  if (!is.language(x) || is.symbol(x) || x[[1L]] != '~' || length(x) != 2L) stop('`x` must be one-sided formula')
  if (!is.symbol(x. <- x[[2L]])) stop('rhs(x) must be a symbol')
  if (!is.matrix(X <- eval(x., envir = data)) || !is.numeric(X)) stop('predictors must be stored in a `numeric` `matrix`')
  #if (anyNA(X)) # okay!
  x_ <- lapply(colnames(X), FUN = function(i) {
    call(name = '[', x., alist(i =)[[1L]], i)
  })
  
  return(list(
    y = y,
    data = data,
    x_ = x_
  ))
  
}






#' @title subset.add_dummy
#' 
#' @param x a [add_dummy_rSplit] and/or [add_dummy] object
#' 
#' @param subset \link[base]{language}
#' 
#' @details
#' Default `(p1>.15 & p1<.85)`.
#' See explanation of \eqn{p_1} in function [splitd].
#' 
#' @returns
#' Function [subset.add_dummy] returns a [add_dummy] object.
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
#' @param object an [add_dummy_rSplit] object
#' 
#' @param ... additional parameters of function [predict.splitd], e.g., `newdata`
#' 
#' @returns
#' Function [predict.add_dummy_rSplit] returns a \link[base]{list} of regression models.
#' 
#' @examples
#' # see ?add_dummy_rSplit
#' @importFrom stats predict
#' @export predict.add_dummy_rSplit
#' @export
predict.add_dummy_rSplit <- function(object, ...) {
  return(lapply(object, FUN = predict.splitd, ...))
}


#' @export
predict.add_dummy <- function(object, ...) {
  stop('will do in future; should be easy')
}
