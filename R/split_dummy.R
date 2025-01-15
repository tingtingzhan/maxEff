

#' @title Dichotomizing Predictors via Repeated Sample Splits
#' 
#' @description
#' Dichotomizing predictors using repeated sample splits.
#' 
#' @param null.model a regression model (e.g., \link[stats]{lm}, \link[stats]{glm}, or \link[survival]{coxph}, etc.)
#' 
#' @param x one-sided \link[stats]{formula} to specify 
#' the \link[base]{numeric} predictors \eqn{x}'s as the columns of one \link[base]{matrix} column in `data`
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @param n,... additional parameters for function [rSplit]
#' 
#' @details 
#' 
#' Function [split_dummy] dichotomizes predictors via repeated sample splits. Specifically, 
#' 
#' \enumerate{
#' \item Generate multiple, i.e., repeated, training-test sample splits (via [rSplit])
#' \item For each candidate predictor \eqn{x_i}, find the ***median-split-dichotomized regression model*** based on the repeated sample splits, see functions [.splitd_] and [quantile.splitd.list];
#' }
#' 
#' @returns 
#' Function [split_dummy] returns an object of \link[base]{class} `'split_dummy'`, which is a \link[base]{list} of dichotomizing \link[base]{function}s, 
#' with the input `formula` and `data` as additional \link[base]{attributes}.
#' 
#' @examples 
#' library(spatstat.grouped)
#' library(spatstat.grouped.data)
#' library(survival) # to help ?spatstat.geom::hyperframe understand ?survival::Surv
#' s = grouped_ppp(hladr + phenotype ~ OS + gender + age | patient_id/image_id, data = wrobel_lung)
#' sQ = s |>
#'  aggregate_quantile(by = ~ patient_id, probs = seq.int(from = 0, to = 1, by = .1))
#' dim(sQ)
#' 
#' sQ0 = sQ[1:100,] # training set
#' sQ1 = sQ[-(1:100),] # test set
#' 
#' set.seed(2364); m1 = coxph(OS ~ 1, data = sQ0) |>
#'  split_dummy(x = ~ hladr.quantile, data = sQ0, n = 20L) |> subset(subset = p1 > .15 & p1 < .85) |>
#'  sort_by(y = abs(cf)) |>
#'  head(n = 2L)
#' m1
#' predict(m1, newdata = sQ1)
#' 
#' 
#' @importFrom parallel mclapply detectCores
#' @importFrom stats formula
#' @export
split_dummy <- function(
    null.model, 
    x,
    data,
    n, 
    mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), 
    ...
) {
  
  fom0 <- formula(null.model)
  
  y <- null.model$y
  if (length(y) != nrow(data)) stop('size of `null.model` and `x` do not match')
  
  ids <- rSplit(n = n, x = y, ...) # using same split for all predictors
  
  if (!is.language(x) || is.symbol(x) || x[[1L]] != '~' || length(x) != 2L) stop('`x` must be one-sided formula')
  if (!is.symbol(x. <- x[[2L]])) stop('rhs(x) must be a symbol')
  if (!is.matrix(X <- eval(x., envir = data)) || !is.numeric(X)) stop('predictors must be stored in a `numeric` `matrix`')
  #if (anyNA(X)) # okay!
  x_ <- lapply(colnames(X), FUN = function(i) {
    call(name = '[', x., alist(i =)[[1L]], i)
  })
  
  out_ <- mclapply(seq_along(x_), mc.cores = mc.cores, FUN = function(p) { # (p = 1L)
  #out_ <- lapply(seq_along(x_), FUN = function(p) { # (p = 1L)
    tmp <- .splitd_(null.model = null.model, x = X[,p], ids = ids)
    quantile.splitd.list(tmp, probs = .5)[[1L]]
  })
  
  # must! for [predict.splitd]
  fom_ <- lapply(x_, FUN = function(i) eval(call(name = '~', fom0[[2L]], i)))
  out <- mapply(FUN = `attr<-`, x = out_, value = fom_, MoreArgs = list(which = 'formula'), SIMPLIFY = FALSE)
  
  # just to beautify
  arg. <- vapply(x_, FUN = deparse1, FUN.VALUE = '')
  txt. <- vapply(out, FUN = attr, which = 'text', exact = TRUE, FUN.VALUE = '')
  names(out) <- paste0(arg., txt.)
  
  # must! for [predict.split_dummy]
  attr(out, which = 'data') <- data
  
  class(out) <- c('split_dummy', class(out))
  return(invisible(out))
  
}









#' @export
`[.split_dummy` <- function(x, i) {
  ret <- unclass(x)[i]
  attr(ret, which = 'data') <- attr(x, which = 'data', exact = TRUE)
  class(ret) <- class(x)
  return(ret)
}



#' @title subset.split_dummy
#' 
#' @param x a [split_dummy] object
#' 
#' @param subset \link[base]{language}
#' 
#' @details
#' Default `(p1>.15 & p1<.85)`.
#' See explanation of \eqn{p_1} in function [.splitd].
#' 
#' @returns
#' Function [subset.split_dummy] returns a [split_dummy] object.
#' 
#' @keywords internal
#' @export subset.split_dummy
#' @export
subset.split_dummy <- function(x, subset, ...) {
  subset <- substitute(subset)
  v_sub <- all.vars(subset)
  if (!all(v_sub %in% c('p1'))) stop('criterion must be set on `p1`, for now')
  p1 <- vapply(x, FUN = attr, which = 'p1', exact = TRUE, FUN.VALUE = NA_real_)
  x[eval(subset)]
}


#' @title [sort_by.split_dummy]
#' 
#' @description
#' ..
#' 
#' @param x [split_dummy] object
#' 
#' @param y \link[base]{language}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Sort the elements of a [split_dummy] object by a certain criterion `y`.
#' 
#' We suggest using `y = abc(cf)`, indicating a `decreasing` order of the \link[base]{abs}olute values of the regression coefficient estimate of the median-split-dichotomized regression models.  
#'
#' @returns 
#' Function [sort_by.split_dummy] returns a [split_dummy] object.
#' 
#' @keywords internal
#' @export sort_by.split_dummy
#' @export
sort_by.split_dummy <- function(x, y, ...) {
  cf <- vapply(x, FUN = attr, which = 'cf', exact = TRUE, FUN.VALUE = NA_real_)
  y_ <- eval(substitute(y))
  o <- order(y_, ...) # ?base::order
  x[o]
}



#' @title head.split_dummy
#' 
#' @param x a [split_dummy] object
#' 
#' @param n positive \link[base]{integer} scalar
#' 
#' @returns
#' Function [head.split_dummy] returns a [split_dummy] object.
# On the top of this rank are the ***optimal dichotomizing predictors***.
#' @keywords internal
#' @importFrom utils head
#' @export head.split_dummy
#' @export
head.split_dummy <- function(x, n, ...) {
  x[seq_len(n)] # not doing any fancy checks
}



#' @title print.split_dummy
#' 
#' @param x a [split_dummy] object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' ..
#' 
#' @returns
#' Function [print.split_dummy] does not have a returned value
#' 
#' @keywords internal
#' @export print.split_dummy
#' @export
print.split_dummy <- function(x, ...) {
  cat(names(x), sep = '\n')
  return(invisible())
}





#' @title Regression Models with Optimal Dichotomizing Predictors
#' 
#' @description
#' Regression models with optimal dichotomizing predictor(s), used either as boolean or continuous predictor(s).
#' 
#' @param object an [split_dummy] object
#' 
#' @param newdata (optional) test \link[base]{data.frame}, candidate \link[base]{numeric} predictors \eqn{x}'s must have the same \link[base]{name} and \link[base]{dim}ension as the training data. If missing, the training data is used
#' 
#' @param ... additional parameters of function [predict.splitd]
#' 
#' @returns
#' Function [predict.split_dummy] returns a \link[base]{list} of regression models, \link[survival]{coxph} model for \link[survival]{Surv} response, \link[stats]{glm} for \link[base]{logical} response, and \link[stats]{lm} model for \link[base]{numeric} response.
#' 
#' @examples
#' # see ?`Qindex-package`
#' @importFrom stats predict
#' @export predict.split_dummy
#' @export
predict.split_dummy <- function(
    object, 
    newdata = attr(object, which = 'data', exact = TRUE),
    ...
) {
  return(lapply(object, FUN = predict.splitd, newdata = newdata, ...))
}


