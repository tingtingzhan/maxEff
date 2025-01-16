
#' @title Dichotomizing Predictors via Repeated Sample Splits
#' 
#' @description
#' Dichotomizing predictors using repeated sample splits.
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
#' @param n,... additional parameters for function [rSplit]
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
#' set.seed(2364); m1 = coxph(OS ~ gender, data = sQ0) |>
#'  add_dummy_rSplit(x = ~ hladr.quantile, n = 20L) |> subset(subset = p1 > .15 & p1 < .85) |>
#'  sort_by(y = abs(cf)) |>
#'  head(n = 2L)
#' m1
#' #hladr.quantile[, "30%"]>=0.15409
#' #hladr.quantile[, "40%"]>=0.21358
#' predict(m1, newdata = sQ1)
#' 
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
  
  fom0 <- formula(start.model)
  
  y <- start.model$y
  force(data)
  if (!is.data.frame(data)) stop('unavailable to retrieve `data` ?')
  if (length(y) != nrow(data)) stop('size of `start.model` and `x` do not match')
  
  ids <- rSplit(n = n, x = y, ...) # using same split for all predictors
  
  if (!is.language(x) || is.symbol(x) || x[[1L]] != '~' || length(x) != 2L) stop('`x` must be one-sided formula')
  if (!is.symbol(x. <- x[[2L]])) stop('rhs(x) must be a symbol')
  if (!is.matrix(X <- eval(x., envir = data)) || !is.numeric(X)) stop('predictors must be stored in a `numeric` `matrix`')
  #if (anyNA(X)) # okay!
  x_ <- lapply(colnames(X), FUN = function(i) {
    call(name = '[', x., alist(i =)[[1L]], i)
  })
  
  out <- mclapply(x_, mc.cores = mc.cores, FUN = function(p) { 
  #out <- lapply(x_, FUN = function(p) { # to debug
    # (p = x_[[1L]])
    tmp <- splitd_(start.model = start.model, x_ = p, data = data, ids = ids)
    quantile.splitd.list(tmp, probs = .5)[[1L]]
  })

  # just to beautify
  arg. <- vapply(x_, FUN = deparse1, FUN.VALUE = '')
  txt. <- vapply(out, FUN = attr, which = 'text', exact = TRUE, FUN.VALUE = '')
  names(out) <- paste0(arg., txt.)
  
  class(out) <- c('add_dummy_rSplit', 'add_', class(out))
  return(invisible(out))
  
}









#' @title subset.add_dummy_rSplit
#' 
#' @param x a [add_dummy_rSplit] object
#' 
#' @param subset \link[base]{language}
#' 
#' @details
#' Default `(p1>.15 & p1<.85)`.
#' See explanation of \eqn{p_1} in function [splitd].
#' 
#' @returns
#' Function [subset.add_dummy_rSplit] returns a [add_dummy_rSplit] object.
#' 
#' @keywords internal
#' @export subset.add_dummy_rSplit
#' @export
subset.add_dummy_rSplit <- function(x, subset, ...) {
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


