

if (FALSE) {
  # follow examples in ?split_dummy
  set.seed(2364); aa = splitDichotom(OS ~ hladr.quantile, data = sQ0, n = 20L) |>
    subset.split_dummy(subset = p1 > .15 & p1 < .85) |>
    sort_by.split_dummy(y = abs(cf)) |>
    head.split_dummy(n = 2L)
  print.split_dummy(aa)
  #hladr.quantile[, "20%"]>=0.05902
  #hladr.quantile[, "0%"]>=0.0435
}






# @importFrom survival coxph
# @importFrom stats lm glm binomial
splitd <- function(y, x, id, ...) {
  
  .Deprecated(new = '.splitd')
  
  # `id`: training set
  rule <- rpartD(y = y[id], x = x[id], check_degeneracy = TRUE)
  
  # `!id`: test set
  y_ <- y[!id]
  dx_ <- tryCatch(rule(x[!id]), warning = identity)
  if (inherits(dx_, what = 'warning')) return(invisible()) # exception
  
  suppressWarnings(m_ <- if (inherits(y, what = 'Surv')) {
    coxph(formula = y_ ~ dx_)
  } else if (is.logical(y) || all(y %in% c(0, 1))) {
    glm(formula = y_ ~ dx_, family = binomial(link = 'logit'))
  } else lm(formula = y_ ~ dx_))
  
  cf_ <- m_$coefficients[length(m_$coefficients)]
  
  attr(rule, which = 'p1') <- mean.default(dx_, na.rm = TRUE)
  attr(rule, which = 'cf') <- if (is.finite(cf_)) unname(cf_) else NA_real_
  class(rule) <- c('splitd', class(rule))
  return(rule)
}







splitd_ <- function(y, ids = rSplit(x = y, ...), ...) {
  .Deprecated(new = '.splitd_')
  ret_ <- lapply(ids, FUN = function(id) splitd(y = y, id = id, ...))
  ret <- ret_[lengths(ret_) > 0L]
  class(ret) <- c('splitd.list', class(ret))
  return(ret)
}



splitDichotom <- function(
    formula, 
    data, 
    n, 
    mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), 
    ...
) {
  
  .Deprecated(new = 'pipeline with [split_dummy]')
  
  y <- eval(formula[[2L]], envir = data)
  ids <- rSplit(n = n, x = y, ...) # using same split for all predictors
  
  if (!is.symbol(x. <- formula[[3L]])) stop('rhs(formular) must be a symbol')
  if (!is.matrix(X <- eval(x., envir = data)) || !is.numeric(X)) stop('predictors must be stored in a `numeric` `matrix`')
  #if (anyNA(X)) # okay!
  x_ <- lapply(colnames(X), FUN = function(i) {
    call(name = '[', x., alist(i =)[[1L]], i)
  })
  
  out_ <- mclapply(seq_along(x_), mc.cores = mc.cores, FUN = function(p) { # (p = 1L)
    tmp <- splitd_(y = y, x = X[,p], ids = ids)
    quantile.splitd.list(tmp, probs = .5)[[1L]]
  })
  
  # must! for [predict.splitd]
  fom_ <- lapply(x_, FUN = function(i) eval(call(name = '~', formula[[2L]], i)))
  out <- mapply(FUN = `attr<-`, x = out_, value = fom_, MoreArgs = list(which = 'formula'), SIMPLIFY = FALSE)
  
  # just to beautify
  arg. <- vapply(x_, FUN = deparse1, FUN.VALUE = '')
  txt. <- vapply(out, FUN = attr, which = 'text', exact = TRUE, FUN.VALUE = '')
  names(out) <- paste0(arg., txt.)
  
  # must! for [predict.splitDichotom]
  attr(out, which = 'data') <- data
  
  class(out) <- c('splitDichotom', class(out))
  return(invisible(out))
  
}
