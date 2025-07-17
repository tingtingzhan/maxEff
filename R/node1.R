


#' @title Dichotomize via 1st Node of Recursive Partitioning
#' 
#' @param x an \link[rpart]{rpart.object}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [node1()] returns an object of class `'node1'`, 
#' which is a \link[base]{function}
#' with one parameter `newx` taking a \link[base]{double} \link[base]{vector}.
#' 
#' @note
#' In future \link[base]{integer} and \link[base]{factor} predictors will be supported.
#' 
#' Function \link[rpart]{rpart} is quite slow.
#' 
#' @examples
#' # See intro vignette, section Appendix, subsection node1()
#' @keywords internal
#' @export
node1 <- function(
    x, 
    ...
) {
  
  s <- x$splits
  if (!length(s)) {
    if (x$control$cp > .Machine$double.eps) {
      stop('re-run rpart(., cp = .Machine$double.eps) to force a split')
    } else stop('really?')
  }
  
  labs <- labels(x) # ?rpart:::labels.rpart
  nd1 <- labs[2L] |> # first node!!!
    str2lang() 
  
  if (nd1[[1L]] == '<=') {
    nd1[[1L]] <- quote(`>`)
  } else if (nd1[[1L]] == '<') {
    nd1[[1L]] <- quote(`>=`)
  } # else if (nd1[[1L]] is '>' or '>=')  do nothing
  
  nd1[[2L]] <- quote(newx)
  
  nd1[[3L]] <- s[1L, 4L] # threshold, in case `labels` are truncated due to `digits`
  
  fn_ <- alist(newx = )
  fn_[[2L]] <- call(
    name = '{',
    call(name = '<-', quote(ret), call(name = '(', nd1)),
    quote(if (all(ret, na.rm = TRUE) || !any(ret, na.rm = TRUE)) warning('Dichotomized value is all-0 or all-1')),
    quote(return(ret))
  )
  fn <- as.function.default(fn_)
  
  attr(fn, which = 'x') <- rownames(s)[1L] |> 
    as.symbol()
  
  class(fn) <- c('node1', class(fn))
  return(fn)
}



#' @title Predict by [node1()]
#' 
#' @param object a [node1] object
#' 
#' @param newdata a \link[base]{data.frame} or \link[spatstat.geom]{hyperframe}
#' 
#' @param ... place holder for `S3` generic
#' 
#' @keywords internal
#' @importFrom stats predict
#' @export predict.node1
#' @export
predict.node1 <- function(object, newdata, ...) {
  
  if (inherits(newdata, what = 'data.frame')) {
    cl <- call(name = 'object', attr(object, which = 'x', exact = TRUE))
    return(with.default(data = newdata, expr = eval(cl)))
  }
  
  if (inherits(newdata, what = 'hyperframe')) {
    stop('be careful with spatstat.geom::with.hyperframe')
  }
  
}



#' @export
print.node1 <- function(x, ...) {
  
  attr(x, which = 'x', exact = TRUE) |>
    deparse1() |>
    sprintf(fmt = 'Dichotomizing Rule (%s) via Recursive Partitioning:\n\n') |>
    cat()
  
  x0 <- unclass(x)
  attributes(x0) <- NULL
  
  # environment(x0) <- globalenv() 
  # suppresses <environment: > line, but does write to .GlobalEnv
  # do NOT do this!!
  
  print(x0)
  
  # all below: TO BE REMOVED!!!
  #atr <- attributes(x)
  #atr$class <- NULL # don't want to print
  #if (!length(atr)) return(invisible())
  
  #cat('\nDevelopers, use\n')
  #if (length(atr$p1)) cat('\nattr(.,\'p1\') to see the mean of dichotomized value in training, or test (if available), data\n')
  #if (length(atr$effsize)) cat('\nattr(.,\'effsize\') to see the regression coefficient, i.e., effect size, of the dichotomized variable in training, or test (if available), data\n')
  #if (length(atr$model)) cat('\nattr(.,\'model\') to see the regression model in training, or test (if available), data\n\n')
  
}



#' @title Get Cutoff Value from a Dichotomizing Rule [node1()]
#' 
#' @description
#' To get the cutoff value from a Dichotomizing Rule [node1()].
#' 
#' @param x see Usage
#' 
#' @keywords internal
#' @name get_cutoff
#' @export
get_cutoff <- function(x) UseMethod(generic = 'get_cutoff')


#' @rdname get_cutoff
#' 
#' @returns
#' Function [get_cutoff.node1()] returns a \link[base]{numeric} scalar.
#' 
#' @export get_cutoff.node1
#' @export
get_cutoff.node1 <- function(x) {
  body(x)[[2L]][[3L]][[2L]][[3L]]
}



#' @title Find \link[base]{labels} from [node1] 
#' 
#' @param object a [node1] object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [labels.node1()] returns a \link[base]{character} scalar.
#' 
#' @keywords internal
#' @export labels.node1
#' @export
labels.node1 <- function(object, ...) {
  (body(object)[[2L]][[3L]][[2L]][c(1L,3L)]) |>
    deparse1()
}


