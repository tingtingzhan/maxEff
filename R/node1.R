


#' @title Dichotomize via 1st Node of Recursive Partitioning
#' 
#' @description
#' Dichotomize one or more predictors of
#' a \link[survival]{Surv}, a \link[base]{logical}, or a \link[base]{double} response,
#' using recursive partitioning and regression tree \link[rpart]{rpart}.
#' 
#' @param x a \link[rpart]{rpart} object
#' 
#' @param check_degeneracy \link[base]{logical} scalar, whether to allow the 
#' dichotomized value to be all-`FALSE` or all-`TRUE` (i.e., degenerate) 
#' for any one of the predictors.
#' Default `TRUE` to produce a \link[base]{warning} message for degeneracy.
#' 
#' @param ... additional parameters of \link[rpart]{rpart} and/or \link[rpart]{rpart.control}
#' 
#' @details
#' Function [node1()] dichotomizes one predictor in the following steps, 
#' 
#' \enumerate{
#' 
#' \item {Recursive partitioning and regression tree \link[rpart]{rpart} analysis is 
#' performed for the response \eqn{y} and the predictor \eqn{x}.}
#' 
#' \item {The \link[rpart]{labels.rpart} of the first node of 
#' the \link[rpart]{rpart} tree
#' is considered as the dichotomizing rule of the \link[base]{double} predictor \eqn{x}.
#' The term *dichotomizing rule* indicates the combination of an inequality sign
#' (\link[base]{>}, \link[base]{>=}, \link[base]{<} and \link[base]{<=}) 
#' and a \link[base]{double} cutoff threshold \eqn{a}}
#' 
#' \item {The dichotomizing rule from Step 2 is further processed, such that
#' \itemize{
#' \item {\eqn{<a} is regarded as \eqn{\geq a}}
#' \item {\eqn{\leq a} is regarded as \eqn{>a}}
#' \item {\eqn{> a} and \eqn{\geq a} are regarded as is.}
#' }
#' This step is necessary for a narrative of 
#' *greater than* or *greater than or equal to* 
#' the threshold \eqn{a}.}
#' 
#' \item {A \link[base]{warning} message is produced, 
#' if the dichotomizing rule, applied to a new \link[base]{double} predictor `newx`, creates 
#' an all-`TRUE` or all-`FALSE` result.
#' We do not make the algorithm \link[base]{stop}, 
#' as most regression models in R are capable of handling 
#' an all-`TRUE` or all-`FALSE` predictor,
#' by returning a `NA_real_` regression coefficient estimate.
#' }
#' 
#' }
#' 
#' 
#' @returns 
#' 
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
#' library(rpart)
#' (r = rpart(Price ~ Mileage, data = cu.summary, control = rpart.control(maxdepth = 2L)))
#' (foo = r |> node1())
#' get_cutoff(foo)
#' labels(foo)
#' rnorm(6L, mean = 24.5) |> foo()
#' @keywords internal
#' @export
node1 <- function(x, check_degeneracy = TRUE, ...) {
  
  s <- x$splits
  if (!length(s)) {
    if (x$control$cp > .Machine$double.eps) {
      stop('re-run rpart(., cp = .Machine$double.eps) to force a split')
    } else stop('really?')
  }
  
  labs <- labels(x) # ?rpart:::labels.rpart
  nd1 <- str2lang(labs[2L]) # first node!!!
  
  if (nd1[[1L]] == '<=') {
    nd1[[1L]] <- quote(`>`)
  } else if (nd1[[1L]] == '<') {
    nd1[[1L]] <- quote(`>=`)
  } # else if (nd1[[1L]] is '>' or '>=')  do nothing
  
  nd1[[2L]] <- quote(newx)
  
  nd1[[3L]] <- s[1L, 4L] # threshold, in case `labels` are truncated due to `digits`
  
  fn_ <- alist(newx = )
  fn_[[2L]] <- if (check_degeneracy) call(
    name = '{',
    call(name = '<-', quote(ret), call(name = '(', nd1)),
    quote(if (all(ret, na.rm = TRUE) || !any(ret, na.rm = TRUE)) warning('Dichotomized value is all-0 or all-1')),
    quote(return(ret))
  ) else nd1
  fn <- as.function.default(fn_)
  #attr(fn, which = 'cutoff') <- nd1[[3L]] # do note deprecate, for now
  #attr(fn, which = 'text') <- deparse1(nd1[c(1L, 3L)])
  class(fn) <- c('node1', class(fn))
  return(fn)
}








#' @export
print.node1 <- function(x, ...) {
  cat('\nDichotomizing Rule based on Recursive Partitioning:\n\n')
  x0 <- unclass(x)
  attributes(x0) <- NULL
  print(x0)
  
  atr <- attributes(x)
  atr$class <- NULL # don't want to print
  if (!length(atr)) return(invisible())
  cat('\nDevelopers, use\n')
  if (length(atr$p1)) cat('\nattr(.,\'p1\') to see the mean of dichotomized value in training, or test (if available), data\n')
  if (length(atr$x)) cat('\nattr(.,\'x\') to see the name of continous variable that is dichotomized\n')
  if (length(atr$effsize)) cat('\nattr(.,\'effsize\') to see the regression coefficient, i.e., effect size, of the dichotomized variable in training, or test (if available), data\n')
  if (length(atr$model)) cat('\nattr(.,\'model\') to see the regression model in training, or test (if available), data\n\n')
  
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


