


#' @title Dichotomize via Recursive Partitioning
#' 
#' @description
#' Dichotomize one or more predictors of
#' a \link[survival]{Surv}, a \link[base]{logical}, or a \link[base]{double} response,
#' using recursive partitioning and regression tree \link[rpart]{rpart}.
#' 
#' @param y a \link[survival]{Surv} object, 
#' a \link[base]{logical} \link[base]{vector}, 
#' or a \link[base]{double} \link[base]{vector}, the response \eqn{y}
#' 
#' @param x \link[base]{numeric} \link[base]{vector}, one predictor \eqn{x}
#' 
#' @param check_degeneracy \link[base]{logical} scalar, whether to allow the 
#' dichotomized value to be all-`FALSE` or all-`TRUE` (i.e., degenerate) 
#' for any one of the predictors.
#' Default `TRUE` to produce a \link[base]{warning} message for degeneracy.
#' 
#' @param cp \link[base]{double} scalar, complexity parameter, see \link[rpart]{rpart.control}.
#' Default `.Machine$double.eps`, so that a split is enforced 
#' no matter how small improvement in overall \eqn{R^2} is
#' 
#' @param ... additional parameters of \link[rpart]{rpart} and/or \link[rpart]{rpart.control}
#' 
#' @details
#' Function [rpartD] dichotomizes one predictor in the following steps, 
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
#' Function [rpartD] returns a \link[base]{function}, 
#' with one parameter `newx` taking a \link[base]{double} \link[base]{vector}.
#' The returned value of `rpartD(y,x)(newx)` is a 
#' \link[base]{logical} \link[base]{vector}.
# with \link[base]{attributes}
# \describe{
# \item{`attr(,'cutoff')`}{\link[base]{double} scalar, the cutoff value for `newx`}
# }
#' 
#' @note
#' In future \link[base]{integer} and \link[base]{factor} predictors will be supported.
#' 
#' @examples
#' data(cu.summary, package = 'rpart') # see more details from ?rpart::cu.summary
#' with(cu.summary, rpartD(y = Price, x = Mileage, check_degeneracy = FALSE))
#' (foo = with(cu.summary, rpartD(y = Price, x = Mileage)))
#' foo(rnorm(10, mean = 24.5))
#' @keywords internal
#' @importFrom rpart rpart
#' @export
rpartD <- function(
    y, x, 
    check_degeneracy = TRUE,
    cp = .Machine$double.eps, # to force a split even if the overall lack of fit is not decreased
    ...
) {
  rp <- rpart(
    formula = y ~ x, 
    cp = cp, 
    maxdepth = 2L, # only the first node is needed
    ...)
  if (!length(rp$splits)) stop('we must force a split')
  
  labs <- labels(rp) # ?rpart:::labels.rpart
  node1 <- str2lang(labs[2L]) # first node!!!
  
  if (node1[[1L]] == '<=') {
    node1[[1L]] <- quote(`>`)
  } else if (node1[[1L]] == '<') {
    node1[[1L]] <- quote(`>=`)
  } # else if (node1[[1L]] is '>' or '>=')  do nothing
  
  #if (!identical(node1[[2L]], quote(x))) stop('rpart package updated?')
  node1[[2L]] <- quote(newx)
  
  node1[[3L]] <- rp$splits[1L, 4L] # threshold, in case `labels` are truncated due to `digits`
  
  fn_ <- alist(newx = )
  fn_[[2L]] <- if (check_degeneracy) call(
    name = '{',
    call('<-', quote(ret), call('(', node1)),
    quote(if (all(ret, na.rm = TRUE) || !any(ret, na.rm = TRUE)) warning('Dichotomized value is all-0 or all-1')),
    quote(return(ret))
  ) else node1
  fn <- as.function.default(fn_)
  attr(fn, which = 'cutoff') <- node1[[3L]] # do note deprecate, for now
  attr(fn, which = 'text') <- deparse1(node1[c(1L, 3L)])
  return(fn)
}



# @note
# \link[rpart]{rpart} is quite slow



#' @title Batch Operation of [rpartD]
#' 
#' @param y,... see function [rpartD]
#' 
#' @param X \link[base]{data.frame} or \link[base]{list}
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @returns 
#' Function [rpartD_] returns a \link[base]{list} of [rpartD] returns.
#' 
#' @importFrom parallel mclapply detectCores
#' @export
rpartD_ <- function(y, X, mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), ...) {
  mclapply(X, mc.cores = mc.cores, FUN = function(x) rpartD(y = y, x = x, ...))
}



