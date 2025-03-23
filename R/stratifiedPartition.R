
#' @title Stratified Partition
#' 
#' @description 
#' A partition, stratified based on the type of the response.
#' 
#' @param y response \eqn{y}, a \link[base]{double}, 
#' \link[base]{logical} or \link[base]{factor} \link[base]{vector},
#' or a \link[survival]{Surv} object
#' 
#' @param times positive \link[base]{integer} scalar \eqn{n}, number of \link[base]{replicate}s of partitions
#' 
#' @param p \link[base]{double} scalar between 0 and 1, 
#' percentage \eqn{p} of training subjects, default `.8`
# following nomenclature of \link[caret]{createDataPartition}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' 
#' Function [stratifiedPartition()] performs stratified partition. 
#' Specifically,
#' 
#' \itemize{
#' 
#' \item If \eqn{y} is a \link[base]{double} \link[base]{vector},
#' then partition \eqn{y} into a training and a test set by odds \eqn{p/(1-p)}, without stratification.
#' 
#' \item Otherwise, if \eqn{y} is a \link[survival]{Surv} object, then partition \eqn{y} and stratified by its censoring status.
#' Specifically, in each of two strata, observed and censored subjects,
#' partition the subjects into a training and a test set by odds \eqn{p/(1-p)}.
#' Then combine the training sets, and the test sets, from both strata.
#' 
#' \item Otherwise, if \eqn{y} is a \link[base]{logical} \link[base]{vector}, then partition \eqn{y} and stratified by itself.
#' Specifically, in each of two strata, subjects with `TRUE` and `FALSE` responses,
#' partition the subjects into a training and a test set by odds \eqn{p/(1-p)}.
#' Then combine the training sets, and the test sets, from both strata.
#' 
#' \item Otherwise, if \eqn{y} is a \link[base]{factor} \link[base]{vector}, then partition \eqn{y} andstratified by its \link[base]{levels}.
#' Specifically, in each level (i.e., strata), 
#' partition the subjects into a training and a test set by odds \eqn{p/(1-p)}.
#' Then combine the training sets, and the test sets, from all levels of \eqn{y}.
#' 
#' }
#' 
#' 
#' @returns 
#' Function [stratifiedPartition()] returns a length-\eqn{n} \link[base]{list} of 
#' \link[base]{logical} \link[base]{vector}s.
#' In each \link[base]{logical} \link[base]{vector}, 
#' the `TRUE` elements indicate training subjects and 
#' the `FALSE` elements indicate test subjects.
#' 
#' @note
#' Nomenclature of the parameters of function [stratifiedPartition()] follows those of function \link[caret]{createDataPartition}.
#' 
#' Function `caTools::sample.split` is not what we need.
#' 
#' @examples
#' set.seed(125); rep(c(TRUE, FALSE), times = c(10, 15)) |>
#'  stratifiedPartition(times = 3L)
#' 
#' @keywords internal
#' @export 
stratifiedPartition <- function(y, times, p = .8, ...) {
  
  if (anyNA(y)) stop('do not allow missingness in the response, for now')
  
  nx <- length(y) 
  # works correctly for ?survival::Surv object, via ?survival:::length.Surv
  
  if (inherits(y, what = 'Surv')) {
    # stratify by censoring status
    if (dim(y)[2L] == 3L) stop('3-col Surv response not supported yet')
    xevent <- as.logical(y[,2L])
    idx <- list(
      which(!xevent), # 'integer' indices of censored events
      which(xevent) # 'integer' indices of observed events
    )

  } else if (is.logical(y) || all(y %in% c(0, 1))) {
    # stratify by the binary response
    y <- as.logical(y)
    idx <- list(
      which(!y), # 'integer' indices of non-responder
      which(y) # 'integer' indices of responder
    )

  } else if (is.factor(y)) {
    # stratify by the levels
    idx <- lapply(seq_along(attr(y, which = 'levels', exact = TRUE)), FUN = function(i) {
      which(unclass(y) == i)
    })
    
  } else if (is.vector(y, mode = 'numeric')) { # must after `binary` if
    # no stratification
    idx <- list(seq_len(nx))
    
  } else stop('unsupported class: ', class(y)[1L])
  
  ret0 <- rep(FALSE, times = nx)
  ret <- replicate(n = times, expr = {
    train <- idx |> lapply(FUN = function(id) {
      sample(id, size = floor(length(id) * p), replace = FALSE)
    }) |>
      unlist(use.names = FALSE)
    tmp <- ret0
    tmp[train] <- TRUE
    tmp
  }, simplify = FALSE)
  class(ret) <- 'listof'
  return(ret)
  
}





# \item If `stratify = FALSE`, 
# or if we have a \link[base]{double} `y`,
# then split the sample into a training and a test set by odds \eqn{p/(1-p)}, without stratification.

rSplit_no_stratify <- function(times, y, stratify = FALSE, p = .8, ...) {
  
  if (anyNA(y)) stop('do not allow missingness in the response, for now')
  
  nx <- length(y) 
  ret0 <- rep(FALSE, times = nx)
  # works correctly for ?survival::Surv object, via ?survival:::length.Surv
  
  if (!stratify) {
    # no stratification
    idx <- list(seq_len(nx))
    
  } else if (inherits(y, what = 'Surv')) {
    # stratify by censoring status
    if (dim(y)[2L] == 3L) stop('3-col Surv response not supported yet')
    xevent <- as.logical(y[,2L])
    idx <- list(
      which(!xevent), # 'integer' indices of censored events
      which(xevent) # 'integer' indices of observed events
    )
    
  } else if (is.logical(y) || all(y %in% c(0, 1))) {
    # stratify by the binary response
    y <- as.logical(y)
    idx <- list(
      which(!y), # 'integer' indices of non-responder
      which(y) # 'integer' indices of responder
    )
    
  } else if (is.factor(y)) {
    # stratify by the levels
    idx <- lapply(seq_along(attr(y, which = 'levels', exact = TRUE)), FUN = function(i) {
      which(unclass(y) == i)
    })
    
  } else if (is.vector(y, mode = 'numeric')) { # must after `binary` if
    # no stratification
    idx <- list(seq_len(nx))
    
  } else stop('unsupported class: ', class(y)[1L])
  
  replicate(n = times, expr = {
    idx_train <- lapply(idx, FUN = function(id) {
      sample(id, size = floor(length(id) * p), replace = FALSE)
    })
    train <- unlist(idx_train, use.names = FALSE)
    ret <- ret0
    ret[train] <- TRUE
    ret
  }, simplify = FALSE)
  
}
