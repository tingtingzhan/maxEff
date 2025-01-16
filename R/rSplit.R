
#' @title Stratified Random Split Sampling
#' 
#' @description 
#' Random split sampling, with optional stratification based on the type of the response.
#' 
#' @param n positive \link[base]{integer} scalar, number of \link[base]{replicate}s of random splits to be performed
#' 
#' @param x a \link[base]{double}, 
#' \link[base]{logical} or \link[base]{factor} \link[base]{vector},
#' or a \link[survival]{Surv} object
#' 
#' @param stratify \link[base]{logical} scalar, 
#' whether stratification based on `x` needs to be implemented, default `TRUE`
#' 
#' @param s_prob \link[base]{double} scalar between 0 and 1, 
#' split ratio, i.e., percentage of training subjects \eqn{p}, default `.8`
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' 
#' Function [rSplit] performs random split sampling, 
#' with or without stratification. Specifically,
#' 
#' \itemize{
#' 
#' \item If `stratify = FALSE`, 
#' or if we have a \link[base]{double} `x`,
#' then split the sample into a training and a test set by odds \eqn{p/(1-p)}, without stratification.
#' 
#' \item Otherwise, split a \link[survival]{Surv} `x`, stratified by its censoring status.
#' Specifically, 
#' split subjects with observed event into a training and a test set by odds \eqn{p/(1-p)},
#' and split the censored subjects into a training and a test set by odds \eqn{p/(1-p)}.
#' Then combine the training sets from subjects with observed events and censored subjects,
#' and combine the test sets from subjects with observed events and censored subjects.
#' 
#' \item Otherwise, split a \link[base]{logical} `x`, stratified by itself.
#' Specifically, 
#' split the subjects with `TRUE` response into a training and a test set by odds \eqn{p/(1-p)},
#' and split the subjects with `FALSE` response into a training and a test set by odds \eqn{p/(1-p)}.
#' Then combine the training sets, and the test sets, in a similar fashion as described above.
#' 
#' \item Otherwise, split a \link[base]{factor} `x`, stratified by its \link[base]{levels}.
#' Specifically, 
#' split the subjects in each level of \eqn{y} into a training and a test set by odds \eqn{p/(1-p)}.
#' Then combine the training sets, and the test sets, from all levels of \eqn{y}.
#' 
#' }
#' 
#' 
#' @returns 
#' Function [rSplit] returns a length-`n` \link[base]{list} of 
#' \link[base]{logical} \link[base]{vector}s.
#' In each \link[base]{logical} \link[base]{vector}, 
#' the `TRUE` elements indicate training subjects and 
#' the `FALSE` elements indicate test subjects.
#' 
#' @note
#' `caTools::sample.split` is not what we need.
#' 
#' @examples
#' rep(c(TRUE, FALSE), times = c(20, 30)) |>
#'  rSplit(n = 3L)
#' 
#' @seealso \link[base]{split}, `caret::createDataPartition`
#' @keywords internal
#' @export 
rSplit <- function(n, x, stratify = TRUE, s_prob = .8, ...) {
  
  if (anyNA(x)) stop('do not allow missingness in the response, for now')
  
  nx <- length(x) 
  ret0 <- rep(FALSE, times = nx)
  # works correctly for ?survival::Surv object, via ?survival:::length.Surv
  
  if (!stratify) {
    # no stratification
    idx <- list(seq_len(nx))
    
  } else if (inherits(x, what = 'Surv')) {
    # stratify by censoring status
    if (dim(x)[2L] == 3L) stop('3-col Surv response not supported yet')
    xevent <- as.logical(x[,2L])
    idx <- list(
      which(!xevent), # 'integer' indices of censored events
      which(xevent) # 'integer' indices of observed events
    )

  } else if (is.logical(x) || all(x %in% c(0, 1))) {
    # stratify by the binary response
    x <- as.logical(x)
    idx <- list(
      which(!x), # 'integer' indices of non-responder
      which(x) # 'integer' indices of responder
    )

  } else if (is.factor(x)) {
    # stratify by the levels
    idx <- lapply(seq_along(attr(x, which = 'levels', exact = TRUE)), FUN = function(i) {
      which(unclass(x) == i)
    })
    
  } else if (is.vector(x, mode = 'numeric')) { # must after `binary` if
    # no stratification
    idx <- list(seq_len(nx))
    
  } else stop('unsupported class: ', class(x)[1L])
  
  replicate(n = n, expr = {
    idx_train <- lapply(idx, FUN = function(id) {
      sample(id, size = floor(length(id) * s_prob), replace = FALSE)
    })
    train <- unlist(idx_train, use.names = FALSE)
    ret <- ret0
    ret[train] <- TRUE
    ret
  }, simplify = FALSE)
  
}

