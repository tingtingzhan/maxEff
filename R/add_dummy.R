


# ## Dichotomizing Predictor(s) via Dichotomizing Split Sample [splitDichotom]
#
# Pipeline `splitDichotom |> subset |> sort_by |> head` identifies 
# the optimal dichotomizing predictors using repeated sample splits on the *training set*.
# 
# Function [predict.splitDichotom] .. testing set
# 




#' @title Additional Predictor as \link[base]{logical}
#' 
#' @description
#' Additional predictor as \link[base]{logical}.
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
#' @param times,... additional parameters of function [statusPartition()] for function [add_dummy_partition].
#' For function [add_dummy()], these parameters are not in use
#' 
#' @details
#' Function [add_dummy_partition()] partitions each additional \link[base]{numeric} predictor 
#' into a \link[base]{logical} variable in the following steps.
#' \enumerate{
#' \item {Generate multiple, i.e., repeated, partitions via functions \link[caret]{createDataPartition} or [statusPartition()].}
#' \item {For each partition, create a dichotomizing rule (via function `node1()`) on the training set. 
#' Apply this dichotomizing rule on the test set and obtain the estimated regression coefficient (i.e., effect size) 
#' of the additional \link[base]{logical} predictor.}
#' \item {Among all partitions, select the one with median effect size of the additional \link[base]{logical} predictor.}
#' }
#' 
#' 
#' @returns 
#' Function [add_dummy_partition()] returns an object of \link[base]{class} `'add_dummy'`, which is a \link[stats]{listof} [node1] objects.
#' 
#' @keywords internal
#' @name add_dummy
#' @importFrom caret createDataPartition
#' @importFrom parallel mclapply
#' @importFrom stats formula quantile
#' @export
add_dummy_partition <- function(
    start.model, 
    x,
    data = eval(start.model$call$data),
    times, 
    mc.cores = getOption('mc.cores'), 
    ...
) {
  
  tmp <- .prepare_add_(start.model = start.model, x = x, data = data)
  y <- tmp$y
  #data <- tmp$data # not here!
  x_ <- tmp$x_
  xval <- tmp$xval

  ids <- if (inherits(y, what = 'Surv')) {
    statusPartition(y = y, times = times, ...)
  } else createDataPartition(y = y, times = times, groups = 2L, ...)
  # using same split for all predictors
  
  out <- x_ |>
    seq_along() |>
    mclapply(mc.cores = mc.cores, FUN = \(i) { 
    #lapply(FUN = \(i) { # debugging
      tmp_ <- ids |>
        lapply(FUN = splitd, start.model = start.model, x_ = x_[[i]], x = xval[[i]], data = data)
      tmp <- tmp_[lengths(tmp_, use.names = FALSE) > 0L]
      
      effsize <- tmp |> 
        vapply(FUN = attr, which = 'effsize', exact = TRUE, FUN.VALUE = NA_real_)
      id <- tmp |> 
        seq_along() |> 
        quantile(probs = .5, type = 3L, na.rm = TRUE) # median *location*
      return(tmp[[order(effsize)[id]]])  
    })

  class(out) <- c('add_dummy', 'add_', class(out))
  return(invisible(out))
  
}






#' @rdname add_dummy
#' 
#' @details 
#' Function [add_dummy()] partitions each additional 
#' \link[base]{numeric} predictor into a \link[base]{logical} variable 
#' using function [node1()], 
#' then \link[stats]{update}s the starting model by adding in each of the dichotomized 
#' \link[base]{logical} predictor. 
#' 
#' @returns
#' Function [add_dummy()] returns an object of class `'add_dummy'`, 
#' which is a \link[stats]{listof} [node1] objects.
#' @importFrom rpart rpart
#' @importFrom stats update
#' @export
add_dummy <- function(
    start.model, 
    x, 
    data = eval(start.model$call$data),
    mc.cores = getOption('mc.cores'), 
    ...
) {
  
  tmp <- .prepare_add_(start.model = start.model, x = x, data = data)
  y <- tmp$y
  data_ <- tmp$data
  x_ <- tmp$x_
  xval <- tmp$xval
  
  out <- x_ |>
    seq_along() |>  
    mclapply(mc.cores = mc.cores, FUN = \(i) {
      x. <- x_[[i]]
      xval <- xval[[i]]
      
      rule <- rpart(formula = y ~ xval, cp = .Machine$double.eps, maxdepth = 2L) |> # partition rule based on complete data
        node1()
      formals(rule)$newx <- x.

      data_$x. <- rule(xval) # partition rule applied to complete data
      suppressWarnings(m_ <- update(start.model, formula. = . ~ . + x., data = data_))
      cf <- m_$coefficients
      cf_ <- cf[length(cf)]
      attr(rule, which = 'p1') <- mean.default(data_$x., na.rm = TRUE)
      attr(rule, which = 'effsize') <- if (is.finite(cf_)) unname(cf_) else NA_real_
      attr(rule, which = 'model') <- m_ # only model formula needed for [update.node1]!!!
      return(rule)
    })
  
  class(out) <- c('add_dummy', 'add_', 'listof')
  return(out)

}





#' @title [print.add_dummy]
#' 
#' @param x an object returned from functions 
#' [add_dummy_partition()] or [add_dummy()]
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' ..
#' 
#' @returns
#' Function [print.add_dummy()] does not have a returned value
#' 
#' @keywords internal
#' @export print.add_dummy
#' @export
print.add_dummy <- function(x, ...) {
  x |>
    vapply(FUN = labels.node1, FUN.VALUE = '') |>
    cat(sep = '\n')
}













#' @title S3 Method Dispatches to `'add_dummy'` Class
#' 
#' @param x,object an object returned from functions [add_dummy_partition()] or [add_dummy()]
#' 
#' @param subset \link[base]{language}
#' 
#' @param ... additional parameters of function [update.node1()], e.g., `newdata`
#' 
#' @details
#' Function [subset.add_dummy()], default subset `(p1>.15 & p1<.85)`.
#' See explanation of \eqn{p_1} in function [splitd()].
#' 
#' @returns
#' Function [subset.add_dummy()] returns a [add_dummy()] object.
#' 
#' @keywords internal
#' @name S3_add_dummy
#' @export subset.add_dummy
#' @export
subset.add_dummy <- function(x, subset, ...) {
  subset <- substitute(subset)
  v_sub <- all.vars(subset)
  if (!all(v_sub %in% c('p1'))) stop('criterion must be set on `p1`, for now')
  p1 <- vapply(x, FUN = attr, which = 'p1', exact = TRUE, FUN.VALUE = NA_real_)
  x[eval(subset)]
}



#' @rdname S3_add_dummy
#' 
#' @returns
#' Function [predict.add_dummy()] returns a \link[stats]{listof} regression models.
#' 
#' @importFrom stats predict
#' @export predict.add_dummy
#' @export
predict.add_dummy <- function(object, ...) {
  # think about changing this to [update.add_dummy()] too 
  ret <- object |> 
    lapply(FUN = update.node1, ...)
  class(ret) <- 'listof'
  return(ret)
}


