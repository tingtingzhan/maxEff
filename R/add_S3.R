



#' @export
`[.add_` <- function(x, i) {
  ret <- unclass(x)[i]
  class(ret) <- class(x) # otherwise class info dropped
  return(ret)
}






#' @title S3 Method Dispatches to `'add_'` Class
#' 
#' @param x an object returned from functions 
#' [add_dummy_partition()], [add_dummy()] or [add_num()]
#' 
#' @param y \link[base]{language}, see function \link[base]{sort_by}
#' 
#' @param ... additional parameters of S3 generic \link[base]{sort_by}, etc.
#' 
#' @details
#' ..
#' 
#' @returns
#' Function [print.add_] does not have a returned value
#' 
#' @name S3_add_
#' @keywords internal
#' @export print.add_
#' @export
print.add_ <- function(x, ...) {
  cat(names(x), sep = '\n')
  return(invisible())
}



#' @rdname S3_add_
#' 
#' @details
#' Function [sort_by.add_()] sorts the elements of an `'add_'` object by a certain criterion `y`.
#' We suggest using `y = abc(effsize)` and `decreasing = TRUE` order of the \link[base]{abs}olute values of the effect sizes of additional predictor.  
#'
#' @returns 
#' Function [sort_by.add_()] returns an object of the same \link[base]{class} as input `x`.
#' 
#' @keywords internal
#' @export sort_by.add_
#' @export
sort_by.add_ <- function(x, y, ...) {
  effsize <- vapply(x, FUN = attr, which = 'effsize', exact = TRUE, FUN.VALUE = NA_real_)
  o <- substitute(y) |> eval() |> order(...) # ?base::order
  x[o]
}



