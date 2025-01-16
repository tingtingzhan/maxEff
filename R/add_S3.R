



#' @export
`[.add_` <- function(x, i) {
  ret <- unclass(x)[i]
  class(ret) <- class(x) # otherwise class info dropped
  return(ret)
}






#' @title print.add_
#' 
#' @param x an [add_dummy] or [add_num] object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' ..
#' 
#' @returns
#' Function [print.add_] does not have a returned value
#' 
#' @keywords internal
#' @export print.add_
#' @export
print.add_ <- function(x, ...) {
  cat(names(x), sep = '\n')
  return(invisible())
}



#head.add_ <- function(x, n, ...) {
#  .Defunct(new = 'utils:::head.default') # since I have `[.add_`
#  x[seq_len(n)] # not doing any fancy checks
#}



#' @title [sort_by.add_]
#' 
#' @description
#' ..
#' 
#' @param x an [add_dummy] or [add_num] object
#' 
#' @param y \link[base]{language}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details
#' Sort the elements of an [add_dummy] or [add_num] object by a certain criterion `y`.
#' 
#' We suggest using `y = abc(cf)`, indicating a `decreasing` order of the \link[base]{abs}olute values of the regression coefficient estimate of the median-split-dichotomized regression models.  
#'
#' @returns 
#' Function [sort_by.add_] returns an [add_dummy] or [add_num] object.
#' 
#' @keywords internal
#' @export sort_by.add_
#' @export
sort_by.add_ <- function(x, y, ...) {
  cf <- vapply(x, FUN = attr, which = 'cf', exact = TRUE, FUN.VALUE = NA_real_)
  o <- order(eval(substitute(y)), ...) # ?base::order
  x[o]
}



