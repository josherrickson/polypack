##' @title Evaluate a \code{poly} onbject
##' @param poly \code{poly} object
##' @param x A vector of values
##' @return A vector of same length of \code{x}, of \code{poly} evaluated at
##'   values of \code{x}.
##' @export
eval_poly <- function(poly, x) {
  vapply(x, function(xx) {
    sum(poly@.Data*xx^poly@powers)
  }, 1)
}
