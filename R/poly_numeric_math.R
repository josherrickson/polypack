.poly_numeric_prod <- function(op, p, n) {
  p@.Data <- op(p@.Data, n)
  return(p)
}

##' @title \code{poly} and numeric arithmetic
##'
##' poly/numeric is not supported. Other operations are supported. Exactly one
##' of \code{e1} or \code{e2} should be \code{poly}.
##' @param e1 \code{poly} or \code{numeric}.
##' @param e2 \code{poly} or \code{numeric}.
##' @return \code{poly}
##' @export
##' @rdname poly_numeric_math
setMethod("*", signature(e1 = "poly", e2 = "numeric"),
          function(e1, e2) .poly_numeric_prod(`*`, e1, e2))

##' @export
##' @rdname poly_numeric_math
setMethod("*", signature(e1 = "numeric", e2 = "poly"),
          function(e1, e2) .poly_numeric_prod(`*`, e2, e1))

##' @export
##' @rdname poly_numeric_math
setMethod("/", signature(e1 = "poly", e2 = "numeric"),
          function(e1, e2) .poly_numeric_prod(`/`, e1, e2))

##' @export
##' @rdname poly_numeric_math
setMethod("/", signature(e1 = "numeric", e2 = "poly"),
{
  function(e1, e2) {
    stop("Division by a `poly` not supported")
  }
})

.poly_numeric_add <- function(p, n) {
  newlist <- as.list(p@.Data)
  names(newlist) <- p@powers
  if (!is.null(newlist$`0`)) {
    newlist$`0` <- newlist$`0` + n
  } else {
    newlist$`0` <- n
  }
  return(make_poly(newlist))
}

##' @export
##' @rdname poly_numeric_math
setMethod("+", signature(e1 = "poly", e2 = "numeric"),
          function(e1, e2) .poly_numeric_add(e1, e2))

##' @export
##' @rdname poly_numeric_math
setMethod("+", signature(e1 = "numeric", e2 = "poly"),
          function(e1, e2) .poly_numeric_add(e2, e1))

##' @export
##' @rdname poly_numeric_math
setMethod("-", signature(e1 = "poly", e2 = "numeric"),
          function(e1, e2) .poly_numeric_add(e1, -e2))

##' @export
##' @rdname poly_numeric_math
setMethod("-", signature(e1 = "numeric", e2 = "poly"),
{
  function(e1, e2) {
    e2@.Data <- -e2@.Data
    .poly_numeric_add(e2, e1)
  }
})
