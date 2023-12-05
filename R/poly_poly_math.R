##' @title \code{poly}-\code{poly} math
##' @details Supports `+`, `-`, and `*`. Division not supported.
##' @param e1 \code{poly} object
##' @param e2 \code{poly} object
##' @return \code{poly} object
##' @export
##' @rdname poly_poly_math
##' @importFrom methods new
setMethod("+", signature(e1 = "poly",
                         e2 = "poly"),
{
  function(e1, e2) {
    # Get list of all powers that exist in either object
    newpowers = unique(c(e1@powers, e2@powers))

    val <- c()
    pow <- c()

    for (p in newpowers) {
      # For each object, obtain the coefficient value,
      # or 0 if the power isn't found
      e1_p <- e1@powers == p
      e1v <- ifelse(any(e1_p), e1@.Data[e1_p], 0)
      e2_p <- e2@powers == p
      e2v <- ifelse(any(e2_p), e2@.Data[e2_p], 0)
      val <- c(e1v + e2v, val)
      pow <- c(p, pow)
    }

    # Strip any 0 coefficient terms
    zeros <- which(val != 0)
    val <- val[zeros]
    pow <- pow[zeros]

    # Re-order
    order <- order(pow, decreasing = TRUE)
    val <- val[order]
    pow <- pow[order]

    return(methods::new("poly", val, powers = pow))
  }
})

##' @export
##' @rdname poly_poly_math
setMethod("-", signature(e1 = "poly",
                         e2 = "poly"),
{
  function(e1, e2) {
    # (a + b) = (a + (-b))
    e2@.Data <- -1*e2@.Data
    return(e1 + e2)
  }
})

##' @export
##' @rdname poly_poly_math
setMethod("*", signature(e1 = "poly",
                         e2 = "poly"),
{
  function(e1, e2) {
    powers <- expand.grid(e1@powers, e2@powers)
    powers <- apply(powers, 1, sum)
    coeffs <- expand.grid(e1@.Data, e2@.Data)
    coeffs <- apply(coeffs, 1, prod)
    inputlist <- as.list(coeffs)
    names(inputlist) <- powers
    return(make_poly(inputlist))
  }
})

##' @export
##' @rdname poly_poly_math
setMethod("/", signature(e1 = "poly",
                         e2 = "poly"),
{
  function(e1, e2) {
    stop("Division of `poly` objects not supported")
  }
})
