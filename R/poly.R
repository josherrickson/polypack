##' A class represnting a polynomial equation
##'
##' @slot The powers crresoponding to each coefficient
setClass("poly",
         contains = "numeric",
         slots = c(powers = "numeric"))

setValidity("poly", function(object) {
  if (length(object) != length(object@powers)) {
    stop("Powers and coefficients of differing lengths")
  }
  if (any(is.na(object@powers))) {
    stop("Powers must not be missing")
  }
  if (any(is.na(object))) {
    stop("Coefficients must be numeric")
  }
  if (any(object@.Data == 0)) {
    stop("0 coefficients should not be stored")
  }
  if (any(diff(object@powers) > 0)) {
    stop("powers must be sorted in descending order")
  }
  TRUE
})

##' @title Create a \code{poly} object
##' @param ... Named arguments where the name is the power, and the value is its
##'   corresponding coefficient. E.g. \code{"2" = 5} implies $5x^2$. If multiple
##'   arguments have the same power (e.g. \code{make_poly("2" = 1, "2" = 5)}),
##'   the coefficients will be summed (continuning example, $x^2 + 5x^2 =
##'   6x^2$).
##' @return A new \code{poly} object
##' @export
##' @importFrom methods new
make_poly <- function(...) {
  coeffs <- list(...)
  if (length(coeffs) == 1 & is.null(names(coeffs))) {
    # This probably means the user called `make_poly(list(...))`, so try and
    # extract the elements of the list
    coeffs <- coeffs[[1]]
    if (!is.list(coeffs)) {
      stop("Invalid input")
    }
  }

  # If multiple arguments of the same name, sum them
  coeffs <-lapply(split(unlist(coeffs), names(coeffs)), sum)

  # Ensure input is ordered
  coeffs <- coeffs[sort(names(coeffs), decreasing = TRUE)]

  powers <- as.numeric(names(coeffs))
  values <- Reduce(c, coeffs)

  # Don't bother storing any 0's
  zeros <- which(values != 0)
  values <- values[zeros]
  powers <- powers[zeros]

  return(methods::new("poly", values, powers = powers))
}

##' @title Show a \code{poly} object
##' @param object A \code{poly} object
##' @return \code{object}, invisibly.
##' @export
##' @importFrom methods show
setMethod("show", "poly",
  function(object) {

    if (length(object@.Data) == 0) {
      # Short circuit on an all-zero coefficient poly
      cat(0, "\n")
    }
    # Generate each term by combining coefficient and power to
    # create terms of the form `cx^p`.
    terms <- paste0(object@.Data, "x^", object@powers)

    ### Special cases - to make it look nicer:
    # 1. x^0 is 1
    terms <- gsub("x\\^0$", "", terms)

    # 2. x^1 is x
    terms <- gsub("x\\^1$", "x", terms)

    # 3. Any coefficients of "1" can be dropped, *except* for the constant:
    terms <- gsub("^1x", "x", terms)
    terms <- gsub("^-1x", "-x", terms)

    ### Add signs
    # 1. Move `-` over to make spacing nicer - but NOT for the first entry
    terms[-1] <- gsub("^-", "- ", terms[-1])

    # 2. Add +'s if not having -'s, again excluding
    pluses <- !grepl("^-", terms)
    pluses[1] <- FALSE
    terms[pluses] <- paste("+", terms[pluses])

    cat(paste(terms, collapse = " "), "\n")
    return(invisible(object))
  }
)
