##' @title Plot a \code{poly} object
##' @param x,poly a \code{poly} object
##' @param y alias for \code{from} for compatibility with \code{plot}
##' @param to right side of range to plot
##' @param from left side of range to plot
##' @return The plot
##' @export
##' @rdname poly-plotting
plot.poly <- function(x,
                      y = 0,
                      to = 1,
                      from = y,
                      ...) {
  plot(function(z) eval_poly(x, z),
       y = y,
       from = from,
       to = to,
       ...)
}

##' @export
##' @rdname poly-plotting
ggplot_poly <- function(poly, from = 0, to = 1, ...) {
  data <- data.frame(x = seq(from = from,
                             to = to,
                             length.out = 100))
  data$y <- eval_poly(poly, data$x)
  ggplot(data = data, aes(x = x, y = y)) +
    geom_line(...)
}
