#' Add function to a plot
#'
#' @details
#' When using `type_function()` in a `tinyplot()` call, the `x` value indicates
#' the range of values to plot on the x-axis.
#'
#' @param fun Function of `x` to plot.
#' @param args Additional arguments to be passed to `fun`
#' @param n Number of points to interpolate on the x axis.
#' @param ... Additional arguments are passed to the `lines()` function,
#' ex: `type="p"`, `col="pink"`.
#' @importFrom stats dnorm
#' @examples
#' # Plot the normal density
#' tinyplot(x = -4:4, type = type_function(dnorm))
#'
#' # Extra arguments for the function to plot
#' tinyplot(x = -1:10, type = type_function(
#'   fun = dnorm, args = list(mean = 3)
#' ))
#'
#' # Additional arguments are passed to the `lines()` function.
#' tinyplot(x = -4:4, type = type_function(
#'   fun = dnorm,
#'   col = "pink", type = "p", pch = 3
#' ))
#'
#' @export
type_function = function(fun = dnorm, args = list(), n = 101, ...) {
  assert_function(fun)
  lines_args = list(...)
  data_function = function(args, fun) {
    funky = function(xlim, ylim, datapoints, ...) {
      if (nrow(datapoints) == 0 || !"x" %in% names(datapoints)) {
        stop("Need to provide x values to plot the function.", call. = FALSE)
      }
      if (is.null(xlim)) {
        xlim = range(datapoints[["x"]])
      }
      if (is.null(ylim)) {
        tmp = c(list(datapoints[["x"]]), args)
        tmp = range(tmp)
        tmp = seq(tmp[1], tmp[2], length.out = n)
        tmp = c(list(tmp), args)
        tmp = do.call(fun, tmp)
        ylim = c(min(tmp), max(tmp))
      }
      out = list(xlim = xlim, ylim = ylim)
      return(out)
    }
  }
  draw_function = function() {
    funky = function(ifacet, data_facet, ...) {
      xrange = range(data_facet[[ifacet]][["x"]])
      x = seq(xrange[1], xrange[2], length.out = n)
      y = do.call(fun, c(list(x), args))
      tmp = c(list(x, y), lines_args)
      do.call(lines, tmp)
    }
    return(funky)
  }
  out = list(
    draw = draw_function(),
    data = data_function(args = args, fun = fun),
    name = "function"
  )
  class(out) = "tinyplot_type"
  return(out)
}
