#' Add straight lines to a plot
#'
#' @inheritParams graphics::abline
#' @examples
#' mod = lm(mpg ~ hp, data = mtcars)
#' y = mtcars$mpg
#' yhat = predict(mod)
#' tinyplot(y, yhat, xlim = c(0, 40), ylim = c(0, 40))
#' tinyplot_add(type = type_abline(a = 0, b = 1))
#' @export
type_abline = function(a = 0, b = 1) {
  data_abline = function(datapoints, ...) {
    if (nrow(datapoints) == 0) {
      msg = "`type_abline() only works on existing plots with x and y data points."
      stop(msg, call. = FALSE)
    }
    return(list())
  }
  draw_abline = function() {
    fun = function(ifacet, data_facet, icol, ilty, ilwd, ...) {
      nfacets = length(data_facet)

      if (length(a) == 1) {
        a = rep(a, nfacets)
      } else if (length(a) != nfacets) {
        msg = "Length of 'a' must be 1 or equal to the number of facets"
        stop(msg, call. = FALSE)
      }

      if (length(b) == 1) {
        b = rep(b, nfacets)
      } else if (length(b) != nfacets) {
        msg = "Length of 'b' must be 1 or equal to the number of facets"
        stop(msg, call. = FALSE)
      }

      abline(a = a[ifacet], b = b[ifacet], col = icol, lty = ilty, lwd = ilwd)
    }
    return(fun)
  }
  out = list(
    draw = draw_abline(),
    data = data_abline,
    name = "abline"
  )
  class(out) = "tinyplot_type"
  return(out)
}
